module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html, div, i, input, label, text)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (onInput)
import Html.Events.Extra.Pointer as Pointer
import List.Extra
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Platform.Cmd as Cmd
import Task
import WebGL exposing (Mesh, Shader)



--- Constants ---


{-| HTML element ID of the WebGL canvas
-}
canvasId : String
canvasId =
    "webgl-canvas"


{-| We only support up to this many waves sources on the canvas
-}
maxNumWavesources : Int
maxNumWavesources =
    10


{-| How close the pointer must be to a wavesource to select it
-}
selectionDistancePx : Float
selectionDistancePx =
    150



--------


main : Program () Model UpdateMsg
main =
    Browser.element
        { init = \_ -> ( defaultModel, getCanvasSize )
        , view = \model -> view model.values
        , subscriptions =
            \_ ->
                Sub.batch
                    [ onAnimationFrameDelta TimeUpdate
                    , Browser.Events.onResize WindowResized
                    ]
        , update =
            \update model ->
                case update of
                    TimeUpdate elapsedTime ->
                        if model.drifting then
                            ( advanceAnimation model elapsedTime, Cmd.none )

                        else
                            ( model, Cmd.none )

                    SliderUpdate newValues ->
                        ( { model
                            | values = newValues
                            , drifting = False
                          }
                        , Cmd.none
                        )

                    PointerUpdate event ->
                        ( pointerUpdate model event, Cmd.none )

                    WindowResized _ _ ->
                        ( model, getCanvasSize )

                    CanvasSizeUpdate result ->
                        case result of
                            Err _ ->
                                ( model, Cmd.none )

                            Ok el ->
                                ( updateCanvasSize model el
                                , Cmd.none
                                )
        }


type UpdateMsg
    = TimeUpdate Float
    | SliderUpdate Values
    | PointerUpdate PointerAction
    | CanvasSizeUpdate (Result Browser.Dom.Error Browser.Dom.Element)
    | WindowResized Int Int


type PointerAction
    = PointerDown Pointer.Event
    | PointerUp Pointer.Event
    | PointerMove Pointer.Event


type PointerState
    = Inactive
    | Selected Int -- Index of selected wavesource
    | Dragging Int -- Index of dragged wavesource


getCanvasSize : Cmd UpdateMsg
getCanvasSize =
    Task.attempt CanvasSizeUpdate (Browser.Dom.getElement canvasId)


updateCanvasSize : Model -> Browser.Dom.Element -> Model
updateCanvasSize model element =
    let
        oldValues =
            model.values

        newSize =
            vec2 element.element.width element.element.height
    in
    { model
        | values =
            { oldValues
                | canvasSize = newSize
                , wavesources =
                    List.map
                        (resizeVec oldValues.canvasSize newSize)
                        oldValues.wavesources
            }
    }


type alias Model =
    { values : Values
    , pointerState : PointerState
    , animationState : AnimationState
    , drifting : Bool
    }


defaultModel : Model
defaultModel =
    { values = defaultValues
    , pointerState = Inactive
    , animationState = initialAnimationState
    , drifting = True
    }


type alias Values =
    { canvasSize : Vec2
    , resolutionMultiplier : Int
    , wavesources : List Vec2
    , wavelength : Float
    , falloff : Float
    , threshold : Float
    }


defaultValues : Values
defaultValues =
    { canvasSize = vec2 500 666
    , resolutionMultiplier = 2
    , wavesources =
        [ vec2 165 500
        , vec2 335 167
        ]
    , wavelength = 150
    , falloff = 30
    , threshold = -0.07
    }


pointerUpdate : Model -> PointerAction -> Model
pointerUpdate model action =
    case model.pointerState of
        Inactive ->
            case action of
                PointerDown event ->
                    selectWavesource
                        model
                        (vec2FromTuple event.pointer.offsetPos)

                PointerUp _ ->
                    model

                PointerMove _ ->
                    model

        Selected index ->
            case action of
                PointerUp _ ->
                    { model
                        | pointerState = Inactive
                        , values = deleteWavesource model.values index
                    }

                PointerDown _ ->
                    model

                PointerMove event ->
                    { model
                        | pointerState = Dragging index
                        , values =
                            moveWavesource
                                model.values
                                index
                                (vec2FromTuple event.pointer.offsetPos)
                    }

        Dragging index ->
            case action of
                PointerUp _ ->
                    { model | pointerState = Inactive }

                PointerDown _ ->
                    model

                PointerMove event ->
                    { model
                        | values =
                            moveWavesource
                                model.values
                                index
                                (vec2FromTuple event.pointer.offsetPos)
                    }



--- WAVESOURCE MANIPULATION ---


addWavesource : Values -> Vec2 -> Values
addWavesource old coords =
    { old
        | wavesources =
            old.wavesources
                ++ [ coords ]
    }


moveWavesource : Values -> Int -> Vec2 -> Values
moveWavesource old index coords =
    let
        newWavesources =
            List.Extra.setAt index coords old.wavesources
    in
    { old | wavesources = newWavesources }


{-| Get wavesource by index, the final float in the Vec3 indicates whether the
wavesource is active or not
-}
getWavesource : List Vec2 -> Int -> Vec3
getWavesource sourceList index =
    sourceList
        |> List.Extra.getAt index
        |> Maybe.map
            (\v ->
                vec3
                    (Vec2.getX v)
                    (Vec2.getY v)
                    1.0
            )
        |> Maybe.withDefault
            (vec3 0 0 0)


deleteWavesource : Values -> Int -> Values
deleteWavesource values index =
    { values
        | wavesources = List.Extra.removeAt index values.wavesources
    }


{-| Given the model and pointer event, decide which wavesource to select
(creating a new wavesource if appropriate) and transition `pointerState` to
`Selected`.
-}
selectWavesource : Model -> Vec2 -> Model
selectWavesource model pointerCoords =
    let
        maybeClosestWavesource =
            model.values.wavesources
                |> List.indexedMap
                    (\index coords -> ( Vec2.distanceSquared coords pointerCoords, index ))
                |> List.Extra.minimumBy
                    (\( dist, _ ) -> dist)
    in
    case maybeClosestWavesource of
        Nothing ->
            -- Must be no values; Add a new wavesource and select that
            { model
                | pointerState = Selected 0
                , values = addWavesource model.values pointerCoords
            }

        Just ( dist, index ) ->
            if dist < selectionDistancePx || List.length model.values.wavesources >= maxNumWavesources then
                -- Either the pointer is close to a wavesource, or we've reached
                -- the maximum allowed wavesources. In both cases we select the
                -- closest wavesource rather than adding another.
                { model
                    | pointerState = Selected index
                    , values = moveWavesource model.values index pointerCoords
                }

            else
                -- Too far away to select an existing source, so we add a new
                -- one and select that
                let
                    newIndex =
                        List.length model.values.wavesources
                in
                { model
                    | pointerState = Dragging newIndex
                    , values = addWavesource model.values pointerCoords
                }



--- ANIMATION STATE FUNCTIONS ---


advanceAnimation : Model -> Float -> Model
advanceAnimation model timeStep =
    let
        newAnimState =
            List.map (advanceValue timeStep) model.animationState

        oldValues =
            model.values

        newValues =
            { oldValues
                | wavelength =
                    newAnimState
                        |> List.Extra.getAt 0
                        |> Maybe.map (\v -> (v.value + 1.25) * 50)
                        |> Maybe.withDefault 0
                , falloff =
                    newAnimState
                        |> List.Extra.getAt 1
                        |> Maybe.map (\v -> (v.value + 1.5) * 20)
                        |> Maybe.withDefault 0
                , threshold =
                    newAnimState
                        |> List.Extra.getAt 2
                        |> Maybe.map (\v -> v.value * 0.1)
                        |> Maybe.withDefault 0
            }
    in
    { model
        | values = newValues
        , animationState = newAnimState
    }


type alias AnimationState =
    List AnimatedValue


initialAnimationState : AnimationState
initialAnimationState =
    [ { value = 0
      , phase = 23500
      , freq = 1 / 5000
      }
    , { value = 0
      , phase = 25100
      , freq = 1 / 10000
      }
    , { value = 0
      , phase = 50000
      , freq = 1 / 15000
      }
    ]


type alias AnimatedValue =
    { value : Float
    , phase : Float
    , freq : Float
    }


advanceValue : Float -> AnimatedValue -> AnimatedValue
advanceValue timeStep old =
    let
        newPhase =
            old.phase + timeStep

        newValue =
            newPhase
                |> (*) old.freq
                |> Basics.radians
                |> Basics.sin
    in
    { old
        | value = newValue
        , phase = newPhase
    }



--- VIEW FUNCTIONS ---


viewCanvas : Values -> Html UpdateMsg
viewCanvas values =
    WebGL.toHtml
        [ values.canvasSize |> Vec2.getX |> round |> (*) values.resolutionMultiplier |> Attr.width
        , values.canvasSize |> Vec2.getY |> round |> (*) values.resolutionMultiplier |> Attr.height
        , Attr.class "webgl-canvas"
        , Attr.id canvasId
        , Pointer.onDown (PointerUpdate << PointerDown)
        , Pointer.onMove (PointerUpdate << PointerMove)
        , Pointer.onUp (PointerUpdate << PointerUp)
        ]
        [ WebGL.entity
            vertexShader
            fragmentShader
            mesh
            (valuesToUniforms values)
        ]


{-| Calculate step size between `min` and `max` for `numSteps`
-}
getStep : Float -> Float -> Float -> Float
getStep min max numSteps =
    (max - min) / numSteps


slider : String -> Float -> ( Float, Float ) -> (Float -> Values) -> Html UpdateMsg
slider sliderName value ( minVal, maxVal ) onChange =
    let
        sliderPercent =
            100 * ((value - minVal) / (maxVal - minVal))
    in
    div
        []
        [ label
            [ for ("slider-" ++ sliderName) ]
            [ text sliderName ]
        , input
            [ type_ "range"
            , name ("slider-" ++ sliderName)
            , Attr.attribute
                "style"
                ("--meter-value: " ++ String.fromFloat sliderPercent ++ "%;")
            , Attr.value <| String.fromFloat value
            , Attr.min (minVal |> String.fromFloat)
            , Attr.max (maxVal |> String.fromFloat)
            , Attr.step (getStep minVal maxVal 200 |> String.fromFloat)
            , onInput
                (\v ->
                    v
                        |> String.toFloat
                        |> Maybe.withDefault minVal
                        |> onChange
                        |> SliderUpdate
                )
            ]
            []
        ]


viewControls : Values -> Html UpdateMsg
viewControls values =
    div []
        [ slider
            "wavelength"
            values.wavelength
            ( 10, 300 )
            (\v -> { values | wavelength = v })
        , slider
            "falloff"
            values.falloff
            ( 0, 60 )
            (\v -> { values | falloff = v })
        , slider
            "threshold"
            values.threshold
            ( -0.4, 0.4 )
            (\v -> { values | threshold = v })
        ]


view : Values -> Html UpdateMsg
view values =
    div []
        [ viewCanvas values
        , viewControls values
        ]



--- WEBGL STUFF ---


{-| The values sent to GLSL
-}
type alias Uniforms =
    { canvasSize : Vec2
    , resolutionMultiplier : Int
    , wavelength : Float
    , falloff : Float
    , threshold : Float

    -- We have to list all possible wave sources as separate uniforms.
    -- First two values are coordinates, final is 1 if active or 0 otherwise.
    , wavesource0 : Vec3
    , wavesource1 : Vec3
    , wavesource2 : Vec3
    , wavesource3 : Vec3
    , wavesource4 : Vec3
    , wavesource5 : Vec3
    , wavesource6 : Vec3
    , wavesource7 : Vec3
    , wavesource8 : Vec3
    , wavesource9 : Vec3
    , wavesource10 : Vec3
    }


valuesToUniforms : Values -> Uniforms
valuesToUniforms values =
    { canvasSize = values.canvasSize
    , resolutionMultiplier = values.resolutionMultiplier
    , wavelength = values.wavelength
    , falloff = values.falloff
    , threshold = values.threshold
    , wavesource0 = getWavesource values.wavesources 0
    , wavesource1 = getWavesource values.wavesources 1
    , wavesource2 = getWavesource values.wavesources 2
    , wavesource3 = getWavesource values.wavesources 3
    , wavesource4 = getWavesource values.wavesources 4
    , wavesource5 = getWavesource values.wavesources 5
    , wavesource6 = getWavesource values.wavesources 6
    , wavesource7 = getWavesource values.wavesources 7
    , wavesource8 = getWavesource values.wavesources 8
    , wavesource9 = getWavesource values.wavesources 9
    , wavesource10 = getWavesource values.wavesources 10
    }


type alias Vertex =
    { position : Vec3
    }


{-| Simple rectangle to fill screen
-}
mesh : Mesh Vertex
mesh =
    WebGL.triangles
        [ ( Vertex (vec3 -1 -1 0)
          , Vertex (vec3 -1 1 0)
          , Vertex (vec3 1 -1 0)
          )
        , ( Vertex (vec3 1 -1 0)
          , Vertex (vec3 -1 1 0)
          , Vertex (vec3 1 1 0)
          )
        ]


{-| Vertex shader just draws a rectangle, all the interesting stuff happens in
the fragment shader.
-}
vertexShader : Shader Vertex Uniforms {}
vertexShader =
    [glsl|
        attribute vec3 position;
        void main () {
            gl_Position = vec4(position, 1.0);
        }
    |]


{-| This is the fun bit! We calculate the interference pattern based on the
values passed through by Elm. Unfortunately, a limitation in Elm means we can't
pass an array of `Vec2` so we instead pass each wavesource coordinate
separately. I think we could alternatively convert these to a flat array and
resconstruct in GLSL, but this is fine for now.
-}
fragmentShader : Shader {} Uniforms {}
fragmentShader =
    [glsl|
        precision mediump float;

        uniform vec2 canvasSize;
        uniform int resolutionMultiplier;
        uniform float wavelength;
        uniform float falloff;
        uniform float threshold;

        uniform vec3 wavesource0;
        uniform vec3 wavesource1;
        uniform vec3 wavesource2;
        uniform vec3 wavesource3;
        uniform vec3 wavesource4;
        uniform vec3 wavesource5;
        uniform vec3 wavesource6;
        uniform vec3 wavesource7;
        uniform vec3 wavesource8;
        uniform vec3 wavesource9;
        uniform vec3 wavesource10;

        vec2 pixelToClipSpace(in vec2 coords) {
            coords /= canvasSize;
            coords.y = 1.0 - coords.y;
            return coords;
        }

        void main() {
            vec2 pixel_pos = gl_FragCoord.xy / (canvasSize * float(resolutionMultiplier));

            float dist0 = distance(pixel_pos, pixelToClipSpace(wavesource0.xy));
            float dist1 = distance(pixel_pos, pixelToClipSpace(wavesource1.xy));
            float dist2 = distance(pixel_pos, pixelToClipSpace(wavesource2.xy));
            float dist3 = distance(pixel_pos, pixelToClipSpace(wavesource3.xy));
            float dist4 = distance(pixel_pos, pixelToClipSpace(wavesource4.xy));
            float dist5 = distance(pixel_pos, pixelToClipSpace(wavesource5.xy));
            float dist6 = distance(pixel_pos, pixelToClipSpace(wavesource6.xy));
            float dist7 = distance(pixel_pos, pixelToClipSpace(wavesource7.xy));
            float dist8 = distance(pixel_pos, pixelToClipSpace(wavesource8.xy));
            float dist9 = distance(pixel_pos, pixelToClipSpace(wavesource9.xy));
            float dist10 = distance(pixel_pos, pixelToClipSpace(wavesource10.xy));

            float continuous = (
              + wavesource0.z * sin(dist0 * wavelength) / (1.0 + (dist0 * falloff))
              + wavesource1.z * sin(dist1 * wavelength) / (1.0 + (dist1 * falloff))
              + wavesource2.z * sin(dist2 * wavelength) / (1.0 + (dist2 * falloff))
              + wavesource3.z * sin(dist3 * wavelength) / (1.0 + (dist3 * falloff))
              + wavesource4.z * sin(dist4 * wavelength) / (1.0 + (dist4 * falloff))
              + wavesource5.z * sin(dist5 * wavelength) / (1.0 + (dist5 * falloff))
              + wavesource6.z * sin(dist6 * wavelength) / (1.0 + (dist6 * falloff))
              + wavesource7.z * sin(dist7 * wavelength) / (1.0 + (dist7 * falloff))
              + wavesource8.z * sin(dist8 * wavelength) / (1.0 + (dist8 * falloff))
              + wavesource9.z * sin(dist9 * wavelength) / (1.0 + (dist9 * falloff))
              + wavesource10.z * sin(dist10 * wavelength) / (1.0 + (dist10 * falloff))
              + 1.0
            ) / 2.0;
            float val = step(0.5, continuous + threshold);
            gl_FragColor = vec4(val, val, val, 1);
        }
    |]



--- HELPER FUNCTIONS ---


vec2FromTuple : ( Float, Float ) -> Vec2
vec2FromTuple ( x, y ) =
    vec2 x y


resizeVec : Vec2 -> Vec2 -> Vec2 -> Vec2
resizeVec sizeBefore sizeAfter targetVec =
    let
        xscale =
            Vec2.getX sizeAfter / Vec2.getX sizeBefore

        yscale =
            Vec2.getY sizeAfter / Vec2.getY sizeBefore

        x =
            Vec2.getX targetVec

        y =
            Vec2.getY targetVec
    in
    vec2 (x * xscale) (y * yscale)
