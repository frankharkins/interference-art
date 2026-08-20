module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html, div, input, label, text)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (onInput)
import Html.Events.Extra.Pointer as Pointer exposing (Event, onDown)
import List.Extra exposing (getAt)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, distanceSquared, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Platform.Cmd as Cmd
import Task
import WebGL exposing (Mesh, Shader)



--- Constants ---
-- Element ID of the webgl canvas


canvasId : String
canvasId =
    "webgl-canvas"



------


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
                        ( model, Cmd.none )

                    SliderUpdate newValues ->
                        ( { model | values = newValues }, Cmd.none )

                    PointerUpdate event ->
                        ( pointerUpdate model event, Cmd.none )

                    WindowResized _ _ ->
                        ( model, getCanvasSize )

                    CanvasSizeUpdate result ->
                        case result of
                            Err _ ->
                                ( model, Cmd.none )

                            Ok el ->
                                let
                                    oldValues =
                                        model.values
                                in
                                ( { model
                                    | values =
                                        { oldValues
                                            | canvasSize = vec2 el.element.width el.element.height
                                        }
                                  }
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


type alias Model =
    { values : Values
    , pointerState : PointerState
    }


defaultModel : Model
defaultModel =
    { values = defaultValues
    , pointerState = Inactive
    }


type alias Values =
    { canvasSize : Vec2
    , wavesources : List Vec2
    , wavelength : Float
    , falloff : Float
    , threshold : Float
    }


defaultValues : Values
defaultValues =
    { canvasSize = vec2 576 768
    , wavesources =
        [ vec2 0.33 0.75
        , vec2 0.66 0.25
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
                    selectWavesource model event

                PointerUp _ ->
                    model

                PointerMove _ ->
                    model

        Selected index ->
            case action of
                PointerUp _ ->
                    { pointerState = Inactive
                    , values = deleteWavesource model.values index
                    }

                PointerDown _ ->
                    model

                PointerMove event ->
                    { pointerState = Dragging index
                    , values =
                        moveWavesource
                            model.values
                            index
                            event.pointer.offsetPos
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
                                event.pointer.offsetPos
                    }


deleteWavesource : Values -> Int -> Values
deleteWavesource values index =
    { values
        | wavesources = List.Extra.removeAt index values.wavesources
    }


selectWavesource : Model -> Pointer.Event -> Model
selectWavesource model event =
    let
        fractionalPointerCoords =
            pixelCoordsToFractional event.pointer.offsetPos model.values.canvasSize

        maybeClosestWavesource =
            model.values.wavesources
                |> List.indexedMap
                    (\index coords -> ( Vec2.distanceSquared coords fractionalPointerCoords, index ))
                |> List.Extra.minimumBy
                    (\( dist, _ ) -> dist)
    in
    case maybeClosestWavesource of
        Nothing ->
            -- Must be no values; Add a new wavesource and select that
            { pointerState = Selected 0
            , values = addWavesource model.values event.pointer.offsetPos
            }

        Just ( dist, index ) ->
            if dist > 0.03 && List.length model.values.wavesources < 10 then
                -- Too far away to select a source so we add a new one and select that
                let
                    newIndex =
                        List.length model.values.wavesources
                in
                { pointerState = Dragging newIndex
                , values = addWavesource model.values event.pointer.offsetPos
                }

            else
                { pointerState = Selected index
                , values = moveWavesource model.values index event.pointer.offsetPos
                }



-- TODO: Work with pixel coords everywhere and convert in shader


pixelCoordsToFractional : ( Float, Float ) -> Vec2 -> Vec2
pixelCoordsToFractional coords canvasSize =
    vec2
        (coords |> Tuple.first |> (\x -> x / Vec2.getX canvasSize))
        (coords |> Tuple.second |> (\y -> y / Vec2.getY canvasSize))


addWavesource : Values -> ( Float, Float ) -> Values
addWavesource old coords =
    { old
        | wavesources =
            old.wavesources
                ++ [ pixelCoordsToFractional coords old.canvasSize ]
    }


moveWavesource : Values -> Int -> ( Float, Float ) -> Values
moveWavesource old index coords =
    let
        fractionalCoords =
            pixelCoordsToFractional coords old.canvasSize

        newWavesources =
            List.Extra.setAt index fractionalCoords old.wavesources
    in
    { old | wavesources = newWavesources }


viewCanvas : Values -> Html UpdateMsg
viewCanvas values =
    WebGL.toHtml
        [ values.canvasSize |> Vec2.getX |> round |> Attr.width
        , values.canvasSize |> Vec2.getY |> round |> Attr.height
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
            , Attr.step (getStep minVal maxVal 100 |> String.fromFloat)
            , onInput
                (\v ->
                    v
                        |> String.toFloat
                        |> Maybe.withDefault 0
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
            ( 20, 300 )
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



-- Mesh


type alias Vertex =
    { position : Vec3
    }



-- Rectangle to fill screen


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



-- Shaders


getWavesource : List Vec2 -> Int -> Vec3
getWavesource sourceList index =
    sourceList
        |> List.Extra.getAt index
        |> Maybe.map
            (\v ->
                vec3
                    (Vec2.getX v)
                    (1 - Vec2.getY v)
                    1.0
            )
        |> Maybe.withDefault
            (vec3 0 0 0)


type alias Uniforms =
    { canvasSize : Vec2
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



-- Vertex shader just draws a rectangle, all the interesting stuff happens in
-- the fragment shader.


vertexShader : Shader Vertex Uniforms {}
vertexShader =
    [glsl|
        attribute vec3 position;
        void main () {
            gl_Position = vec4(position, 1.0);
        }
    |]


fragmentShader : Shader {} Uniforms {}
fragmentShader =
    [glsl|
        precision mediump float;

        uniform vec2 canvasSize;
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

        void main() {
            vec2 pixel_pos = gl_FragCoord.xy / canvasSize;

            float dist0 = distance(pixel_pos, wavesource0.xy);
            float dist1 = distance(pixel_pos, wavesource1.xy);
            float dist2 = distance(pixel_pos, wavesource2.xy);
            float dist3 = distance(pixel_pos, wavesource3.xy);
            float dist4 = distance(pixel_pos, wavesource4.xy);
            float dist5 = distance(pixel_pos, wavesource5.xy);
            float dist6 = distance(pixel_pos, wavesource6.xy);
            float dist7 = distance(pixel_pos, wavesource7.xy);
            float dist8 = distance(pixel_pos, wavesource8.xy);
            float dist9 = distance(pixel_pos, wavesource9.xy);
            float dist10 = distance(pixel_pos, wavesource10.xy);

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
