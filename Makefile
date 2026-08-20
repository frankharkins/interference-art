.PHONY: install build run

export PATH := ./node_modules/.bin:./.bin:$(PATH)

install: # Install everything needed to build this project
	echo "Installing elm..."
	mkdir -p ./.bin
	curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz
	gunzip elm.gz
	chmod +x elm
	mv elm ./.bin/

	npm install

format:
	elm-format . --yes


build:
	rm -rf ./build && mkdir -p ./build

	elm-optimize-level-2 --optimize-speed src/Main.elm --output=build/main.min.js
	mv ./build/main.min.js ./build/main.js

	terser ./build/main.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | terser --mangle --output=./build/main.min.js
	minify ./src/index.html > ./build/index.html
	minify ./src/main.css > ./build/main.css
	cp src/favicon.svg ./build/

run:
	make build
	(cd build && python -m http.server)
