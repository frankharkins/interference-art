.PHONY: install build run

install: # Install everything needed to build this project
	echo "Installing elm..."
	mkdir -p ./.bin
	curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.2/elm-0.19.2-linux-x64.gz
	gunzip elm.gz
	chmod +x elm
	mv elm ./.bin/

	npm install

format:
	npm run format


build:
	rm -rf ./build && mkdir -p ./build
	./.bin/elm make src/Main.elm --output=build/main.js
	cp src/*.{html,css,svg} ./build/

run:
	make build
	(cd build && python -m http.server)
