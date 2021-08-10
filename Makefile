.PHONY: build-debug build-release gui-debug gui-release clean-haskell clean-elm serve docker docker-clean

build-debug:
	stack build --fast csdc-api

build-release:
	stack build csdc-api

gui-debug:
	cd csdc-gui && elm make src/Main.elm --output ../www/app.js

gui-release:
	cd csdc-gui && elm make src/Main.elm --optimize --output ../www/app.js

clean-haskell:
	stack clean

clean-elm:
	rm -r csdc-gui/elm-stuff

serve:
	stack exec csdc-server config.json secret.json