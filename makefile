SRC_PURS=$(wildcard src/*.purs)
SRC_JS=$(wildcard src/*.js)

dist/bundle.js: $(SRC_PURS) $(SRC_JS)
	bower install
	pulp build --to dist/game.js

clean:
	rm dist/game.js
	rm -rf bower_components
	rm -rf output