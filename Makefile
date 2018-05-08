all:
	elm-make App.elm --output App.js --warn

setup:
	elm-package install

clean:
	rm App.js

#serve:
#    python -m SimpleHTTPServer

debug:
	echo "goto http://localhost:8000/App.elm" && elm-reactor 
