say_hello:
		@echo "available targets: mandy, tsp, clean"
mandy:
		@echo "Building mandlebrot viewer"
		buildapp --output mandy --load ~/quicklisp/setup.lisp --load mandy.lisp --entry mandy::main
tsp-viewer:
		@echo "Building tsp viewer"
		buildapp --output tsp-viewer --load ~/quicklisp/setup.lisp --load tsp-viewer.lisp --entry tsp-viewer::main
clean:
		@echo "Cleaning up..."
		rm mandy
		rm tsp-viewer
