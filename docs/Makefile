platform = $(shell uname | tr '[:upper:]' '[:lower:]')

all: htmlDocs

htmlDocs:
	../darwin/sbcl --core ../darwin/sbcl.core --noinform --noprint --disable-debugger --load letf.lisp $(platform) htmlConfig.txt nil

