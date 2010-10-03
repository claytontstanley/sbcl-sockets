platform = $(shell uname | tr '[:upper:]' '[:lower:]')

all: htmlDocs

htmlDocs:
	sbcl --noinform --noprint --disable-debugger --load letf.lisp $(platform) htmlConfig.txt nil

