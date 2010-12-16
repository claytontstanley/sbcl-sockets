
all: test

ccl:
	ccl64 --load letf.lisp -- nil testServer.txt nil

test:
	../darwin/sbcl --core ../darwin/sbcl.core --noinform --noprint --disable-debugger --load letf.lisp nil testServer.txt nil
