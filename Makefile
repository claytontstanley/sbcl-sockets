
all: client

server:
	sbcl --noinform --noprint --disable-debugger --load letf.lisp nil server-config.txt nil

client:
	sbcl --noinform --noprint --disable-debugger --load letf.lisp nil client-config.txt nil
