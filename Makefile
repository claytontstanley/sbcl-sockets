
all: client


monitor:
	sbcl --noinform --noprint --disable-debugger --load letf.lisp nil monitor-config.txt nil

server:
	sbcl --noinform --noprint --disable-debugger --load letf.lisp nil server-config.txt nil

compile:
	sbcl --noinform --noprint --disable-debugger --load letf.lisp nil server-compile.txt nil

client:
	sbcl --noinform --noprint --disable-debugger --load letf.lisp nil client-config.txt nil
