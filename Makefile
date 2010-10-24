
all: display


monitor:
	sbcl --noinform --noprint --disable-debugger --load letf.lisp nil monitor-config.txt nil

server:
	sbcl --noinform --noprint --disable-debugger --load letf.lisp nil server-config.txt nil

compile:
	sbcl --noinform --noprint --disable-debugger --load letf.lisp nil server-compile.txt nil

display:
	sbcl --noinform --noprint --disable-debugger --load letf.lisp nil display-config.txt nil

daq:
	sbcl --noinform --noprint --disable-debugger --load letf.lisp nil daq-config.txt nil
