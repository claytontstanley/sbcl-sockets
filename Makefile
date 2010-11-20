
all: server

daq2: compile
	sbcl --noinform --noprint --disable-debugger --load letf.lisp nil daq2-config.txt nil

labjack: compile
	sbcl --noinform --noprint --disable-debugger --load letf.lisp nil labjack-config.txt nil

monitor2:
	sbcl --noinform --noprint --disable-debugger --load letf.lisp nil monitor-config2.txt nil

monitor:
	sbcl --noinform --noprint --disable-debugger --load letf.lisp nil monitor-config.txt nil

server: compile
	sbcl --noinform --noprint --disable-debugger --load letf.lisp nil server-config.txt nil

compile:
	sbcl --noinform --noprint --disable-debugger --load letf.lisp nil server-compile.txt nil

display:
	sbcl --noinform --noprint --disable-debugger --load letf.lisp nil display-config.txt nil

daq:
	sbcl --noinform --noprint --disable-debugger --load letf.lisp nil daq-config.txt nil
