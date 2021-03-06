COMMAND = darwin/sbcl --core darwin/sbcl.core  

all: server

#server to handle modbus comm with the daq, and relay floating-point vals (w/ names) to main server
daq2: compile
	$(COMMAND) --noinform --noprint --disable-debugger --load letf.lisp nil daq2-config.txt nil

#server to mock up actual labjack device; communicates with above server
labjack: compile
	$(COMMAND) --noinform --noprint --disable-debugger --load letf.lisp nil labjack-config.txt nil

#monitor for daq2 server
monitor2:
	$(COMMAND) --noinform --noprint --disable-debugger --load letf.lisp nil monitor-config2.txt nil

#monitor for main server
monitor:
	$(COMMAND) --noinform --noprint --disable-debugger --load letf.lisp nil monitor-config.txt nil

#main server; handles comm between daq and display
server: compile
	$(COMMAND) --noinform --noprint --disable-debugger --load letf.lisp nil server-config.txt nil

#main server running ccl64
server-ccl:
	ccl64 --load letf.lisp -- nil server-config-ccl.txt nil

#compiles server.lisp -> server.fasl
compile:
	$(COMMAND) --noinform --noprint --disable-debugger --load letf.lisp nil server-compile.txt nil

#sbcl mock of display 
display:
	$(COMMAND) --noinform --noprint --disable-debugger --load letf.lisp nil display-config.txt nil

#ccl mock of display; will eventually turn into ccl code that binds with cocoa objects
display2:
	ccl64 --load letf.lisp -- nil display-config2.txt nil

#mock of daq2 server
daq:
	$(COMMAND) --noinform --noprint --disable-debugger --load letf.lisp nil daq-config.txt nil
