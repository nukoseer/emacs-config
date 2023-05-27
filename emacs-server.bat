@echo off
REM To run this .bat at the startup, create shortcut of this script in shell:startup.
REM You can start emacsclient -n -c -a="" -e "(raise-frame)" if you have running daemon.
REM You also can (probably should) create shortcut for emacsclient.
start runemacs --daemon
