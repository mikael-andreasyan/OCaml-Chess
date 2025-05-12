In order to be able to run our MS3 demo, you will need to have the ocaml graphics library and 

"opam install graphics"
"opam install base"


THIS SECTION ONLY APPLIES TO WINDOWS. YOU WILL NEED IT TO SEE GUI
-Download VcXsrv at https://sourceforge.net/projects/vcxsrv/
-Run the installer with default options
-Open the app
 -Launch it using "Multiple windows"
 -Enable "Disable access control" (for testing),
 -Enable "Native OpenGL" (optional).
 -Keep it running in the background.
-Run "export DISPLAY=:0" in your terminal
-To test if it works, do the following code. It'll open a silly little app with eyes following your mouse:
  -sudo apt update
  -sudo apt install x11-apps
  -xeyes

Whenever you run the app on windows, you must do these 2 lines
export DISPLAY=:0
dune exec bin/main.exe

Run



How to use the app:
