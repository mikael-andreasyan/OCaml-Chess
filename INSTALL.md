In order to be able to run our MS3 demo, you will need to have the ocaml graphics library and jane street base library

Run the following commands to install:
"opam install base"
"opam install graphics"
(NOTE: if you're on mac and after running opam install grapics you see the option to "download with brew" (or something along those lines), select that option. If it says installing xquarts or something like that, then you can skip the first two steps of mac installation process)


THIS SECTION ONLY APPLIES TO MAC. YOU WILL NEED IT TO SEE GUI
-Install xquartz at https://www.xquartz.org/
-Run the pkg to install
-RESTART THE COMPUTER (very important step)
-In terminal:
  -run "open -a XQuartz"
  
THIS SECTION ONLY APPLIES TO WINDOWS. YOU WILL NEED IT TO SEE GUI
-Download VcXsrv at https://sourceforge.net/projects/vcxsrv/
-Run the installer with default options
-Open the app
 -Launch it using "Multiple windows"
 -Enable "Disable access control" (for testing),
 -Enable "Native OpenGL" (optional).
 -Keep it running in the background.
-Run the following lines: 
  -"sudo apt update"
  -"sudo apt install x11-apps"


FOR BOTH SYSTEMS, TO CHECK IF INSTALLATION WORKED
-Run "export DISPLAY=:0" in your terminal
-To test if it works, do the following terminal command. It'll open a silly little app with eyes following your mouse:
  -"xeyes"

Whenever you run our chess app on a fresh terminal window, you will need to run
"export DISPLAY=:0"

Okay now that all the complicated installation is out of the way, here is how you run the app:

"dune exec bin/main.exe" -> this option launches the game in hotseat mode, where you can play against a friend on the same computer
"dune exec bin/main.exe white" -> this option launches the game against the AI, where you play as white
"dune exec bin/main.exe black" -> this option launches the game against the AI, where you play as black