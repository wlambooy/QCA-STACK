# QCA-STACK
QCA-STACK is a QCA design and simulation system written in Haskell. The system simulates discretely (with a modified bistable approximation simulation engine) and has no limits regarding the third spacial dimension, lending itself well to theoretical exploration with (stackable) three-dimensional designs. Read the thesis included in this repository for more information.

*NB:* The project was made on Windows and has thus not been tested on Linux or macOS.

## Instructions on setting up with Jetbrains IntelliJ IDEA
1. [Install Haskell Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/).
2. [Install Jetbrains IntelliJ IDEA](https://www.jetbrains.com/idea/download/) and [install the plugin IntelliJ-Haskell](https://github.com/rikvdkleij/intellij-haskell#installing-the-plugin). The latter link has detailed instruction on running a stack project in IntelliJ. Consult this if the following steps do not work for you.
3. Clone this repository or make sure you have the `code` folder and its contents on your file system.
4. Open the aforementioned folder in IntelliJ with the following steps (the steps could depend on your IntelliJ version):
    - Import project, select the `code` directory.
    - Select "Import project from external model" and select Haskell Stack.
    - Click configure on "Haskell Tool Stack" and select the path to the stack binary. On Windows, this is typically in: `C:\Users\<username>\AppData\Roaming\local\bin\stack.exe`.
5. Under the 'Haskell' menu, click "Update Settings and restart REPLs".
6. Now you can add run configurations:
    - Add new, Haskell Stack -> Haskell Stack REPL. Give a name to the run configuration (for instance "REPL"), tick "Allow running in parallel" and press OK. This configuration will launch GHCi.
    - Add new, Haskell Stack -> Haskell Stack Runner. Give a name again (for instance "RUN") and press OK. This configuration will compile the project running the `main` function in `app/Main.hs` to an exe; the output is written to `.stack-work/dist/<buildid>/build/QCA-STACK-exe/QCA-STACK-exe.exe`.
7. You are now ready to explore the possibilities of quantum-dot cellular automata with QCA-STACK.


