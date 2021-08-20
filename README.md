# QCA-STACK
QCA-STACK is a QCA design and simulation system written in Haskell. The system simulates discretely (with a modified bistable approximation simulation engine) and has no limits regarding the third spacial dimension, lending itself well to theoretical exploration with (stackable) three-dimensional designs. Read the thesis included in this repository for more information.

The project was made on Windows and has thus not been tested on Linux or macOS.

## Instructions on setting up with Jetbrains IntelliJ IDEA
- [Install Haskell Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/).
- [Install Jetbrains IntelliJ IDEA](https://www.jetbrains.com/idea/download/) and [install the plugin IntelliJ-Haskell](https://github.com/rikvdkleij/intellij-haskell#installing-the-plugin).
- Clone this repository or make sure you have the 'code' folder and its contents on your file system.
- Open the aforementioned folder in IntelliJ.
- Open a terminal with its path set to the 'code' folder and run ```stack build```.


