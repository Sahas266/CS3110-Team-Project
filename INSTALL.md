# Introduction
This file will provide instructions on how to run this project!

# System Requirements
This project runs in a Linux environment. Please make sure you are running the project in any Linux distribution.

# Packages
All of the below installation prompts are to be done in a Linux terminal. 

This project uses OCaml libraries, so the OPAM package manager needs to be downloaded. To install, in your terminal please run:

```text
$ sudo apt update
$ sudo apt install opam
$ opam init
$ eval $(opam env)
```
This also project uses Dune as the build system as it is an OCaml project. To install, in your terminal please run:

```text
$ opam install dune
```

You can check if it was installed correctly by running:

```text
$ dune --version
```

This should return a single line containing the version number of the Dune build system currently installed in your active environment. Finally, this project uses the TSDL package to generate its UI. To successfully run the code, in your terminal please run:

```text
$ opam install -y tsdl
```

# Testing the Project
We are currently displaying the graphics in the terminal, so you can run this command to check out our progress!

```text
$ dune exec bin/main.exe
```

Please enter commands in the text box under the board display generated.