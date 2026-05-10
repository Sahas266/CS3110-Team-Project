# Introduction
This file will provide instructions on how to run this project!

# System Requirements
This project runs in Linux and macOS environments.

# Packages
Use the setup steps for your operating system.

## Linux (Ubuntu/Debian)

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

## macOS

All of the below installation prompts are to be done in a macOS terminal.

This project uses OCaml libraries, so OPAM needs to be installed first. We recommend using Homebrew.

If you already have Homebrew installed, skip the Homebrew installation command and start at `brew update`.

In your terminal, run:

```text
$ /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
$ brew update
$ brew install opam dune sdl2
$ opam init
$ eval "$(opam env)"
```

You can check if Dune was installed correctly by running:

```text
$ dune --version
```

Finally, install the OCaml UI package used by this project:

```text
$ opam install -y tsdl
```

# Testing the Project
You can run this command to check out the GUI and functionality for our project:

```text
$ dune exec bin/main.exe
```

Please enter commands in the text box under the board display generated.