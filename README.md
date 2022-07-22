# drunner

drunner is a dmenu based application launcher that makes it easy to find and launch your favorite applications. It is fast, lightweight, and easy to use. Simply type in the name of the application you want to launch, and drunner will do the rest. It reads both .desktop files and PATH executables to find applications.

## Installation

First, install the following dependencies:

```sh
sudo pacman -S dmenu ocaml opam dune
```

Then, build and install the drunner binary:

```sh
make build
sudo make install
```