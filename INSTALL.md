# Installation Instructions

## Playing on the command line:

We have included a `Makefile` for your convenience, adapted from CS 3110 assignments. Feel free to use it if you want; most of the commands are valid, including `make test` and `make build`.

- `make build` builds the project. Don't worry about this step if you simply want to play the game, in which you might find useful:

- `make play` launches OScrabbl. Once the game launches, you will find further instructions for play. When positioning a tile, the vertical axis represents the first coordinate, while the horizontal axis represents the second coordinate. 

  - Note: make sure to resize the window accordingly to ensure the entire game fits on the screen. Since the display also depends on the various font sizes and styles of your command line, it is impossible to guarantee the full OScrabbl experience on our end.

- `make test` runs the OUnit2 tests we wrote in `test.ml`. They are modularized into different .ml files, so you can test specific ones you want without having to modify any of the test case code. As our project is a game, much of our correctness has been verified through direct playtesting, but the core functionality of the game has been tried and through with these tests.

## Easy installation:

Ah, you are missing out my friend! Though many stories and testimonials can be shared to show off [the best programming language ever,](https://ocaml.org/) we understand if you would rather skip the mundane and cut to the chase of playing OScrabbl! Simply head over to the [releases](https://github.com/TrueshotBarrage/oscrabbl/releases) section and download the latest release to play. We have included a bash script executable that can be double clicked to launch the game.
