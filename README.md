# Clojure Tetris

This is a very basic implementation of Tetris written in Clojure.

I originally wrote it while teaching myself Clojure. Hopefully it'll help someone else out there learn this
awesome programming language.

Features and implementation details:
+ Ghost piece
+ Wall kick! (http://tetris.wikia.com/wiki/Wall_kick)
+ Drawn using a double buffered Java Swing Canvas

## Usage

To run, just type:

   lein run

You can pass your starting level on startup if you want to skip the early stages:

   lein run -- --level 15

## License

Copyright (C) 2013 Michael Quinn, See http://opensource.org/licenses/MIT for details.
