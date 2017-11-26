# toy-robot

## Assumptions
* All invalid commands will be ignored, not just commands that will result in an impossible position.
* When reading from file, the executable will only accept one input file.

## Prerequisite
[Haskell Stack](https://docs.haskellstack.org/en/stable/README/)

On Mac, simply run the following in a terminal window
```
brew install haskell-stack
```

## Build the project
Under the project's root directory, run
```
stack build
```

## Run the app in REPL mode
The interactive REPL mode allows you to type the Robot commands directly.

Use ctrl-c to exit REPL mode.

To start up the REPL mode, under the project's root directory, run
```
stack exec toy-robot-exe
```

## Run the app on a given input file
Note the program will exit upon finishing executing commands from the input file.

Under the project's root directory, run
```
stack exec toy-robot-exe "path_to_input_file"
```
For example:
```
stack exec toy-robot-exe "resources/input.txt"
```

## Run tests
Under the project's root directory, run
```
stack test
```
