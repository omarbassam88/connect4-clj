# Connect 4 with Clojure

A CLI Connect Four game written in Clojure implementing the Minimax Algorithm.
Planning to add the alpha pruning technique as well.

## Requirements

- Clojure
- lein

## Usage

To run the project using lein:

```sh
lein run
```

To build a `.jar` file:

```sh
lein uberjar
```

Then you can directly run the uber jar:

```sh
java -jar target/uberjar/connect4-clj-0.1.0-SNAPSHOT-standalone.jar
```
