# The Haskell In-Memory Interactive Database System - HimiDB

* Realised at [MuniHac 2018](https://munihac.github.io/)
* Based on [Scala API project](https://github.com/jbackfield/BecomingFunctional.git)
 
## Idea 

```
Scala -> Haskell
```

## Build -> Run tests -> Run up the himiDB

```
stack build && stack test && stack exec himiDB
```

## Built With

* [Haskellstack](https://docs.haskellstack.org/en/stable/README/) - The Haskell Tool Stack
* [Hspec](https://hspec.github.io/) - A Testing Framework For Haskell
* [Megaparsec](http://hackage.haskell.org/package/megaparsec) - A Haskell parsing library, used for the CLI
* [Haskeline](http://hackage.haskell.org/package/haskeline) - A Haskell command line interface

## himiDB CLI
 
    Usage:

    create:
        Create a table
        > create myTable (a int, b int, c int)
        > create tableName (col1 int, col2 string) description goes here

    drop:
        Delete a table and its contents
        > drop tableName

    describe:
        Show all tables, or the data of one table
        > describe
        > describe tableName

    insert:
        Insert a row into a table
        > insert tableName (1, "me")
        > insert tableName (1, "me") (2, "you")

    select:
        Select data from a table
        > select tableName *
        > select tableName col1
        > select tableName (col1, col2)
        > select tableName * where col1 > 4 || col2 == "me"

    delete:
        Delete data from a table
        > delete tableName
        > delete tableName where col1 > 4 || col2 == "me"

    help:
        Show the help guide, with examples

    exit:
        Exit and clear the database
        > ^D