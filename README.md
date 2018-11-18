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
 
```
create: Create a table
drop: Delete a table and its contents
describe: Show all tables, or the data of one table
insert: Insert a row into a table
select: Select data from a table
delete: Delete data from a table
help: Show the help guide, with examples
exit: Exit and clear the database
```
