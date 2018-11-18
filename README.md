# The Haskell in-memory interactive database - HimiDB

```
HHHHHHHHH     HHHHHHHHH  iiii                            iiii  
H:::::::H     H:::::::H i::::i                          i::::i 
H:::::::H     H:::::::H  iiii                            iiii  
HH::::::H     H::::::HH                                        
  H:::::H     H:::::H  iiiiiii    mmmmmmm    mmmmmmm   iiiiiii 
  H:::::H     H:::::H  i:::::i  mm:::::::m  m:::::::mm i:::::i 
  H::::::HHHHH::::::H   i::::i m::::::::::mm::::::::::m i::::i 
  H:::::::::::::::::H   i::::i m::::::::::::::::::::::m i::::i 
  H:::::::::::::::::H   i::::i m:::::mmm::::::mmm:::::m i::::i 
  H::::::HHHHH::::::H   i::::i m::::m   m::::m   m::::m i::::i 
  H:::::H     H:::::H   i::::i m::::m   m::::m   m::::m i::::i 
  H:::::H     H:::::H   i::::i m::::m   m::::m   m::::m i::::i 
HH::::::H     H::::::HHi::::::im::::m   m::::m   m::::mi::::::i
H:::::::H     H:::::::Hi::::::im::::m   m::::m   m::::mi::::::i
H:::::::H     H:::::::Hi::::::im::::m   m::::m   m::::mi::::::i
HHHHHHHHH     HHHHHHHHHiiiiiiiimmmmmm   mmmmmm   mmmmmmiiiiiiii

DDDDDDDDDDDDD      BBBBBBBBBBBBBBBBB   
D::::::::::::DDD   B::::::::::::::::B  
D:::::::::::::::DD B::::::BBBBBB:::::B 
DDD:::::DDDDD:::::DBB:::::B     B:::::B
  D:::::D    D:::::D B::::B     B:::::B
  D:::::D     D:::::DB::::B     B:::::B
  D:::::D     D:::::DB::::BBBBBB:::::B 
  D:::::D     D:::::DB:::::::::::::BB  
  D:::::D     D:::::DB::::BBBBBB:::::B 
  D:::::D     D:::::DB::::B     B:::::B
  D:::::D     D:::::DB::::B     B:::::B
  D:::::D    D:::::D B::::B     B:::::B
DDD:::::DDDDD:::::DBB:::::BBBBBB::::::B
D:::::::::::::::DD B:::::::::::::::::B 
D::::::::::::DDD   B::::::::::::::::B  
DDDDDDDDDDDDD      BBBBBBBBBBBBBBBBB   
```

* Realised at [MuniHac 2018](https://munihac.github.io/)
 
## Idea 

Joshua Backfield’s book, _Becoming Functional: Steps for Transforming Into a Functional Programmer_, introduces a [very simple database engine](https://github.com/jbackfield/BecomingFunctional/tree/master/Chapter10/PuttingItAllTogether) written in Scala. This project is to convert this system into Haskell, and build a CLI frontend for it in a purer way than the Scala system uses.

## Building and executing:

    stack build
    stack test
    stack exec himiDB

## Built With

* [Haskellstack](https://docs.haskellstack.org/en/stable/README/) - The Haskell Tool Stack
* [Hspec](https://hspec.github.io/) - A Testing Framework For Haskell
* [Megaparsec](http://hackage.haskell.org/package/megaparsec) - A Haskell parsing library, used for the CLI
* [Haskeline](http://hackage.haskell.org/package/haskeline) - A Haskell command line interface

## Demo

    HimiDB v0.1
    The Haskell In-Memory Interactive Database System
    Created for the MuniHac 2018
    type `help` for instructions

    himiDB > create users (id int, name string) a table to describe my subscribers
    himiDB > insert users (1, "me") (2, "you") (3, "him")
    himiDB > insert users ("them", 4)
    Error: Wrong type for column id
    himiDB > describe
    users (id int, name string) a table to describe my subscribers
    himiDB > describe users
    users: a table to describe my subscribers
    id  | name
    int | string
    ----+-------
    3   | "him"
    2   | "you"
    1   | "me"
    himiDB > select users * where name == "me" || id > 2
    3 | "him"
    1 | "me"

## himiDB CLI
 
    Usage:

    - create:
        Create a table
        > create myTable (a int, b int, c int)
        > create tableName (col1 int, col2 string) description goes here

    - drop:
        Delete a table and its contents
        > drop tableName

    - describe:
        Show all tables, or the data of one table
        > describe
        > describe tableName

    - insert:
        Insert a row into a table
        > insert tableName (1, "me")
        > insert tableName (1, "me") (2, "you")

    - select:
        Select data from a table
        > select tableName *
        > select tableName col1
        > select tableName (col1, col2)
        > select tableName * where col1 > 4 || col2 == "me"

    - delete:
        Delete data from a table
        > delete tableName
        > delete tableName where col1 > 4 || col2 == "me"

    - help:
        Show the help guide, with examples

    - exit:
        Exit and clear the database
        > ^D
