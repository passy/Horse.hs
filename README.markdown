Horse.hs
========

Tweet like a Horse.

*Note:* The output is really not as random as it should be and thus less funny.
Working on it, but don't hold your breath.

Development
-----------

```bash
cabal sandbox init
cabal install --only-dependencies
cabal build
```

Running
-------

```bash
cat data/*.txt | cabal run horse.conf
```
