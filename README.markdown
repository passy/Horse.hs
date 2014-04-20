Horse.hs
========

Tweet like a Horse.

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
cat data/*.txt | tr -d '\n' | cabal run horse.conf
```
