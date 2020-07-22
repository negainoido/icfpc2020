# interpreter

This is our Galaxy Evaluator written in Haskell

## Requirement
- gnuplot
- ruby
- GHC (8.8)


## How to run
You don't have to compile it. The evaluator is deployed on Cloud. 

```
ruby vis.rb
```

![galaxyinit](galaxy_init.png)

![galaxymain](galaxy_main.png)

You can click the window to interact with galaxy pad.

You can change the initial state, by changing
around Line 298 in `vis.rb` 

## How to build

## Docker

```
docker build -t interpreter .
```

## Local

```
cabal new-build
```
