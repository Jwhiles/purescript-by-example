# purescript-by-example
Code from working thru purescript by example

## Notes


### Differnces from Haskell

### Composition
rather than using `.` for function composition as in Haskell, we instead use
`>>>` and `<<<`. 

`<<<` is equivalent to `.`, or standard composition. While `>>>` is forward 
composition, often known as the pipe or pipeline operator. 

### Types

PureScript allows for quantified types for example `forall a. a -> ....` I think that the
fullstop is jus delimiting the quantifier 
