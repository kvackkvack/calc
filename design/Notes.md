LOOK INTO:
  * Category theory
  * Homotype type theory
    
# Scalars and sets
```
-- # is the scalar type
-- {x} is a set of xs
-- * = {#, {*}}, ie the set of all sets
```

# Polymorphism as sets
```
-- 'x is the internal representation of x
x = {'x} -- ie a scalar that can only take the form x
1 = {'1} -- scalar that can only take the form 1
N = {{'1}, {'2}, {'3}, ...} -- set of scalars 
let m = (all n :> N. n) = {'1, '2, '3, ...} -- scalar that can be any number
Maybe a = { Just a, Nothing } = { {'Just a}, {'Nothing} } 
(all a <: {*}. Maybe a) = { Nothing, Just Int, Just String, Just Bool, ... }
```

# Proofs as things the compiler can use to simplify terms
http://www.joachim-breitner.de/blog/717-Why_prove_programs_equivalent_when_your_compiler_can_do_that_for_you_#fn1



# Things to be careful of

> I've heard noises about a future version of Idris being able to spot "Nat with types"-style datatypes (like Fin) and automatically erase the proofs and pack the values into machine integers, but as far as I know we're not there yet.
