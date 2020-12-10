### Name: freeze
### Title: Freeze and thaw matrices
### Aliases: freeze thaw
### Keywords: internal

### ** Examples

frozen <- matrix(NA, nrow = 4, ncol = 2)
frozen[3, ] <- .5

input <- basis_random(4, 2)
freeze(input, frozen)
thaw(input, frozen)
freeze(basis_random(4, 2), frozen)



