### Name: geodesic_path
### Title: Generate geodesic path.
### Aliases: geodesic_path
### Keywords: internal

### ** Examples

a <- basis_random(4, 2)
b <- basis_random(4, 2)
path <- geodesic_path(a, b)

path$dist
all.equal(a, path$interpolate(0))
# Not true generally - a rotated into plane of b
all.equal(b, path$interpolate(1))



