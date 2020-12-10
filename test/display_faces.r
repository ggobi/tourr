### Name: display_faces
### Title: Chernoff faces tour path animation.
### Aliases: display_faces animate_faces
### Keywords: hplot

### ** Examples

# The drawing code is fairly slow, so this animation works best with a
# limited number of cases
animate_faces(flea[1:2, 1:6])
animate_faces(flea[1:4, 1:6])

animate_faces(flea[1:2, 1:6], grand_tour(5))



