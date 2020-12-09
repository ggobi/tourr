### Name: display_pca
### Title: Display tour path with principal component scores with original
###   axes
### Aliases: display_pca animate_pca

### ** Examples

flea_std <- scale(flea[, 1:6])
flea_pca <- prcomp(flea_std, center = FALSE, )
flea_coefs <- flea_pca$rotation[, 1:3]
flea_scores <- flea_pca$x[, 1:3]
animate_pca(flea_scores, pc_coefs = flea_coefs)



