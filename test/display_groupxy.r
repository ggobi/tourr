### Name: display_groupxy
### Title: Display 2D tour projections displayed separately by groups
### Aliases: display_groupxy animate_groupxy

### ** Examples

f <- flea[, 1:6]
col <- rainbow(length(unique(flea$species)))[as.numeric(as.factor(flea$species))]
pch <- as.numeric(flea$species) + 14

animate_groupxy(f, col = col, pch = pch, group_by = flea$species)
animate_groupxy(f, col = col, pch = pch, group_by = flea$species, plot_xgp = FALSE)



