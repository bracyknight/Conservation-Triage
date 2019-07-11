#5. Final cleanup of db & tree

db$binomial <- factor(db$binomial)
db$iucn.no <- as.numeric(db$iucn.no)

dup.sp <- db$binomial[which(duplicated(db$binomial))]
#View(db)

orphans <- setdiff(levels(tip_order), levels(db$binomial))
tree <- drop.tip(tree, tip = orphans)
db.keep <- intersect(levels(tip_order), levels(db$binomial))
orphans <- setdiff( levels(db$binomial),levels(tip_order))


tip_order <- factor(tree$tip.label)
write.tree(tree, 'data/untrametric.genbank.tre')

originality<-evol.distinct(tree,type="fair.proportion")
tree.k <- tree

fixit <- db$binomial[is.na(db$supregion)]
db$supregion[db$binomial == fixit] <- 'afrotropical'

write.csv(db, 'data/ch.4.data.csv')
