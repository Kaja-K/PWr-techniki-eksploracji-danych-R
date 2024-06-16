library(ISLR)
library(rpart) #for fitting decision trees
library(rpart.plot) #for plotting decision trees
set.seed(12)
tree <- rpart(Sales~., data=Carseats, 
              control=rpart.control(cp=.0001))
prp(tree,
    faclen=0, #use full names for factor labels
    extra=1, #display number of obs. for each terminal node
    roundint=F, #don't round to integers in output
    digits=5) #display 5 decimal places in output
#view results
printcp(tree)
plotcp(tree)
#identify best cp value to use
best <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
best
pruned_tree <- prune(tree, cp=best)
prp(pruned_tree,
    faclen=0, #use full names for factor labels
    extra=1, #display number of obs. for each terminal node
    roundint=F, #don't round to integers in output
    digits=5) #display 5 decimal places in output
