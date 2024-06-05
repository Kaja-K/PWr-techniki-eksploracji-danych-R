library(rpart) #for fitting decision trees
library(rpart.plot)
library(DAAG)
set.seed(12)
tree <- rpart(yesno ~., data=spam7, parms=list(split="information"),
              control=rpart.control(cp=.0001))
rpart.plot(tree)
prp(tree,
    faclen=0, #use full names for factor labels
    extra=1, #display number of obs. for each terminal node
    roundint=F, #don't roun?d to integers in output
    digits=5) #display 5 decimal places in output
printcp(tree)
best <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
best
prunedtree <- prune(tree, cp=best)
rpart.plot(prunedtree)
prp(prunedtree,
    faclen=0, #use full names for factor labels
    extra=1, #display number of obs. for each terminal node
    roundint=F, #don't round to integers in output
    digits=5) #display 5 decimal places in output