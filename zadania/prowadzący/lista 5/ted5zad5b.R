library(rpart) #for fitting decision trees
library(rpart.plot)
library(DAAG)
set.seed(12)
ran=sample(1:nrow(spam7), 0.7*nrow(spam7))
train=spam7[ran,]
test=spam7[-ran,]
tree <- rpart(yesno ~., data=train, control=rpart.control(cp=.00001))
rpart.plot(tree)
prp(tree,
    faclen=0, #use full names for factor labels
    extra=1, #display number of obs. for each terminal node
    roundint=F, #don't round to integers in output
    digits=5) #display 5 decimal places in output
printcp(tree)
best <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
best
prunedtree <- prune(tree, cp=best)
rpart.plot(prunedtree)
#błąd treningowy
p <- predict(tree, train, type = 'class')
a=table(p, train$yesno)
a
accur=function(x){sum(diag(x))/sum(rowSums(x))*100}
accur(a)
bladtren=100-accur(a)
bladtren

p <- predict(tree, test, type = 'class')
a=table(p, test$yesno)
a
accur=function(x){sum(diag(x))/sum(rowSums(x))*100}
accur(a)
bladtest=100-accur(a)
bladtest