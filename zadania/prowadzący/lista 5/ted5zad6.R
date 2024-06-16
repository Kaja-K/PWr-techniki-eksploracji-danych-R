library(rpart)       # performing regression trees
library(rpart.plot) 
library(modeldata)
library(rsample)
library(ipred)       # bagging
library(caret) #RMSE
library(MLmetrics)
head(ames)
names(ames)
nrow(ames)
set.seed(123)
nrow(ames)
0.7*nrow(ames)
los=sample(1:nrow(ames), 2000)
ames_train=ames[los,]
ames_test=ames[-los,]
ames_split <- initial_split(ames, prop = .7)
ames_train <- training(ames_split)
ames_test  <- testing(ames_split)
tree <- rpart(Sale_Price~., method="anova", data=ames_train, 
              control=rpart.control(cp=.0000000001))
prp(tree,
    faclen=0, #use full names for factor labels
    extra=1, #display number of obs. for each terminal node
    roundint=F, #don't round to integers in output
    digits=5) #display 5 decimal places in output
#view results
printcp(tree)
plotcp(tree)
plot(tree)
text(tree)
#realtive error = 1-R^2 suma(y_i-y^_i)^2/suma(y_i-y^bar)^2
best <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
best
pruned_tree <- prune(tree, cp=best)
prp(pruned_tree,
    faclen=0, #use full names for factor labels
    extra=1, #display number of obs. for each terminal node
    roundint=F, #don't round to integers in output
    digits=5) #

#błędy na zbiorze testowy
predn <- predict(pruned_tree, newdata = ames_test)
RMSE(predn, ames_test$Sale_Price)
MAPE(predn, ames_test$Sale_Price) # funkcja zwraca wartości w ułamkach nie w procentach

#błędy na zbiorze uczącym
predn <- predict(pruned_tree, newdata = ames_train)
RMSE(predn, ames_train$Sale_Price)
sqrt(mean((predn-ames_train$Sale_Price)^2))
MAPE(predn, ames_train$Sale_Price) # funkcja zwraca wartości w ułamkach nie w procentach




optimal_tree <- rpart(
  formula = Sale_Price ~ .,
  data    = ames_train,
  method  = "anova",
  control = list(minsplit = 20, maxdepth = 3, cp = best)
)
rpart.plot(optimal_tree, extra=1, cex=0.5)


prp(optimal_tree,
    faclen=0, #use full names for factor labels
    extra=1, #display number of obs. for each terminal node
    roundint=F, #don't round to integers in output
    digits=5) #

predn <- predict(optimal_tree, newdata = ames_test)
RMSE(predn, ames_test$Sale_Price)
MAPE(predn, ames_test$Sale_Price) # funkcja zwraca wartości w ułamkach nie w procentach


predn <- predict(optimal_tree, newdata = ames_train)
RMSE(predn, ames_train$Sale_Price)
MAPE(predn, ames_train$Sale_Price) # funkcja zwraca wartości w ułamkach nie w procentach


