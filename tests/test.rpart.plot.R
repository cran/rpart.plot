# test.rpart.plot.R
# Check for porting problems by building a few simple models.
# For much more comprehensive tests see rpart.plot\inst\slowtests.
library(rpart.plot)
data(trees)     # anova model
Volume <- rpart(Volume~., data=trees)
print(rpart.rules(Volume, trace=1))
data(ptitanic)  # binomial model
survived <- rpart(survived ~ ., data=ptitanic, method="class")
print(rpart.rules(survived))
data(iris)      # multiclass model
Species <- rpart(Species ~ ., data=iris, method="class")
print(rpart.rules(Species))
