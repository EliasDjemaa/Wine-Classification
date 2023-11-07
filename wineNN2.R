library(neuralnet)
winedata = read.csv("/Users/elias/Downloads/winedata2 (1).csv")
#origindata = read.csv("/Users/elias/Downloads/winedata2 (1).csv")
#comp_data <- origindata [(0.7* nrow(winedata)):nrow(winedata),]
#comp_values = cbind(colInt = origindata$Color.intensity, Hue = origindata$Hue)

#selecting variables colour intensity and hue
winedata = cbind(winClass = winedata$WineClass-1, colInt = winedata$Color.intensity, Hue = winedata$Hue)

#splitting windedata 70-30 for test and training purposes
#70% training data
train_data <- winedata [1:(0.7 * nrow(winedata)),]
#30% test data
test_data <- winedata [(0.7* nrow(winedata)):nrow(winedata),]
testValues = test_data[,c(2,3)]
testClasses = test_data[,1]

#setting seed for reproducability
set.seed(42)

#Running neuralnet command
#taking output variables and  regressing it on the colour intensity and hue
nn1 <- neuralnet(train_data[,1] ~., train_data[,2:3], 
                hidden = c(2), threshold = 0.5, 
               stepmax = 1e+05, linear.output = FALSE)
plot(nn1)
nn1$result.matrix

#prednn3 = predict(nn1, testValues)
#prednn$net.result

nn <- nn1

#prediction and testing
#using test data from two variables to see how the network responds

tsdf = as.data.frame(testValues)
prednn = predict(nn, tsdf)

#prednn = predict(nn, testValues)
#prednn$net.result
n = length(testClasses)

#comparing predicted values to actual values
predict.nn <- ifelse(predict(nn, testValues, type = 'response') > 0.5,1,0)
train.predict.nn <- ifelse(predict(nn, testValues, type = 'response') > 0.5,1,0)
table(train.predict.nn, testClasses)

truepredict = 0
for(column in 1:(length(prednn [,1]))){
  for(row in 1:(length(prednn [1,]))){
    
    tp = (round(prednn[column])==testClasses[column])
    if(tp){
      truepredict = truepredict + 1
    }
  }
}

cat('accuracy: ', truepredict, '/',n)
acc <- truepredict/n
sapply(acc, function(x) eval(parse(text=x)))

#scaling improves the error result or 
