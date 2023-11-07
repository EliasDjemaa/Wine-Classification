
library(neuralnet)

#XOR gate input data
trainin = rbind(c(-1,-1), c(1,-1), c(-1,1), c(1,1));
plot(trainin)

#XOR gate output data
trainout = rbind(0, 1, 1, 0);

#Combined OR gate data
ORdat=cbind(trainout,trainin)

# fit neural network with no hidden layers
set.seed(2)
# adding 2 hidden layers, and 3 neuons each
NN = neuralnet(ORdat[,1]~., ORdat[,-1], hidden = c(5,5) , threshold = 0.001,
               stepmax = 1e+05, linear.output = FALSE)
#visualise the NN
plot(NN) 

NN$weights

testin= rbind(c(1,1))
predict_testNN = compute(NN, testin)

predict_testNN$net.result

predict_out = as.numeric(predict_testNN$net.result>0.5)
print(predict_out)

#set up the input sequence
testin=rbind(c(1,1),c(1,-1),c(-1,1), c(-1,-1))
predict_testNN = compute(NN, testin)
predict_testNN$neurons
predict_testNN$net.result
predict_out = as.numeric(predict_testNN$net.result>0.5)
predict_out