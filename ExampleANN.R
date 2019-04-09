###NOTES
##Variables in dataset
#Quality of Life mocked-up data for illustration purposes only. Variables include:
#IQ (intelligence), 
#SES (socioeconomic status: 1=low, 4=high), 
#Education (ordinal variable: 1=low, 5=high), 
#Urbanicity (interval score), 
#NetworkScore (size of network)
#LifeQualityScore (quality of life score: lower score = lower quality)

##Installing packages
#If asked to restart R, say no.  You may get a warning about versions, but the code should still run correctly.  

###Block 1

## Creating index variable 

# Read the Data
data = read.csv("https://raw.githubusercontent.com/jshutay/TIM-8535/master/QualityOfLife.csv", header=T)

# Random sampling
samplesize = 0.65 * nrow(data)
set.seed(60)
index = sample( seq_len ( nrow ( data ) ), size = samplesize )

# Create training and test set
datatrain = data[ index, ]
datatest = data[ -index, ]


###Block 2

## Scale data for neural network

max = apply(data , 2 , max)
min = apply(data, 2 , min)
scaled = as.data.frame(scale(data, center = min, scale = max - min))


###Block 3

## Fit neural network 

# install library
install.packages("neuralnet")

# load library
library(neuralnet)

# creating training and test set
trainNN = scaled[index , ]
testNN = scaled[-index , ]

# fit neural network - specifying number of hidden layers as 3, linear output = True
set.seed(2)
NN = neuralnet(LifeQualityScore ~ IQ + SES + Education + Urbanicity + NetworkScore, trainNN, hidden = 3 , linear.output = T )

# plot neural network
plot(NN)


###Block 4

## Prediction using neural network

predict_testNN = compute(NN, testNN[,c(1:5)])
predict_testNN = (predict_testNN$net.result * (max(data$LifeQualityScore) - min(data$LifeQualityScore))) + min(data$LifeQualityScore)

plot(datatest$LifeQualityScore, predict_testNN, col='blue', pch=16, ylab = "predicted score NN", xlab = "real score")

abline(0,1)

# Calculate Root Mean Square Error (RMSE)
RMSE.NN = (sum((datatest$LifeQualityScore - predict_testNN)^2) / nrow(datatest)) ^ 0.5


###Block 5

## Cross validation of neural network model

# install relevant libraries
install.packages("boot")
install.packages("plyr")

# Load libraries
library(boot)
library(plyr)

# Initialize variables
set.seed(60)
k = 100
RMSE.NN = NULL

List = list( )

# Fit neural network model within nested for loop
for(j in 20:60){
  for (i in 1:k) {
    index = sample(1:nrow(data),j )
    
    trainNN = scaled[index,]
    testNN = scaled[-index,]
    datatest = data[-index,]
    
    NN = neuralnet(LifeQualityScore ~ IQ + SES + Education + Urbanicity + NetworkScore, trainNN, hidden = 3, linear.output= T)
    predict_testNN = compute(NN,testNN[,c(1:5)])
    predict_testNN = (predict_testNN$net.result*(max(data$LifeQualityScore)-min(data$LifeQualityScore)))+min(data$LifeQualityScore)
    
    RMSE.NN [i]<- (sum((datatest$LifeQualityScore - predict_testNN)^2)/nrow(datatest))^0.5
  }
  List[[j]] = RMSE.NN
}

Matrix.RMSE = do.call(cbind, List)


###Block 6

## Prepare boxplot
boxplot(Matrix.RMSE[,41], ylab = "RMSE", main = "RMSE BoxPlot (length of traning set = 60)")


###Block 7

## Variation of median RMSE 
install.packages("matrixStats")
library(matrixStats)

med = colMedians(Matrix.RMSE)

X = seq(20,60)

plot (med~X, type = "l", xlab = "length of training set", ylab = "median RMSE", main = "Variation of RMSE with length of training set")
