####First, I need to set my working directory and import the data from the csv.####
  setwd("C:/Users/happy/Library of Michael/Western/Math 510 - Mathematical Modeling/Projects/Project 3")
  dataset = read.csv("Workable Dataset.csv")
  functionInput = 1480*(dataset$ST * dataset$SS)/(dataset$Pr)
  linearFunctionData = dataset$PAR * functionInput
  squareFunctionData = dataset$PAR * functionInput^2
  cubeFunctionData = dataset$PAR * functionInput^3
  fourFunctionData = dataset$PAR * functionInput^4
  rootFunctionData = dataset$PAR * functionInput^(1/2)
  exponentialFunctionData = dataset$PAR * exp(functionInput)
  logarithmFunctionData = dataset$PAR * log(functionInput)
  reciprocalFunctionData = dataset$PAR * (1/functionInput)
  
####Linear Regression####
  linearModel <- lm(dataset$NEE ~ 0 + linearFunctionData)
  summary(linearModel)

  par(mfrow=c(1,1))
  plot(linearFunctionData, dataset$NEE, 
       main="Regression of NEE = PAR * ((cp) (SS) (ST)/ (PA))", 
       sub="Determining if the function is linear",
       xlab="PAR * ((cp) (SS) (ST)/ (PA))", ylab="NEE",
       col.main="red", col.lab="blue", col.sub="black")
  abline(linearModel)

  par(mfrow=c(2,2))
  plot(linearModel, col.main="black", col.lab="blue")
  title(main = "Diagnostic Plots for linear model", line = -1.5, 
        outer = TRUE, col.main="red")
  
####Square Regression####
  squareModel <- lm(dataset$NEE ~ 0 + squareFunctionData)
  summary(squareModel)
  
  par(mfrow=c(1,1))
  plot(squareFunctionData, dataset$NEE, 
       main="Regression of NEE = PAR * ((cp) (SS) (ST)/ (PA))^2", 
       sub="Determining if the function is quadratic",
       xlab="PAR * ((cp) (SS) (ST)/ (PA))^2", ylab="NEE",
       col.main="red", col.lab="blue", col.sub="black")
  abline(squareModel)
  
  par(mfrow=c(2,2))
  plot(squareModel, col.main="black", col.lab="blue")
  title(main = "Diagnostic Plots for quadratic model", line = -1.5, 
        outer = TRUE, col.main="red")
  
####Logarithm Regression####
  logarithmModel <- lm(dataset$NEE ~ 0 + logarithmFunctionData)
  summary(logarithmModel)
  
  par(mfrow=c(1,1))
  plot(logarithmFunctionData, dataset$NEE, 
       main="Regression of NEE = PAR * log((cp) (SS) (ST)/ (PA))", 
       sub="Determining if the function is logarithmic",
       xlab="PAR * ln((cp) (SS) (ST)/ (PA))", ylab="NEE",
       col.main="red", col.lab="blue", col.sub="black")
  abline(logarithmModel)
  
  par(mfrow=c(2,2))
  plot(logarithmModel, col.main="black", col.lab="blue")
  title(main = "Diagnostic Plots for logarithmic model", line = -1.5, 
        outer = TRUE, col.main="red")
  
####Root Regression####
  rootModel <- lm(dataset$NEE ~ 0 + rootFunctionData)
  summary(rootModel)
  
  par(mfrow=c(1,1))
  plot(rootFunctionData, dataset$NEE,
       main="Regression of NEE = PAR * ((cp) (SS) (ST)/ (PA))^(1/2)", 
       sub="Determining if the function is a square root",
       xlab="PAR * ((cp) (SS) (ST)/ (PA))^(1/2)", ylab="NEE",
       col.main="red", col.lab="blue", col.sub="black")
  abline(rootModel)
  
  par(mfrow=c(2,2))
  plot(rootModel, col.main="black", col.lab="blue")
  title(main = "Diagnostic Plots for square root model", line = -1.5, 
        outer = TRUE, col.main="red")
  