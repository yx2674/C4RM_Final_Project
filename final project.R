library(ggplot2)
library(lubridate)
install.packages("ggpubr")
library("ggpubr")
dataBase <- read.csv("https://raw.githubusercontent.com/yx2674/C4RM_Final_Project/main/Updated%20Covid%20%26%20Industry%20Index.csv")

###Draw the graph to get an overall idea
ggplot(data=dataBase,mapping=aes(x=TotalCases,y=VNQ))+geom_line()
ggplot(data=dataBase,mapping=aes(x=TotalCases,y=IHI))+geom_line()
ggplot(data=dataBase,mapping=aes(x=TotalCases,y=VHT))+geom_line()
ggplot(data=dataBase,mapping=aes(x=TotalCases,y=XLI))+geom_line()
ggplot(data=dataBase,mapping=aes(x=TotalCases,y=XLK))+geom_line()
ggplot(data=dataBase,mapping=aes(x=TotalCases,y=IYE))+geom_line()
ggplot(data=dataBase,mapping=aes(x=TotalCases,y=XLY))+geom_line()
ggplot(data=dataBase,mapping=aes(x=TotalCases,y=XLP))+geom_line()

###Create lm model
VNQmodel=lm(data=dataBase,VNQ ~ TotalCases)
summary(VNQmodel)
IHImodel=lm(data=dataBase,IHI ~ TotalCases)
summary(IHImodel)
VHTmodel=lm(data=dataBase,VHT ~ TotalCases)
summary(VHTmodel)
XLImodel=lm(data=dataBase,XLI ~ TotalCases)
summary(XLImodel)
XLKmodel=lm(data=dataBase,XLK ~ TotalCases)
summary(XLKmodel)
IYEmodel=lm(data=dataBase,IYE ~ TotalCases)
summary(IYEmodel)
XLYmodel=lm(data=dataBase,XLY ~ TotalCases)
summary(XLYmodel)
XLPmodel=lm(data=dataBase,XLP ~ TotalCases)
summary(XLPmodel)

###Create a function to first create lm models for different dependent variables and find the greatest R squared value.
FindGreastedRsquared <- function(df,independentvariable){
  modelset=c()
  output=c()
  for(name in names(df)[2:length(names(df))]){
    if(name != independentvariable){
      myformula=paste(name,'~',independentvariable)
      mymodel=lm(myformula,data=df)
      modelset[name]=summary(mymodel)$adj.r.squared
    }
    for(i in 1:length(modelset)){
      output[i]=modelset[[i]]
    }
  }
  max=max(output)
  location=which(modelset==max)
  index=modelset[location]
  return(index)
}

FindGreastedRsquared(dataBase,'TotalCases')

###Create a function to calculate pearson correlations and choose the max value
ChooseMaxCor <- function(df,independentvariable){
  correlation=c()
  output=c()
  for(name in names(df)[2:length(names(df))]){
    if(name != independentvariable){
      independentdata=df[independentvariable]
      dependentdata=df[name]
      correlation[name]=cor(dependentdata,independentdata,method = c("pearson"))
    }
  }
  for(i in 1:length(correlation)){
    output[i]=abs(correlation[[i]])
  }
  max=max(output)
  location=which(abs(correlation)==max)
  index=correlation[location]
  return(index)
}

ChooseMaxCor(dataBase,'TotalCases')

### Since XLY has the highest adjusted R square and the largest absolute value of Pearson correlation
### We take a deep look into the relationship between XLY and total covid cases.
XLYmodel=lm(data=dataBase,XLY ~ TotalCases)
summary(XLYmodel)
###For deciding whether there is a significant relationship between the variables in the linear regression model of the data 
###set faithful at .05 significance level.
###As in this result, we can see that the p-value is much less than 0.05, 
###so we reject the null hypothesis that β= 0.
###Hence there is a significant relationship between the variables in the linear regression model of the data set faithful.

ggscatter(dataBase, x = "XLY", y = "TotalCases", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "cases", ylab = "Price($)")
cor(dataBase$XLY,dataBase$TotalCases,method = c("pearson"))


ggplot(data=dataBase,mapping=aes(x=TotalCases,y=XLY))+geom_point()+geom_line(aes(x=TotalCases,y=XLYmodel$fitted.values))
###As conclusion, the data tells us there is a strong relationship exists between XLY and total covid cases number.
###However, there are other factors that may affect the price of XLY and we didn't take them into consideration.
###All we can say is that total covid cases number is one of factors that affect the price of XLY.
###XLY is the fund index that invests in companies that sell nonessential (elastic) goods and services. 
###We speculate the reason is that during the epidemic period, the whole economic environment is under pressure, 
###the elastic goods were the most affected.