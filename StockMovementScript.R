install.packages('TTR')
install.packages('quantmod')
install.packages('timeDate')
install.packages('dplyr')
install.packages('rlm')
install.packages('FinancialInstrument')

library('TTR')
library('quantmod')
library('timeDate')
library('caret')
library('e1071')
library('rlm')


####### Getting Yahoo Data of IBM and S&P 500 ##################

s1<-getYahooData("IBM",19630101,20160408)

s10<-getYahooData("^GSPC",19630101,20160408)

###### Calculate Price Relative Index ##################

PriceRelative<-matrix(0,nrow(s1))

for(i in 1:nrow(PriceRelative))
{
	PriceRelative[i,1]<-s1[i,"Close"]/s10[i,"Close"]

}

##### Calculate Price Relative Change for 5 day ##################

PerChgPriRel<-matrix(0,nrow(s1))

for(i in 1:nrow(PerChgPriRel))
{
	PerChgPriRel[i,1]<-PriceRelative[i+5,1]-PriceRelative[i,1]
	
	if(PerChgPriRel[i,1]>=0)
	{
		PerChgPriRel[i,1]<-1
	}
	else if(PerChgPriRel[i,1]<0)
	{
		PerChgPriRel[i,1]<-0
	}
}


#Calculate Price Relative Change for 30 day

PerChgPriRel30<-matrix(0,nrow(s1))

for(i in 1:nrow(PerChgPriRel30))
{
	PerChgPriRel30[i,1]<-PriceRelative[i+30,1]-PriceRelative[i,1]
	
	if(PerChgPriRel30[i,1]>=0)
	{
		PerChgPriRel30[i,1]<-1
	}
	else if(PerChgPriRel30[i,1]<0)
	{
		PerChgPriRel30[i,1]<-0
	}
}


###### Calculating other features for training set #############3 

price <- s1[,"Close"]
# Default case
rsi <- RSI(price)


stochWPR <- WPR(s1[,c("High","Low","Close")])


macd <- MACD( s1[,"Close"] )
macd <- macd[,"macd"]

ema.20 <- EMA(s1[,"Close"], 20)
sma.20 <- SMA(s1[,"Close"], 20)
sma.50 <- SMA(s1[,"Close"], 50)
dema.20 <- DEMA(s1[,"Close"], 20)

######### for PerChgPriRel 5


colnames(PerChgPriRel)<-c("PerChgPriRel")

PerChgPriRel<-as.data.frame(PerChgPriRel)

####### for PerChgPriRel 30

colnames(PerChgPriRel30)<-c("PerChgPriRel30")

PerChgPriRel30<-as.data.frame(PerChgPriRel30)



######## s40 is Final Training Set for 5 day ##############


s40<-cbind(PerChgPriRel[,1],rsi[,1],macd[,1],ema.20[,1],sma.20[,1],sma.50[,1],dema.20[,1])

colnames(s40)<-c("PerChgPriRel","rsi","macd","ema.20","sma.20","sma.50","dema.20")
s40<-as.data.frame(s40)


s40<-s40[1:13409,]

s40<-s40[40:13409,]


####### s_ibm_final30 is Final Training Set for 30 day ########

s_ibm_final30<-cbind(PerChgPriRel30[,1],rsi[,1],macd[,1],ema.20[,1],sma.20[,1],sma.50[,1],dema.20[,1])


colnames(s_ibm_final30)<-c("PerChgPriRel30","rsi1","macd1","ema1.20","sma1.20","sma1.50","dema1.20")





s_ibm_final30<-as.data.frame(s_ibm_final30)

s_ibm_final30<-s_ibm_final30[1:13409,]


s_ibm_final30<-s_ibm_final30[40:13409,]





######## Logistic Regression for 5 day ##################

stock_h<-glm(PerChgPriRel~.,s40,family = binomial)

summary(stock_h)



stock_training<-s40[1:9359,]

stock_test<-s40[9360:13370,]


stock_training<-s40[1:700,]

stock_test<-s40[701:1000,]

stock_pred<-predict(stock_h,stock_test)

if(stock_pred[4011]==0){
	print("Stock Value will decrease in next 5 days")
	}
if(stock_pred[4011]==1){
	print("Stock Value will increase in next 5 days")
	}


for( i in 1:length(stock_pred))
{
    if(stock_pred[i]>=0)
	{
       stock_pred[i]<-1
	}    
	else if(stock_pred[i]<0)
	{
	 stock_pred[i]<-0
 	}
}
stock_t<-table(stock_pred,stock_test[,"PerChgPriRel"])
confusionMatrix(t(stock_t))

################ Logistic Regression for 30 day ################

stock_h_ibm30<-glm(PerChgPriRel30~.,s_ibm_final30,family = binomial)

summary(stock_h_ibm30)



stock_training30<-s_ibm_final30[1:9359,]

stock_test30<-s_ibm_final30[9360:13370,]


stock_training30_small<-s_ibm_final30[1:700,]

stock_test30_small<-s_ibm_final30[701:1000,]

stock_pred30<-predict(stock_h_ibm30,stock_test30)

stock_pred30_small<-predict(stock_h_ibm30,stock_test30_small)

#for large

for( i in 1:length(stock_pred30))
{
    if(stock_pred30[i]>=0)
	{
       stock_pred30[i]<-1
	}    
	else if(stock_pred30[i]<0)
	{
	 stock_pred30[i]<-0
 	}
}
stock_t30<-table(stock_pred30,stock_test30[,"PerChgPriRel30"])
confusionMatrix(t(stock_t30))

#for small


for( i in 1:length(stock_pred30_small))
{
    if(stock_pred30_small[i]>=0)
	{
       stock_pred30_small[i]<-1
	}    
	else if(stock_pred30_small[i]<0)
	{
	 stock_pred30_small[i]<-0
 	}
}
stock_t30_small<-table(stock_pred30_small,stock_test30_small[,"PerChgPriRel30"])
confusionMatrix(t(stock_t30_small))



###### SVM for PerChgPriRel ########

stock_training<-s40[1:9308,]

stock_test<-s40[9308:13298,]




####### mini attempt on smaller data (Best) ######

stock_training<-s40[1:700,]

stock_test<-s40[701:1000,]

stock_svm<- svm(PerChgPriRel~.,s40,family=binomial)

stock_pred<-predict(stock_svm,stock_test,probability = TRUE)

stock_test_pred<- round(stock_pred)




stock_t1<-table(stock_test_pred,stock_test[,"PerChgPriRel"])
confusionMatrix(t(stock_t1))

############################ IBM Naive Bayes PerChgPriRel  ##############

stock_training<-s40[1:9308,]

stock_test<-s40[9308:13298,]


stock_nb<- naiveBayes(PerChgPriRel~.,s40)

summary(stock_nb)

stock_pred<-predict(stock_nb,stock_test[,1])

head(stock_pred)

stock_pred<-round(stock_pred)

#for( i in 1:length(stock_pred))
#{
#    if(stock_pred[i]>=0.5)
#       stock_pred[i]<-1
#    else stock_pred[i]<-0
# }

stock_t<-table(stock_pred,stock_test[,1])




####################### RLM PerChgPriRel ########

install.packages("MASS")
library("MASS")

stock_rlm<- rlm(PerChgPriRel ~ rsi + macd + ema.20 + sma.20 + sma.50 + dema.20)

t_pred2<-predict(stock_rlm,stock_test[,1],type="response")

t_rpred2<-round(t_pred2)

xtab2<-table(t_rpred2,stock_test[,1])
confusionMatrix(xtab2)



########## Calculating 5 day ahead close value

close_new<-matrix(0,13342,1)
priceChangeNew<-matrix(0,13342,1)

for(i in 1:nrow(s1))
{
	close_new[i,1]<- s1[,"Close"][i+5]

}

priceChangeNew<- close_new - s1[,"Close"]

for( i in 1:length(priceChangeNew))
 {
    if(priceChangeNew[i]>=0)
	{
       priceChangeNew[i]<-1
	}    
	else if( priceChangeNew[i]<0)
	{
	priceChangeNew[i]<-0
 	}
}

priceChangeNew<-as.data.frame(priceChangeNew)

colnames(priceChangeNew)<-c("priceChangeNew")

s4<-cbind(macd[,1],ema.20[,1],sma.20[,1],sma.50[,1],dema.20[,1])

colnames(s4)<-c("macd","ema.20","sma.20","dema.20")

s4<-as.data.frame(s4)



s4<-cbind(s4,priceChangeNew)

s4<-s4[1:13337,]

s4<-s4[40:13337,]


stock_h<-glm(priceChangeNew~.,s4,family = binomial)

summary(stock_h)




######################### Attempt 1: glm IBM ###########################

s5<-s4

s5 <- s4[sample(nrow(s4)),]


stock_training<-s5[1:9308,]

stock_test<-s5[9308:13298,]

stock_pred<-predict(stock_h,stock_test)

for( i in 1:length(stock_pred))
{
    if(stock_pred[i]>=0.5)
	{
       stock_pred[i]<-1
	}    
	else if(stock_pred[i]<0.5)
	{
	 stock_pred[i]<-0
 	}
}
stock_t<-table(stock_pred,stock_test[,"priceChangeNew"])
confusionMatrix(t(stock_t))

########################## Attempt 2: glm IBM #############

stock_training<-s4[1:700,]

stock_test<-s4[701:1000,]

stock_pred<-predict(stock_h,stock_test)

for( i in 1:length(stock_pred))
{
    if(stock_pred[i]>=0.5)
	{
       stock_pred[i]<-1
	}    
	else if(stock_pred[i]<0.5)
	{
	 stock_pred[i]<-0
 	}
}
stock_t<-table(stock_pred,stock_test[,"priceChangeNew"])

confusionMatrix(t(stock_t))


########################## Attempt 3: IBM SVM Failed #########################


stock_training<-s4[1:9308,]

stock_test<-s4[9308:13298,]




####### mini attempt on smaller data ( failed) ######

stock_training<-s4[1:700,]

stock_test<-s4[701:1000,]

stock_svm<- svm(priceChangeNew~.,s4,family=binomial,type='C',kernel='sigmoid',probability = TRUE)

stock_pred<-predict(stock_svm,stock_test,probability = TRUE)

stock_test_pred<- attr(stock_pred, "probabilities")[,1]




stock_t1<-table(stock_rpred1,stock_test[,1])
confusionMatrix(stock_t1)



############################ Attempt 4: IBM Naive Bayes ##############

stock_training<-s4[1:9308,]

stock_test<-s4[9308:13298,]


stock_nb<- naiveBayes(priceChangeNew~.,s4)

summary(stock_nb)

stock_pred<-predict(stock_nb,stock_test)

head(stock_pred)

stock_pred<-round(stock_pred)

#for( i in 1:length(stock_pred))
#{
#    if(stock_pred[i]>=0.5)
#       stock_pred[i]<-1
#    else stock_pred[i]<-0
# }

stock_t<-table(stock_pred,stock_test[,1])




####################################
length(s1[,1])

stock_training<-s4[1:176,]

stock_test<-s4[177:252,]

stock_h1<-glm(Close~.,data=stock_training)

summary(stock_h1)

stock_pred<-predict(stock_h1,stock_test)

stock_pred<-as.data.frame(stock_pred)

head(stock_pred)

stock_pred<-round(stock_pred)

#for( i in 1:length(stock_pred))
#{
#    if(stock_pred[i]>=0.5)
#       stock_pred[i]<-1
#    else stock_pred[i]<-0
# }

stock_t<-table(stock_pred,stock_test[,1])

################################ SVM #############################################


library('e1071')

stock_svm<- svm(Close~.,data=stock_training,family=binomial,type='C',kernel='sigmoid',probability = TRUE)

stock_pred1<-predict(stock_svm,stock_test,probability = TRUE)

stock_test_pred <- attr(stock_pred1, "probabilities")[,1]


stock_rpred1<-round(stock_test_pred )

stock_t1<-table(stock_rpred1,stock_test[,1])
confusionMatrix(stock_t1)

########################### Naive Bayes ############################

stock_nb<- naiveBayes(Close~.,data=stock_training)

summary(stock_h1)

stock_pred<-predict(stock_h1,stock_test)

head(stock_pred)

stock_pred<-round(stock_pred)

#for( i in 1:length(stock_pred))
#{
#    if(stock_pred[i]>=0.5)
#       stock_pred[i]<-1
#    else stock_pred[i]<-0
# }

stock_t<-table(stock_pred,stock_test[,1])

################################ Percentage Change as parameter ################

library('TTR')
library('quantmod')
library('timeDate')


p1<-as.matrix(s1)

s2<-getYahooData("IBM",19800116,20160101)

p1_final<-as.matrix(s2)

p2<-p1[6346:9081,]

#percent1 is training data

#first column of percent1 is to be predicted

percent1<-matrix(0,6346,10)

for( i in 1:nrow(percent1))
{
	temp<-i+10
	for( j in 1:ncol(percent1))
	{
		perc = ((p1[temp,4]-p1[temp-1,4])/p1[temp-1,4])*100		
		
		percent1[i,j]<-perc
		
		temp<-temp-1
	
	}
	
}

for( i in 1:nrow(percent1))
{
   for(j in 1:ncol(percent1))
    {	
	if(percent1[i,j]<0)
	{
		percent1[i,j]<-0
	}
	else if(percent1[i,j]>=0)
	{
		percent1[i,j]<-1
	}
     }
}


#percent2 is test data

percent2<-matrix(0,2726,10)

for( i in 1:nrow(percent2))
{
	temp<-i+10
	for( j in 1:ncol(percent2))
	{
		perc = ((p2[temp,4]-p2[temp-1,4])/p2[temp-1,4])*100		
		
		percent2[i,j]<-perc
		
		temp<-temp-1
	
	}
	
}


#Modeling

percent1<-as.data.frame(percent1)

percent2<-as.data.frame(percent2)


perc_hypo<-glm(percent1$V1~.,percent1,family = binomial)


################# Function for Predicting the Stock Movement ########



s_ibm_today<-getYahooData("IBM",20160408,20160408)

s_sp_today<-getYahooData("^GSPC",20160408,20160408)

#PriceRelative1

PriceRelative1-matrix(0,nrow(s_ibm_today))

for(i in 1:nrow(PriceRelative1)
{
	PriceRelative1[i,1]<-s_ibm_today[i,"Close"]/s_sp_today[i,"Close"]

}


#Calculate Price Relative Change for 5 day

PerChgPriRel5<-matrix(0,nrow(s_ibm_today))

for(i in 1:nrow(PerChgPriRel5)
{
	PerChgPriRel5[i,1]<-PriceRelative1[i+5,1]-PriceRelative1[i,1]
	
	if(PerChgPriRel5[i,1]>=0)
	{
		PerChgPriRel5[i,1]<-1
	}
	else if(PerChgPriRel5[i,1]<0)
	{
		PerChgPriRel5[i,1]<-0
	}
}

#Calculate Price Relative Change for 30 day

PerChgPriRel10<-matrix(0,nrow(s_ibm_today))

for(i in 1:nrow(PerChgPriRel10)
{
	PerChgPriRel10[i,1]<-PriceRelative1[i+30,1]-PriceRelative1[i,1]
	
	if(PerChgPriRel30[i,1]>=0)
	{
		PerChgPriRel30[i,1]<-1
	}
	else if(PerChgPriRel30[i,1]<0)
	{
		PerChgPriRel30[i,1]<-0
	}
}

#Calculating other features for training set 

price1 <- s_ibm_today[,"Close"]
# Default case
rsi1 <- RSI(price1)


macd1 <- MACD( s_ibm_today[,"Close"] )
macd1 <- macd1[,"macd"]

ema1.20 <- EMA(s_ibm_today[,"Close"], 20)
sma1.20 <- SMA(s_ibm_today[,"Close"], 20)
sma1.50 <- SMA(s_ibm_today[,"Close"], 50)
dema1.20 <- DEMA(s_ibm_today[,"Close"], 20)




s_ibm_final1<-cbind(rsi1[,1],macd1[,1],ema1.20[,1],sma1.20[,1],sma1.50[,1],dema1.20[,1])


colnames(s_ibm_final1)<-c("rsi1","macd1","ema1.20","sma1.20","sma1.50","dema1.20")

stock_pred<-predict(stock_h1,s_ibm_final1)


#for PerChgPriRel


colnames(PerChgPriRel5)<-c("PerChgPriRel5")

PerChgPriRel5<-as.data.frame(PerChgPriRel5)

colnames(PerChgPriRel30)<-c("PerChgPriRel30")

PerChgPriRe30<-as.data.frame(PerChgPriRe30)



#s_ibm_final is Final Training Set


s_ibm_final5<-cbind(PerChgPriRel5[,1],rsi1[,1],macd1[,1],ema1.20[,1],sma1.20[,1],sma1.50[,1],dema1.20[,1])

s_ibm_final30<-cbind(PerChgPriRel30[,1],rsi1[,1],macd1[,1],ema1.20[,1],sma1.20[,1],sma1.50[,1],dema1.20[,1])



colnames(s_ibm_final5)<-c("PerChgPriRel5","rsi1","macd1","ema1.20","sma1.20","sma1.50","dema1.20")


colnames(s_ibm_final30)<-c("PerChgPriRel30","rsi1","macd1","ema1.20","sma1.20","sma1.50","dema1.20")




s_ibm_final5<-as.data.frame(s_ibm_final5)
s_ibm_final30<-as.data.frame(s_ibm_final30)

s_ibm_final<-s_ibm_final[1:13337,]

s_ibm_final<-s_ibm_final[40:13337,]

s_ibm_final5<-s_ibm_final[1:13337,]

s_ibm_final30<-s_ibm_final[40:13337,]


#Logistic Regression

stock_h_ibm5<-glm(PerChgPriRel5~.,s_ibm_final5,family = binomial)

stock_h_ibm30<-glm(PerChgPriRel30~.,s_ibm_final30,family = binomial)

summary(stock_h_ibm5)

summary(stock_h_ibm30)





stock_training5<-s_ibm_final5[1:9308,]

stock_test5<-s_ibm_final5[9308:13298,]


stock_training5<-s_ibm_final5[1:700,]

stock_test5<-s_ibm_final5[701:1000,]


stock_training30<-s_ibm_final30[1:9308,]

stock_test30<-s_ibm_final30[9308:13298,]


stock_training30<-s_ibm_final30[1:700,]

stock_test30<-s_ibm_final30[701:1000,]




stock_pred<-predict(stock_h,stock_test)

for( i in 1:length(stock_pred))
{
    if(stock_pred[i]>=0)
	{
       stock_pred[i]<-1
	}    
	else if(stock_pred[i]<0)
	{
	 stock_pred[i]<-0
 	}
}
stock_t<-table(stock_pred,stock_test[,"PerChgPriRel"])
confusionMatrix(t(stock_t))


###### SVM for PerChgPriRel ########

stock_training<-s40[1:9308,]

stock_test<-s40[9308:13298,]




####### mini attempt on smaller data (Best) ######

stock_training<-s40[1:700,]

stock_test<-s40[701:1000,]

stock_svm<- svm(PerChgPriRel~.,s40,family=binomial)

stock_pred<-predict(stock_svm,stock_test,probability = TRUE)

stock_test_pred<- round(stock_pred)




stock_t1<-table(stock_test_pred,stock_test[,"PerChgPriRel"])
confusionMatrix(t(stock_t1))


