c-matrix(0,nrow = 100,ncol = 5);c #create a 100*5 matrix (100 users, 5 movies from history)
userhistorydf<-data.frame(c); userhistorydf #make a data frame of user history

#1-kids 10-comedy 100-drama 1000-action 10000-horror
#assigning to each user a history of 5 movies watched
userhistorydf[,1]<-sample(c(1,10,100,1000,10000),100,repl=T) #history1
userhistorydf[,2]<-sample(c(1,10,100,1000,10000),100,repl=T) #history2
userhistorydf[,3]<-sample(c(1,10,100,1000,10000),100,repl=T) #history3
userhistorydf[,4]<-sample(c(1,10,100,1000,10000),100,repl=T) #history4
userhistorydf[,5]<-sample(c(1,10,100,1000,10000),100,repl=T) #history5


userhistorydf[,6]<-0; userhistorydf #create a new column for average values
userhistorydf[,6]<-rowMeans(userhistorydf[,1:5]) #store average value of all the hsitories
userhistorydf[,7]<-"Default" #new column with next possible reccomendation

#forloop to classify based on calculated weight

for(x in 1:100)
{
  
  if(userhistorydf[x,6]>0 && userhistorydf[x,6]<=100)
  {
    userhistorydf[x,7]<-sample(c("Kids","Comedy","Drama"),1) #Assigns one out of 3 for 0-100 average
  }
  
  else if(userhistorydf[x,6]>100 && userhistorydf[x,6]<=1000)
    {
      userhistorydf[x,7]<-sample(c("Drama","Action","Comedy"),1) #Assigns one out of 3 for 100-1000
    }
  
  else if(userhistorydf[x,6]>1000 && userhistorydf[x,6]<=10000)
  { 
    userhistorydf[x,7]<-sample(c("Action","Horror"),1) #Assigns one out of 3 for 1000-10000
  }
}

userhistorydf



names(userhistorydf)=c("History1","History2","History3","History4","History5","Means","Recommendation")

write.csv(userhistorydf,file="reccomend1.csv")
