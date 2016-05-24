complete<-function(directory, id=1:332)
{
        setwd(directory)
        
        data1<-data.frame()
        
        for(i in id)
        {
                d1<-data.frame()
                
                if(i<10)
                {
                        d1<-rbind(d1,read.csv(paste("00",as.character(i),".csv",sep='')))
                }
                else 
                        if(i>=10 & i<100)
                        {
                                d1<-rbind(d1,read.csv(paste("0",as.character(i),".csv",sep='')))
                        }
                else
                        if(i>=100)
                        {
                                d1<-rbind(d1,read.csv(paste(as.character(i),".csv",sep='')))
                        }
                
                a<-complete.cases(d1)
                data1<-rbind(data1,c(i, sum(a)))  
                names(data1)=c("id","nobs")
                
                rm(d1)
                rm(a)
        }
        
       
       
       return(data1)
       
       setwd("..")
        
}

complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)
complete("specdata", 1)

abc<-complete("specdata",1:332)
setwd("..")


corr<-function(directory,threshold=0)
{
        setwd(directory)
        final<-vector()
        
        if(threshold==0)
        {threshold=1}
        
        
        for(i in 1:332)
        {
                
                
                if(i<10 & abc[i,2]>=threshold)
                {
                        d1<-read.csv(paste("00",as.character(i),".csv",sep=''))
                        cr1<-cor(d1[,2],d1[,3],use="complete")
                        final<-c(final,cr1)
                }
                else 
                        if(i>=10 & i<100 & abc[i,2]>=threshold)
                        {
                                d1<-read.csv(paste("0",as.character(i),".csv",sep=''))
                                cr1<-cor(d1[,2],d1[,3],use="complete")
                                final<-c(final,cr1)
                        }
                else
                        if(i>=100 & abc[i,2]>=threshold)
                        {
                                d1<-read.csv(paste(as.character(i),".csv",sep=''))
                                cr1<-cor(d1[,2],d1[,3],use="complete")
                                final<-c(final,cr1)
                        }
                
        }
        is.null(final)
                final=as.numeric(final)
        return(final)
        setwd("..")
}

cr <- corr("specdata", 150)
head(cr)
summary(cr)

cr1<-cor(1,2)
cr1

cr <- corr("specdata", 400)
head(cr)
summary(cr)

cr <- corr("specdata", 5000)
summary(cr)
length(cr)

cr <- corr("specdata")
summary(cr)
length(cr)

getwd()
