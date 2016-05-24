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
        
       
       
       print(data1)
       
       setwd("..")
        
}

complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)
complete("specdata", 1)

complete("specdata",1:332)


