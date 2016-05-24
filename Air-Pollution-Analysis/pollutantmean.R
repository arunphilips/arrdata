pollutantmean<-function(directory, pollutant, id=1:332) 
{
        setwd(directory)
        d1<-data.frame()
        
        for(i in id)
        {
                
                
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
        }
        
        if(pollutant=="sulfate")
                print(mean(d1[,"sulfate"],na.rm = TRUE))
        else
                if(pollutant=="nitrate")
                        print(mean(d1[,"nitrate"],na.rm=TRUE))
        setwd("..")
        
}




