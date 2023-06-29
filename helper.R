library(ecm) # forlagpad

# define add_rec_shade function
add_rec_shade<-function(st_date,ed_date,shade_color, y_min, y_max) {
  
  # download NBER recession indicators, peak through trough
  recession<- fredr(series_id = "USRECD",
                    observation_start = as.Date(st_date),
                    observation_end = as.Date(ed_date))
  
  #code 1 for 1st day of recession, -1 for 1st day after it ends
  recession$diff<-recession$value-lagpad(recession$value,k=1)
  
  #drop 1st N.A. value
  recession<-recession[!is.na(recession$diff),] 
  
  #create vector of recession start dates
  recession.start<-recession[recession$diff==1,]$date 
  
  #create vector of recession end dates
  recession.end<-recession[recession$diff==(-1),]$date 
  
  # if there are more dates listed in recession.start than recession.end
  if(length(recession.start)>length(recession.end))
    # then enter system date for last date in recession.end
  {recession.end<-c(recession.end,Sys.Date())} 
  
  # if there are more dates listed in recession.end than recession.start
  if(length(recession.end)>length(recession.start))       
    # then enter the earliest date in recession$date as first date in recession.start  
  {recession.start<-c(min(recession$date),recession.start)} 
  
  # make a dataframe out of recession.start and recession.end
  recs<-as.data.frame(cbind(recession.start,recession.end))
  
  # convert recession.start into a date
  recs$recession.start<-as.Date(
    as.numeric(recs$recession.start),
    origin=as.Date("1970-01-01")) 
  
  # convert recession.end into a date
  recs$recession.end<-as.Date(
    recs$recession.end,
    origin=as.Date("1970-01-01")) 
  
  # if the number of rows in recs > 0
  if(nrow(recs)>0) 
    # draw the rectangle  
  {rec_shade<-geom_rect(data=recs, 
                        # inherit.aes=F overrides default aesthetics
                        inherit.aes=F, 
                        aes(xmin=recession.start, 
                            xmax=recession.end, 
                            ymin=y_min, ymax=y_max), 
                        fill=shade_color, alpha=0.5)
  return(rec_shade)
  }
}