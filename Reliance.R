library('readr')
library('dplyr')
library('lubridate')
library(ggplot2)
library('quantmod') #To Get the stock data and analyse
df <- read_csv("Downloads/Quote-Equity-RELIANCE-EQ-01-01-2021-to-31-12-2022 - Quote-Equity-RELIANCE-EQ-01-01-2021-to-31-12-2022.csv.csv")
colnames(df)[1]<- "old_date"
df$year <- year(dmy(df$old_date))
df$date <- dmy(df$old_date)
df$week <- lubridate::week(df$date)
df$wday <- wday(df$date, week_start=1,label=TRUE)
df$pct <- round(((df$close -df$`PREV. CLOSE` )/df$`PREV. CLOSE`)*100,3)
df$quarter <- quarter(df$date)

sd <- sd(df$close, na.rm = FALSE)

df$MA2 <- TTR::SMA(df$close, n = 2)
df$MA5 <-  SMA(df$close,n=5)
df$MA7 <-  SMA(df$close,n=7)
df$MA15 <-  SMA(df$close,n=15)
df$MA20 <-  SMA(df$close,n=20)
df$MA30 <-  SMA(df$close,n=30)

p<- ggplot(data=df, aes(x=date))
p<-p+geom_line(aes(y=close), color= "black")
p<-p+geom_line(aes(y=MA2), color= "blue")
p<-p+geom_line(aes(y=MA5), color= "brown")
p<-p+geom_line(aes(y=MA7), color= "grey")
p<-p+geom_line(aes(y=MA15), color= "red")
p<-p+geom_line(aes(y=MA20), color= "orange")
p<-p+geom_line(aes(y=MA30), color= "green")+scale_y_continuous()
p <- p +  theme_minimal()
p+theme(legend.position = "right")
ggplotly(p)

x<- ggplot(data=df,aes(x=date,y=close))+geom_line(aes(y=close))

df$MA2upper <- df$MA2+sd
df$MA2lower <- df$MA2-sd
ma2<- x+geom_line(aes(y=df$MA2upper),color="red")
ma2<-ma2+geom_line(aes(y=df$MA2lower),color="green")
ggplotly(ma2)

df$MA5upper <- df$MA5+sd
df$MA5lower <- df$MA5-sd
ma5<- x+geom_line(aes(y=df$MA5upper),color="red")
ma5<-ma5+geom_line(aes(y=df$MA5lower),color="green")
ggplotly(ma5)

df$MA7upper <- df$MA7+sd
df$MA7lower <- df$MA7-sd
ma7<- x+geom_line(aes(y=df$MA7upper),color="red")
ma7<-ma7+geom_line(aes(y=df$MA7lower),color="green")
ggplotly(ma7)

df$MA15upper <- df$MA15+sd
df$MA15lower <- df$MA15-sd
ma15<- x+geom_line(aes(y=df$MA15upper),color="red")
ma15<-ma15+geom_line(aes(y=df$MA15lower),color="green")
ggplotly(ma15)

df$MA20upper <- df$MA20+sd
df$MA20lower <- df$MA20-sd
ma20<- x+geom_line(aes(y=df$MA20upper),color="red")
ma20<-ma20+geom_line(aes(y=df$MA20lower),color="green")
ggplotly(ma20)

df$MA30upper <- df$MA30+sd
df$MA30lower <- df$MA30-sd
ma30<- x+geom_line(aes(y=df$MA30upper),color="red")
ma30<-ma30+geom_line(aes(y=df$MA30lower),color="green")
ggplotly(ma30)

df$ocdiff <- -df$OPEN+df$close
df$candle <- case_when(df$ocdiff >=0  ~ "Green", TRUE~ "Red")

df$candlestrength <- case_when( df$ocdiff >=100 ~ "   Very Super Strong Green",
                                df$ocdiff >=75 ~ " Super Strong Green",
                                df$ocdiff >=50 ~ " Strong Green",
                                df$ocdiff >=25 ~ " Mid Green",
                                df$ocdiff >=0 ~ " Green",
                                df$ocdiff <=-100 ~ "  Very Super Strong Red",
                                df$ocdiff <=-75 ~ " Super Strong Red",
                                df$ocdiff <=-50 ~ " Strong Red",
                                df$ocdiff <=-25 ~ " Mid Red",
                                df$ocdiff <= 0 ~ "Red",
                       TRUE~ "Cool")

df %>% 
  count(candlestrength)

df$ocdiffMA <- TTR::SMA(df$ocdiff, n = 7)
ggplot(data=df,aes(x=date,y=ocdiff))+geom_line(aes(y=ocdiff),color="skyblue")

y<- ggplot(data=df,aes(x=date,y=close))+geom_line(aes(y=close))

z<-y+geom_line(aes(y=df$ocdiff),color="skyblue")
ggplotly(z)


sqldf('select max(VOLUME), pct,date, wday, quarter,ocdiff,candlestrength from df')
sqldf('select min(VOLUME), pct,date, wday, quarter,candlestrength from df')
sqldf('select avg(VOLUME), pct,date, wday, quarter,candlestrength from df')

sqldf(' select max(pct),date, wday, quarter,volume from df')
sqldf(' select min(pct),date, wday, quarter,volume from df')
sqldf(' select date,pct,volume,ocdiff,quarter,candlestrength from df where candlestrength = "Green"')

## Plotting ranges of Open Close Differences 
ggplot(data = df,aes(y=ocdiff))+geom_boxplot()
p1<-ggplot(data = df,aes(y=ocdiff))+geom_boxplot()
ggplotly(p1)

## Plotting ranges of  Day Percentage changes
ggplot(data = df,aes(y=pct))+geom_boxplot()
p2<-ggplot(data = df,aes(y=pct))+geom_boxplot()
ggplotly(p2)

g<- ggplot(data=df1,aes(x=date,y=VOLUME/1000000))+geom_line()+scale_y_continuous()
g1<-g+geom_line(aes(y=pct),color="green")
g1+ ylab("Volume in millions")
