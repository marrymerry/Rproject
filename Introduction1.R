5+10

15/300

help(lm)

#x = 5

x

coauthornetwork.csv <- read.csv("./exportdata/coauthornetwork.csv")


KMS305 <- read.csv("./exportdata/KMS305.csv")

coauthornetwork.csv.copy <- coauthornetwork.csv
KMS305.copy <- KMS305

head(coauthornetwork.csv)
head(KMS305)

x <-5
coauthornetwork.csv.rowno <- nrow(coauthornetwork.csv)
coauthornetwork.csv.colno <-ncol(coauthornetwork.csv)


tail(coauthornetwork.csv)
tail(KMS305)

summary(coauthornetwork.csv)
summary(KMS305)


KMS305$NR <- (KMS305$NR1 + KMS305$NR2 + KMS305$NR3)/3

class(KMS305$NR1)
class(KMS305)

dim(KMS305)
nrow(KMS305)
ncol(KMS305)

x <- 1
x <- c(1,2,3,4)


V1 <- c("NR1","NR2","NR3","ITR1","ITR2","ITR3","KSSE1","KSSE2","KSSE3")
KMS305constructs3 <- KMS305[,V1]

KMS305constructs3.sub <- subset(KMS305constructs3,NR1 > 5)
coauthornetwork.csv.sub <- subset(coauthornetwork.csv,times > 2)







authorattributes.csv <- read.csv("./exportdata/authorattributes.csv")
head(authorattributes.csv)
head(coauthornetwork.csv)

coauthornetwork.csv.tmp <- coauthornetwork.csv

colnames(coauthornetwork.csv.tmp) <- c("author","author_co","times")

coauthornetwork.csv.tmp.Step1 <- merge(x = coauthornetwork.csv.tmp, y = authorattributes.csv, by = c("author"),all.x  = TRUE)


write.csv(coauthornetwork.csv.tmp.Step1,"./exportdata/XXX.csv")

colnames(coauthornetwork.csv.tmp.Step1) <- c("author_ori","author","times","ori_publications")




#install.packages(c("DBI","RMySQL"))
library(DBI)
library(RMySQL)


con <- dbConnect(MySQL(),user="nanshan",password="999",dbname="jasist","144.214.55.105")
dbSendQuery(con,'SET NAMES gb2312')
dbListTables(con,"table_name")
getOption("encoding")


authors<-dbSendQuery(con,"Select Author from jasist")
authors_all <- fetch(authors,n=-1)



hist(KMS305[,1])
hist(KMS305[,2])


GetDistribution <- function(df){
  variable.No <- ncol(df) #获取这个df有多少列
  for(i in 1:variable.No){
    TmpFileName <- paste("./exportdata/test/",i,"-",colnames(df)[i],".png",sep="")
    png(file=TmpFileName)
    hist(df[,i]) #循环 获得每列的分布
    dev.off()
  }
}

GetDistribution(KMS305)


result <- lm(KMS305$FrequencyRead~KMS305$FrequencyPost + KMS305$PRA1)
result
summary(result)
plot(result)

height<-c(1.75, 1.80, 1.65, 1.90, 1.74, 1.91)
weight<-c(60, 72, 57, 90, 95, 72)
sq.height<-height^2
ratio<-weight/sq.height
t.test(ratio, mu=22.5) 


day <- c(2, 4, 3, 2, 4, 7, 7, 2, 5, 4, 5, 6, 8, 5, 10, 
         7, 12, 6, 6,7,11,6, 6, 7, 9, 5, 10, 6, 3, 10)
type <- c(rep("a",10),rep("b",9),rep("c",11))

bac <- data.frame(day,type)
ba.an<-aov(lm(day~type, data=bac)) 
summary(ba.an)

boxplot(day~type,data=bac,col=c("red","blue","green"))
