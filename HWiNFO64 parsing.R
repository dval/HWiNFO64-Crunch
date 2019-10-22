library(tidyverse)
library(R.utils)


csvPath = "./csv/workdaySet1.csv"

# number of Rows in file
nL = countLines(csvPath)
# magic number until a way to count cols is found
# There are 216 cols in CSV, but we are classing 
# 2 of them explicitly. The other 214 get classed
# as numeric.
nC = 214

datasample <- read.csv(file=csvPath, 
                       header=TRUE, 
                       stringsAsFactors=FALSE, 
                       nrows=nL-3, 
                       sep=",", 
                       dec=".", 
                       check.names=FALSE, 
                       colClasses=c(Date="character", Time="character", rep("numeric", nC))
                       )

options( digits=3)

cpuL <- as.data.frame(datasample[c(1:(nL-3)), c(grep("CPU", colnames(datasample)) )])

gpuL <- as.data.frame(datasample[c(1:(nL-3)), c(grep('^GPU.*C\\]', colnames(datasample)) )])

gpuL$Time = as.POSIXct(strptime(paste(datasample$Date, datasample$Time), "%d.%m.%Y %H:%M:%OS"))
options(digits.secs=3)

#gputemp$`GPU Temperature [°C]` <- as.numeric(as.character(gputemp$`GPU Temperature [°C]`))

ggplot(gpuL, aes(x=Time)) + 
  geom_hline(yintercept = 60) + 
  labs(y="Tempurature") + 
  geom_line(aes( y=gpuL[[1]], colour="red")) + 
  geom_line(aes( y=gpuL[[2]], colour="blue")) + 
  geom_line(aes( y=gpuL[[3]], colour="grey")) 

