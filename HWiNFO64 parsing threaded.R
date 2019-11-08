library(tidyverse)
library(cowplot)
library(R.utils)
library(qdapRegex)
library(parallel)
library(doSNOW)  
library(foreach)


#====================================#
# User defined settings
#====================================#
# open file in working directory 
#csvPath = "./csv/workdaySet1.csv"
csvPath = "./csv/workdaySet2(OC).csv"
# these values are 'padding'. They are not row indices.
# They are the number of rows to skip at the start and 
# the number of rows to skip at the end.  
rowStartPadding = 1000  #
rowEndPadding = 1000    #


#====================================#
# Gather local environment info
#====================================#
# find number of cores
#coreCount = floor( 0.86 * detectCores()) # use most of our cores
coreCount = detectCores() - 1 
# setup simple multi-thread cluster with local node for each core
# specifically declaring outfile as blank outputs to console
localCluster <- makeCluster(coreCount, outfile="" , type = "SOCK")

#====================================#
# setup CSV read for threading 
#====================================#
# makeTime returns a copy of the dataset with Date and Time 
# columns concatentated into one column 'Time'. Time is
# formatted as POSIX datetime, and 'Date' is removed from set.
makeTime <- function( t_dataset ){
  # concatenate Date and Time columns into one
  dTime <- paste( t_dataset[ c( rowStartPadding:( nL-rowEndPadding )),]$Date, 
                  t_dataset[ c( rowStartPadding:( nL-rowEndPadding )),]$Time )
  # rewrite Time as datetime value
  t_dataset$Time <- as.POSIXct( strptime( dTime , "%d.%m.%Y %H:%M:%OS" ))
  options(digits.secs=3) #keep our miliseconds
  # drop Date column 
  t_dataset[ , !(names(t_dataset) %in% c("Date"))]
  #provide update
  return(t_dataset)
}
makeTimeHeader <- function( t_dataset ){
  # concatenate Date and Time columns into one
  dTime <- paste( t_dataset[ 1, ]$Date, 
                  t_dataset[ 1, ]$Time )
  # rewrite Time as datetime value
  t_dataset$Time <- as.POSIXct( strptime( dTime , "%d.%m.%Y %H:%M:%OS" ))
  options(digits.secs=3) #keep our miliseconds
  # drop Date column 
  t_dataset[ , !(names(t_dataset) %in% c("Date"))]
  #provide update
  return(t_dataset)
}

# get first row so we have model
tmp_df <- read.csv( file=csvPath, header=TRUE,  stringsAsFactors=FALSE, 
                    nrows=1, sep=",", check.names=FALSE # stop read.csv replacing special characters with '.' dots.
                    ) 
# fix our $Time and $Date data
tmp_df <- makeTimeHeader( tmp_df )
# store list of colomn names
columnNames <- colnames( tmp_df )
# number of columns.
nC = length( tmp_df )
# number of Rows
nL = countLines( csvPath )

# the number of lines in a chunk depends on the size of the csv
# and how many rows are padded on each side and how many cores
# the local machine has
chunkSize = floor((nL - (rowStartPadding + rowEndPadding))/coreCount)

#====================================#
# define dataset and whatever
#====================================#



# the entire csv file
dataset <- na.omit(read.csv( file=csvPath, 
                             header=TRUE, # the first row of the csv is the column headers
                             stringsAsFactors=FALSE, # character fields as single string
                             nrows=nL-3, # last 3 rows contain entirely different data then rest of table
                             sep=",", dec=".", # match csv separator and decimal characters
                             check.names=FALSE, # stop read.csv replacing special characters with '.' dots.
                             colClasses=c( Date="character",  # set class types for columns
                                           Time="character", 
                                           rep( "numeric", nC )
                             )
                    ))


read.csvChunk <- function( chunkIndex ){
  
}




# the entire csv file
dataset <- na.omit(read.csv( file=csvPath, 
                        header=TRUE, # the first row of the csv is the column headers
                        stringsAsFactors=FALSE, # character fields as single string
                        nrows=nL-3, # last 3 rows contain entirely different data then rest of table
                        sep=",", dec=".", # match csv separator and decimal characters
                        check.names=FALSE, # stop read.csv replacing special characters with '.' dots.
                        colClasses=c( Date="character",  # set class types for columns
                                      Time="character", 
                                      rep( "numeric", nC )
                                     )
                    ))
# keep our miliseconds
options( digits=3 )


#====================================#
# collect data groups
#====================================#

plotData <- function( searchData ){
  
  # get the pieces 
  regexp <- searchData[[1]]
  ptitle <- searchData[[2]]
  punit <- searchData[[3]]
  
  #check for valid expression
  if( qdapRegex::is.regex(regexp) == TRUE ){
    
    # create a sample by searching for columns 
    # that match the regex 
    datasample <- na.omit( as.data.frame( dataset[ c( rowStartPadding:( nL-rowEndPadding )), 
                                          c( grep(regexp, columnNames, fixed=FALSE, perl=TRUE )) 
                                         ]))
    # check for data in return sample
    if( nrow( datasample ) > 0 ){
      
      # easier to store annalysis before we replace Time
      maxs <- sapply( datasample, max )
      mins <- sapply( datasample, min )
      means <- sapply( datasample, mean )
      
      # concatenate Date and Time columns into one
      dTime <- paste( dataset[ c( rowStartPadding:( nL-rowEndPadding )),]$Date, 
                      dataset[ c( rowStartPadding:( nL-rowEndPadding )),]$Time )
      # add formatted datetime to sample
      datasample$Time <- as.POSIXct( strptime( dTime , "%d.%m.%Y %H:%M:%OS" ))
      options(digits.secs=3) #keep our miliseconds
      
      # get tricky here and reformat the table into long data table
      # of key:value pairs. This allows ggplot to look at column 
      # names as qualitative groups.
      datasampleLong <- datasample %>% 
        select( colnames( datasample )) %>%  
        gather( key="variable", value="value", -Time) # ignore Time column
      
      #TODO: figure out better monospace fonts for mac and linux
      if( Sys.info()["sysname"] == "Windows" ){
        windowsFonts( Consolas=windowsFont("Consolas"))
      }#else{
       # fonts( Consolas=font("Consolas"))
      #}
      
      # plot the current search
      tplot <- ggplot(datasampleLong, aes( x=Time, y=value )) + 
        geom_line( aes( colour=variable )) + 
        labs( subtitle=ptitle, colour="Measurement", title=csvPath ) + # use specified title
        scale_color_hue( labels=function(x) str_trunc( x, 21 )) +
        theme( legend.text=element_text( family="Consolas" ))
      
      # extract legend
      legend <- cowplot::get_legend(tplot)
      
      # replot without legend
      tplot <- ggplot(datasampleLong, aes( x=Time, y=value )) + 
        geom_line( aes( colour=variable )) + 
        labs( subtitle=ptitle, colour="Measurement", 
              title=csvPath, y=paste( "Value", punit )) + # use specified title
        scale_y_continuous( labels=function(x)str_pad(x, 7, "left", pad=" " )) +  # left pad y values to 7 characters and
        theme( legend.position = "none", axis.text=element_text( family ="mono" )) # use mono-space for proper alignment
      
      # put plot and legend in grid for placement
      pgrid <- cowplot::plot_grid(tplot, legend, nrow=1, align="h", axis="t", rel_widths=c(9,2))
      
      # save the plot to a png file in working directory
      cowplot::save_plot(paste("./output/" ,ptitle, ".png", sep=""), plot = pgrid, device = "png", scale=2 )
      #dev.off()
    }
  }
}

# list of defined column groups, created by regex-ing the titles
# then each is given a human readable name. 
# This could probable even be stored in external csv file to share...
searches <- list(
  c( "^GPU.*C\\]", "GPU Temperatures", "°C" ),
  c( "^GPU.*Core*", "GPU Core", "Mixed" ),
  c( "^GPU.*Mem*", "GPU Memory", "Mixed" ),
  c( "^GPU.*MHz*", "GPU Clock Speed", "MHz" ),
  c( "^(?!.*GPU).*(Mem|Page).*", "System Memory", "Mixed" ),
  c( "^(?!.*GPU).*Core.*Use*", "System Core Use", "Mixed" ),
  c( "^(?!.*(?:Core|CPU|Activity|Page|Mem)).*\\[%\\]", "GPU Utilization", "%" ),
  c( "^.*(Core|CPU).*\\[%\\]", "System Utilization", "%" ),
  c( "^.*Activity.*\\[%\\]", "Disk Utilization", "%" ),
  c( "^(?!.*RPM).*CPU*", "CPU Info", "Mixed" )
)
si <- length(searches)

# build a data.frame to hold search list. 
# A nice object that can be passed to plotData() 
searchData <- data.frame( searchString=character(si), 
                          searchTitle =character(si),
                          displayUnits=character(si), 
                          stringsAsFactors=FALSE
                        )
# populate the data.frame with the search list.
for( i in 1:si ) searchData[i, ] <- searches[[i]] 


# start local cluster/ register parallel mode
# use doSNOW package so it works on windows also
registerDoSNOW(localCluster)
# time it, because why not 
# print() is necessary on windows Rstudio
print( system.time(
  
  # using %dopar% we can tell foreach to use a new thread 
  # for each iteration. we need to export tidyverse to each
  # node for plotting and publishing. Libraries are not 
  # exported by default.
  foreach(i=1:si, .packages=c( "dplyr", "tidyr", "stringr", "ggplot2", "cowplot", "qdapRegex" )) %dopar% {
    
    # sample plotting function now acts as
    # single process on each thread
    plotData(searchData[i,])
  }
  
))

# shut down the local cluster 
stopCluster(localCluster)

# return to sequential mode from parallel mode
registerDoSEQ()


# ta done
