library(tidyverse)
library(cowplot)
library(R.utils)
library(qdapRegex)
library(parallel)
library(doSNOW)  
library(foreach)

# open file in working directory 
#csvPath = "./csv/workdaySet1.csv"
csvPath = "./csv/workdaySet2(OC).csv"


#====================================#
# setup dataset 
#====================================#

# count number of Rows in file
nL = countLines( csvPath )

# TODO: find a better way to count cols.
# There are 200+ columns in HWiNFO CSV. All 
# except 2 of them get classed as numeric.
nC = length( read.csv( file=csvPath, header=TRUE,  stringsAsFactors=FALSE, 
                       nrows=1, sep=",", check.names=FALSE )) - 2

# these values are 'padding'. They are not row indices.
# They are the number of rows to skip at the start and 
# the number of rows to skip at the end.  
rowStartPadding = 1000  #
rowEndPadding = 1000    #


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

# store list of colomn names
columnNames <- colnames( dataset )


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

# find number of cores
coreCount = floor( 0.85 * detectCores()) # use most of our cores
# setup simple multi-thread cluster on local nodes
localCluster <- makeCluster(coreCount, outfile=" " , type = "SOCK")
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
