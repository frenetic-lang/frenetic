#!/usr/bin/Rscript

## ----------------------------------------------------------------------------
## Globals for reading the data file
## ----------------------------------------------------------------------------
skip.header  <- 7           # how many lines to skip including the header row
comment.char <- "="         # skip lines with <char> in it 


## ----------------------------------------------------------------------------
## Don't touch below unless you know what you are doing
## ----------------------------------------------------------------------------
col.names    <- c( "time", "frames", "bytes" )
args <- commandArgs( trailingOnly = TRUE )
number.graphs <- length( args )

for ( d in 1:length( args ) ) {
    file      <- args[[d]]
    # get date and time from file name and convert to a time object
    # read the data
    traffic   <- read.table( file=file,
                             header=F,
                             col.names=col.names,
                             skip=skip.header,
                             comment.char="="
                           )
    # massage the data a bit
    traffic$kbits   <- ( traffic$bytes * 8 ) / 1024
    traffic$frames  <- NULL
    traffic$bytes   <- NULL
    traffic$time    <- as.numeric(
                         gsub("-.*", "", traffic$time, perl = T )
                       ) 
    # calculate max an avg
    traffic.max <- round( max( traffic$kbits ), digits = 2 )
    traffic.avg <- round( mean( traffic$kbits ), digits = 2 )
    # prepare the graph
    sub.title <- paste( "Max:", traffic.max, "Kbit/s; Avg:", traffic.avg, "Kbit/s" )
    names( traffic )
    # output as pdf and png 
    pdf( paste( file, ".pdf", sep = "" ) )
    plot( traffic,  type="h", main=NULL, lwd=3, sub=NULL, xlab="Time", ylab="Kbit/s", cex.axis=1.5, cex.lab=1.5, cex.sub=2.0)
    #png( paste( file, ".png", sep = "" ) )
    #plot( traffic,  type="h", main=file, sub=sub.title, xlab="Time", ylab="Kbit/s" )
}

