## launch with: /usr/bin/env R --args /some/path/to/file.dat

dataFile <- commandArgs(trailingOnly=TRUE)

result <- file.info( dataFile )

output.value <- paste( dataFile , result$uname , result$grname , result$size , sep="," )

cat( output.value )
