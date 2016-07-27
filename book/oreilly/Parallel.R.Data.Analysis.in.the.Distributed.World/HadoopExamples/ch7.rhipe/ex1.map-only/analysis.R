#! /usr/bin/env Rscript

library(Rhipe)

source.data.file <- "/tmp/small-input.csv.bz2"
output.folder <- "/tmp/rhipe-out"

map.block <- expression({

	map.function <- function( row ){

		tuple <- unlist( strsplit( row , "," ) )

		map.key <- tuple[1]

		map.value <- paste( "pid=" , Sys.getpid() , sep="" )
		rhcollect( map.key , map.value )
		rhcounter( "map_task" , "handle_line" , 1 )
	}

	lapply( map.values , map.function )

})

config.block <- list(
	mapred.map.tasks=40 ,
	mapred.reduce.tasks=0
)

options.block <- rhoptions()
## uncomment and modify the following line if RHIPE is installed in a
## different path on the cluster than on your local workstation
## options.block$runner[1] <- "/usr/local/lib/R/site-library/Rhipe/libs/imperious.so"

rhinit(TRUE,TRUE,buglevel=2000)

rhipe.job.def <- rhmr(
	jobname="RHIPE example 1" ,

	map=map.block ,

	mapred=config.block ,

	opts=options.block ,

	ifolder=source.data.file ,
	ofolder=output.folder ,
	inout=c( "text" , "text" )

)

rhdel( output.folder )

rhipe.job.result <- rhex( rhipe.job.def )


print( rhipe.job.result )
