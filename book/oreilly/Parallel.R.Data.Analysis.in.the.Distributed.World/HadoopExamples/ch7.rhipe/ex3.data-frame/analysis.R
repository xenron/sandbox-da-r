#! /usr/bin/env Rscript

library(Rhipe)

source.data.file <- "/tmp/small-sample-tweets.dat.bz2"
output.folder <- "/tmp/rhipe-out"

setup.block <- list(
	map=expression({
		## uncomment and modify the following lines if you're using
		## packages from someplace other than the global install path

		## home.Rlib <- "/path/to/your/other/R/libs"
		## invisible( .libPaths( c( home.Rlib , .libPaths() ) ) )
		## library(RJSONIO)
	}) ,
	reduce=expression({ })
)

config.block <- list(
	mapred.task.timeout=600000
)

map.block <- expression({

	rhcounter( "map_stage" , "enter_block" , 1 )

	map.function <- function( tweet.raw ){
		tryCatch({
			tweet <- fromJSON( tweet.raw )

			reply_user_id <- ifelse( is.null( tweet$in_reply_to_user_id ) ,  NA , tweet$in_reply_to_user_id )
			geo <- ifelse( is.null( tweet$geo ) ,  NA , tweet$geo )
			mentions <- ifelse( is.null( tweet$user_mentions ) , 0 , length( is.null( tweet$user_mentions ) ) )

			tuple <- data.frame(
				screen_name=tweet$user$screen_name ,
				in_reply_to=reply_user_id ,
				create_time=tweet$created_at ,
				retweet_count=tweet$retweet_count ,
				user_mentions=mentions ,
				location=geo ,
				text=tweet$text
			)

			print( tuple )

			rhcollect( tweet$user$screen_name , tuple )
			rhcounter( "map_stage" , "success" , 1 )
		} ,
		error=function( error ){
			rhcounter( "map_stage" , "error" , 1 )
			print( error )
		})
	}

	lapply( map.values , map.function )
})

reduce.block <- expression(
	pre = {
		df <- data.frame()
		currentKey <- reduce.key
		rhcounter( "reduce_stage" , "pre" , 1 )
	} ,
	reduce = {
		df.tmp <- do.call( rbind , reduce.values )
		df <- rbind( df , df.tmp )
		rhcounter( "reduce_stage" , "reduce" , 1 )
	} ,
	post = {
		cat( currentKey , nrow(df) , "\n" )
		rhcollect( currentKey , df )
		rhcounter( "reduce_stage" , "post" , 1 )
	}
)

rhinit(TRUE,TRUE,buglevel=2000)

options.block <- rhoptions()
## you'll need this if the R package is installed in a different directory
## on the cluster than on your local workstation
## options.block$runner[1] <- "/usr/local/lib/R/site-library/Rhipe/libs/imperious.so"

rhipe.job.def <- rhmr(
	jobname="rhipe tweet test" ,

	setup=setup.block ,
	map=map.block ,
	reduce=reduce.block ,

	opts=options.block ,

	mapred=config.block ,

	ifolder=source.data.file ,
	ofolder=output.folder ,
	inout=c( "text" , "sequence" )
)

rhipe.job.result <- rhex( rhipe.job.def )

