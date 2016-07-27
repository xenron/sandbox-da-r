#! /usr/bin/env Rscript

input <- file( "stdin" , "r" )

while( TRUE ){

	currentLine <- readLines( input , n=1 )
	if( 0 == length( currentLine ) ){
		break
	}

	currentFields <- unlist( strsplit( currentLine , "," ) )

	result <- paste(
		currentFields[1] , currentFields[2] , currentFields[3] ,
		currentFields[4] , currentFields[5] , currentFields[6]
	) 

	cat( Sys.getpid() , result , "\n" ) 

}

close( input )
