#! /usr/bin/env Rscript

input <- file( "stdin" , "r" )

while( TRUE ){

	currentLine <- readLines( input , n=1 )
	if( 0 == length( currentLine ) ){
		break
	}

	currentFields <- unlist( strsplit( currentLine , "," ) )

	result <- paste(
		currentFields[1] ,
		currentLine ,
		sep="\t"
	) 

	cat( result , "\n" ) 

}

close( input )
