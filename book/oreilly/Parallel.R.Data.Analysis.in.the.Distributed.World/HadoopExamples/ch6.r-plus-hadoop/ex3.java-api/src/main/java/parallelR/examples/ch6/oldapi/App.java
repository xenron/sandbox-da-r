package parallelR.examples.ch6.oldapi;

import org.apache.hadoop.conf.Configured;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.compress.BZip2Codec;
import org.apache.hadoop.io.compress.CompressionCodec;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapred.FileInputFormat;
import org.apache.hadoop.mapred.FileOutputFormat;
import org.apache.hadoop.mapred.JobClient;
import org.apache.hadoop.mapred.JobConf;
import org.apache.hadoop.mapred.SequenceFileInputFormat;
import org.apache.hadoop.util.Tool;
import org.apache.hadoop.util.ToolRunner;


public class App 
	extends Configured
	implements Tool
{
    public int run( String[] args )
    	throws Exception
    {

    	if( 2 != args.length ){
    		System.err.printf( "Usage: %s {generic options} {input} {output}\n" , getClass().getSimpleName() ) ;
    		ToolRunner.printGenericCommandUsage( System.err ) ;
    		return( -1 ) ;
    	}

 		final JobConf job = new JobConf( getConf() ) ;
    	
    	job.setJobName( "Parallel R: Chapter 3, example 3" ) ;
    	job.setJarByClass( Ch6Ex3Mapper.class ) ;
    	job.setMapperClass( Ch6Ex3Mapper.class ) ;

    	FileInputFormat.addInputPath( job , new Path( args[0] ) ) ;
    	FileOutputFormat.setOutputPath( job , new Path( args[1] ) ) ;
    	job.setInputFormat( SequenceFileInputFormat.class ) ;

    	job.setOutputKeyClass( Text.class ) ;
    	job.setOutputValueClass( Text.class ) ;
    	job.setBoolean( "mapred.output.compress" , true ) ;
    	job.setClass( "mapred.output.compression.codec" , BZip2Codec.class , CompressionCodec.class ) ;
    	job.setInt( "mapreduce.job.reduces" , 0 ) ;

		JobClient.runJob( job ) ;
		return( 0 ) ;

    }

    public static void main( final String[] args ) throws Exception {

    	if( 0 == args.length ){
    		System.err.printf( "Usage: %s {generic options} {input} {output}\n" , App.class.getSimpleName() ) ;
    		ToolRunner.printGenericCommandUsage( System.err ) ;
    		System.exit( 1 ) ;
    	}

    	int exitCode = ToolRunner.run( new App() , args ) ;

    	System.exit( exitCode ) ;
    	
    }

}
