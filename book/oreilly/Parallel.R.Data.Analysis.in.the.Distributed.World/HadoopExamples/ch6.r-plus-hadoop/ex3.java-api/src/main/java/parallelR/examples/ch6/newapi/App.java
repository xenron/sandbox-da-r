package parallelR.examples.ch6.newapi;

import org.apache.hadoop.conf.Configured;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.io.compress.BZip2Codec;
import org.apache.hadoop.io.compress.CompressionCodec;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.input.SequenceFileInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
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
	
		final Job job = new Job( getConf() ) ;
		
		job.setJobName( "Parallel R: Chapter 6, example 3" ) ;
		job.setMapperClass( Ch6Ex3Mapper.class ) ;
	
		FileInputFormat.addInputPath( job , new Path( args[0] ) ) ;
		FileOutputFormat.setOutputPath( job , new Path( args[1] ) ) ;
		job.setInputFormatClass( SequenceFileInputFormat.class ) ;
	
		job.setOutputKeyClass( Text.class ) ;
		job.setOutputValueClass( Text.class ) ;
		
		job.getConfiguration().setBoolean( "mapred.output.compress" , true ) ;
		job.getConfiguration().setClass( "mapred.output.compression.codec" , BZip2Codec.class , CompressionCodec.class ) ;
		job.getConfiguration().setInt( "mapreduce.job.reduces" , 0 ) ;
	
		return( job.waitForCompletion( true ) ? 0 : 1 ) ;
	
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
