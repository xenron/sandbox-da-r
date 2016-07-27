package parallelR.examples.ch6.oldapi;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.apache.hadoop.io.BytesWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapred.MapReduceBase;
import org.apache.hadoop.mapred.Mapper;
import org.apache.hadoop.mapred.OutputCollector;
import org.apache.hadoop.mapred.Reporter;



public class Ch6Ex3Mapper
	extends MapReduceBase
	implements Mapper<Text, BytesWritable , Text , Text>
{

	public Ch6Ex3Mapper(){

		_outputValue = new Text() ;
		_rOutputBuffer = new StringBuilder() ;
		
	}
	
	@SuppressWarnings( "unchecked" )
	@Override
	public void map(Text inputKey , BytesWritable inputValue , OutputCollector<Text, Text> output , Reporter reporter )
		throws IOException
	{
		
		_outputValue.clear() ;
		
		if( _rOutputBuffer.length() > 0 ){
			_rOutputBuffer.delete( 0 , _rOutputBuffer.length() ) ;
		}

		
		
		BufferedReader rOutputReader = null ;
		OutputStream fileWriteHandle = null ;
		final File currentFile = new File( inputKey.toString() ) ;
		
		try{

			// write the raw bytes to a file. (input key name is the file name)
			fileWriteHandle = new FileOutputStream( currentFile ) ;
			fileWriteHandle.write( inputValue.getBytes() , 0, inputValue.getLength() ) ;
			closeOutputStream( fileWriteHandle ) ;

			
			final ArrayList<String> tempList = new ArrayList<String>() ;
			

			// note that Distributed Cache does not work in local mode; so if you
			// are testing a job locally before sending it to the cluster, be sure
			// to copy helper.R to your current directory (from which you run the
			// hadoop command)

			final List<String> commandLine = new ArrayList<String>() ;
			commandLine.add( "/usr/bin/env" ) ;
			commandLine.add( "R" ) ;
			commandLine.add( "--vanilla" ) ;
			commandLine.add( "--slave" ) ;
			commandLine.add( "--file=helper.R" ) ;
			commandLine.add( "--args " ) ;
			commandLine.add( inputKey.toString() ) ;
			
			
			final Process runtime = new ProcessBuilder( commandLine )
				.redirectErrorStream( true ) // merge stdout and stderr; simplifies debugging
				.start()
			;
			
			final int exitCode = runtime.waitFor() ;

			rOutputReader = new BufferedReader( new InputStreamReader( runtime.getInputStream() ) ) ;

			if( 0 != exitCode ){
				_rOutputBuffer.append( "error! " ) ;
			}
			
			_rOutputBuffer.append( rOutputReader.readLine() ) ;
			_outputValue.set( _rOutputBuffer.toString() ) ;
			
			// sic - we use the filename (the input key) as the output key
			output.collect( inputKey ,  _outputValue ) ;

		}catch( final Exception rethrow ){
			throw( new IOException( rethrow ) ) ;
		}finally{
			closeReader( rOutputReader ) ;
			closeOutputStream( fileWriteHandle ) ;
			currentFile.delete() ;
		}

	}
	
	private void closeReader( final Reader toClose ){
		try{
			toClose.close() ;
		}catch( final Exception swallowed ){
			// sic - igoring error, even null-pointer exception
		}
	}

	private void closeOutputStream( final OutputStream toClose ){
		try{
			toClose.flush() ;
			toClose.close() ;
		}catch( final Exception swallowed ){
			// sic - igoring error, even null-pointer exception
		}
	}


	private final Text _outputValue ;
	private final StringBuilder _rOutputBuffer ;


} // Ch6Ex3Mapper
