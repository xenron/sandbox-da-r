This directory contains examples from the Hadoop-related chapters of _Parallel R_
by Q Ethan McCallum and Stephen Weston.

The book's O'Reilly catalog page is:
	http://shop.oreilly.com/product/0636920021421.do

and the book's website is:
	http://www.parallelrbook.com/


NOTE: More than likely, the code will not run as-is.

- you'll need to modify it to match your environment

- many examples refer to a nonexistent function (such as "callAnalysis()").
  In the sample code, I've replaced this with paste() or cat()
  


CHAPTER BREAKDOWN

for all examples:

- modify common-settings.txt to match your environment

- modify */commandline.txt to match your Hadoop input and output paths

- source the file to pull in the environment variables.  If bash is your shell,
  the command would be:

	. ./common-settings.txt


ch6.r-plus-hadoop:

- there is no sample code for the second example, since it's practically the
  same as the first example.

- example 3 uses the Maven tool to build the Java code. You can grab a copy at:
  http://maven.apache.org/

- example 3 includes code for both the old and new Hadoop APIs


ch7.rhipe:

- modify all of the R scripts to match your environment and input/output paths

ch8.segue:

- modify the R script to match your AWS credentials and SSH key
