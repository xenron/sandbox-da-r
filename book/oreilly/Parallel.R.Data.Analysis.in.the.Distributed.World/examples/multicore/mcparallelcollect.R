library(multicore)

# Define our set of three contrived (but illustrative) functions
fun1 <- function() {Sys.sleep(10); 1}
fun2 <- function() {Sys.sleep(5);  2}
fun3 <- function() {Sys.sleep(1);  3}

# Start the three functions executing in the background
cat('1) Starting three tasks:\n')
f1 <- parallel(fun1())
f2 <- parallel(fun2())
f3 <- parallel(fun3())

# Here is where you might do something useful before you
# commit to waiting for all of the results

# Wait for the results of all three computations
cat('Calling collect with wait=TRUE:\n')
print(collect(list(f1, f2, f3)))

# Start the same three functions executing again
cat('2) Starting three tasks:\n')
f1 <- parallel(fun1())
f2 <- parallel(fun2())
f3 <- parallel(fun3())

# Check for results, but don't wait, so none are ready
cat('Calling collect with wait=FALSE:\n')
print(collect(list(f1, f2, f3), wait=FALSE))

# Pause a bit, simulating doing something useful
cat('Sleeping for 15 seconds...\n')
Sys.sleep(15)

# Now all the results are finished, so we get them in a list
cat('Calling collect with wait=FALSE:\n')
print(collect(list(f1, f2, f3), wait=FALSE))

# When call collect this time we get a list of three NULL's
# indicating that each of the worker processes has exited
cat('Calling collect with wait=FALSE:\n')
print(collect(list(f1, f2, f3), wait=FALSE))

# Finally we get a single NULL
cat('Calling collect with wait=FALSE:\n')
print(collect(list(f1, f2, f3), wait=FALSE))

# Start the three functions one last time
cat('3) Starting three tasks:\n')
f1 <- parallel(fun1())
f2 <- parallel(fun2())
f3 <- parallel(fun3())

# Because wait is FALSE and we have a timeout, we find out
# about the events (result or process death) one at a time
# until we get the final NULL
cat('Calling collect with wait=FALSE, timeout=1000000:\n')
print(collect(list(f1, f2, f3), wait=FALSE, timeout=1000000))
cat('Calling collect with wait=FALSE, timeout=1000000:\n')
print(collect(list(f1, f2, f3), wait=FALSE, timeout=1000000))
cat('Calling collect with wait=FALSE, timeout=1000000:\n')
print(collect(list(f1, f2, f3), wait=FALSE, timeout=1000000))
cat('Calling collect with wait=FALSE, timeout=1000000:\n')
print(collect(list(f1, f2, f3), wait=FALSE, timeout=1000000))
cat('Calling collect with wait=FALSE, timeout=1000000:\n')
print(collect(list(f1, f2, f3), wait=FALSE, timeout=1000000))
cat('Calling collect with wait=FALSE, timeout=1000000:\n')
print(collect(list(f1, f2, f3), wait=FALSE, timeout=1000000))
cat('Calling collect with wait=FALSE, timeout=1000000:\n')
print(collect(list(f1, f2, f3), wait=FALSE, timeout=1000000))
