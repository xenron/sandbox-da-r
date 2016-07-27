missingVal <- function(arg1,arg2="bubba")
{
   if(missing(arg2))
     {
        warning("I am using the default value: ",arg2)
     }
  return(1/arg1)
}

missingVal(3)
missingVal(4,"it is okay")

