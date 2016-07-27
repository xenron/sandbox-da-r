badValue <- function(arg1,arg2)
{
      if(missing(arg2))
         {
           stop("What are you thinking, the second argument is missing, jerk.")
         }
      return(arg1+arg2)
}

badValue(3,5)
bad <- badValue(3)
bad

                      
