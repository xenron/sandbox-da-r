defaultArg <- function(arg1,arg2="bubba")
{
    return(list(one=arg1+1,two=arg2))
}

val <- defaultArg(1,FALSE)
val

val <- defaultArg(2)
val

val <- defaultArg(1,2,3)
