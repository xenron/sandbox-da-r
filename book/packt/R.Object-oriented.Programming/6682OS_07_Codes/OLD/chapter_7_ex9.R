lupe <- 1
while(lupe <= 2.0)
{
   cat("The value of lupe is ",lupe,"\n")
   lupe <- lupe + 0.33
   if(lupe > 1.5)
     break
}

lupe <- 1
while(lupe <= 2.0)
{
    lupe <- lupe + 0.33
    if(lupe < 1.5)
            next
    cat("The value of lupe is ",lupe,"\n")
}
