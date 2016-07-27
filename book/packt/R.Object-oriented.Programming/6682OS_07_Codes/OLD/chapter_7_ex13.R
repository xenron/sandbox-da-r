addThings <- function(first,second,third,fourth)
{
    resultOne <- first + second
    resultTwo <- third[1] & fourth[1]
    resultThree <-  xor(third,fourth)
    list(one=resultOne,two=resultTwo,third=resultThree)
}

bubba <- addThings(c(1,2,3),c(4,5,6),c(TRUE,FALSE),c(TRUE,TRUE))
bubba
bubba$one

