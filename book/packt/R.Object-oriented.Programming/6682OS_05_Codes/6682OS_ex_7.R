#example 7
colons <- regexpr(":",urls)
mailto <- urls
substr(mailto,1,colons-1) <- c("mailto","mailto","mailto")
mailto

