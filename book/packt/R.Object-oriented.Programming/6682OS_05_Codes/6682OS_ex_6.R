#example 6
colons <- regexpr(":",urls)
protocols <- substr(urls,1,colons-1)
protocols

