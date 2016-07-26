#Chapter 4: Reshaping Datasets

#######################
#  Code block 1
#######################


# Example of typical two dimensional data
# A demo dataset "students" with typical layout. This data 
# contains two students' exam score of "math", "literature" 
# and "language" in different term exam.
students <- data.frame(sid=c(1,1,2,2),
exmterm=c(1,2,1,2),
math=c(50,65,75,69),
literature=c(40,45,55,59),
language=c(70,80,75,78))
students

#######################
#  Code block 2
#######################

#The new layout of a dataset
# Example of molten data 
library(reshape)
molten_students <- melt.data.frame(students,id.vars=c("sid","exmterm"))

#######################
#  Code block 3
#######################

#Reshaping the dataset from the typical layout
# Reshaping dataset using reshape function
wide_students <- reshape(students,direction="wide",idvar="sid",timevar="exmterm")
wide_students
# Now again reshape to long format
long_students <- reshape
(wide_students,direction="long",idvar="id")
long_students

#######################
#  Code block 4
#######################

#Melting data
# original data
students
# Melting by specifying both id and measured variables
melt(students,id=c("sid","exmterm"),
   measured=c("math","literature","language"))
# Melting by specifying only id variables
melt(students,id=c("sid","exmterm"))

#Casting molten data
# Melting students data
molten_students <- melt(students,id.vars=c("sid","exmterm"))
molten_students
cast(molten_students,sid+exmterm~variable)
cast(molten_students,...~variable)
cast(molten_students,...~sid)
cast(molten_students,...~exmterm)

#######################
#  Code block 5
#######################

#The reshape2 package
library(reshape2)
molten_students <- melt(students,id.vars=c("sid","exmterm"))
dcast(molten_students, sid~variable)
dcast(molten_students, sid+exmterm~variable)
acast(molten_students, sid~variable)
acast(molten_students, sid+exmterm~variable)
dcast(molten_students, sid~variable+exmterm)
acast(molten_students, sid~variable+exmterm)
acast(molten_students, sid~exmterm~variable)






