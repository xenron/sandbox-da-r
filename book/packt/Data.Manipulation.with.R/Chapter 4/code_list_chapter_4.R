##################
# Code Snipped-1
##################

# Example of typical two dimensional data

# A demo dataset "students" with typical layout. This data contains
# two students' exam score of "math", "literature" and "language" in
# different term exam.
students <- data.frame(sid=c(1,1,2,2),
                       exmterm=c(1,2,1,2),
                       math=c(50,65,75,69),
                       literature=c(40,45,55,59),
                       language=c(70,80,75,78))
students

##################
# Code Snipped-2
##################

library(reshape)
# Example of molten data
molten_students <- melt.data.frame(students,id.vars=c("sid","exmterm"))

##################
# Code Snipped-3
##################

# Reshaping dataset using reshape function
wide_students <- reshape(students,direction="wide",idvar="sid",timevar="exmterm")
wide_students

# Now again reshape to long format
long_students <- reshape(wide_students,direction="long",idvar="id")
long_students

##################
# Code Snipped-4
##################

# original data
students

# Melting by specifying both id and measured variables
melt(students,id=c("sid","exmterm"), measured=c("math","literature","language"))

# Melting by specifying only id variables
melt(students,id=c("sid","exmterm"))

##################
# Code Snipped-5
##################

# Melting students data
molten_students <- melt(students,id.vars=c("sid","exmterm"))
molten_students

# return back to original data
cast(molten_students,sid+exmterm~variable)

# Now the same operation but specifying only row variable.
cast(molten_students,...~variable)

# We now rearrange the data where sid is now separate column for each student
cast(molten_students,...~sid)

# Again rearranging the data where exmterm is now separate column for each term
cast(molten_students,...~exmterm)




