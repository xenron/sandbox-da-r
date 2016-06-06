#####---lesson 7---#####

###-object-oriented programming
##-R encapsulation
# 定义老师对象和行为
teacher <- function(x, ...) UseMethod("teacher")
teacher.lecture <- function(x) print("讲课")
teacher.assignment <- function(x) print("布置作业")
teacher.correcting <- function(x) print("批改作业")
teacher.default<-function(x) print("你不是teacher")

# 定义同学对象和行为
student <- function(x, ...) UseMethod("student")
student.attend <- function(x) print("听课")
student.homework <- function(x) print("写作业")
student.exam <- function(x) print("考试")
student.default<-function(x) print("你不是student")

# 定义两个变量，a老师和b同学
a<-'teacher'
b<-'student'

# 给老师变量设置行为
attr(a,'class') <- 'lecture'

# 执行老师的行为
teacher(a)

# 给同学变量设置行为
attr(b,'class') <- 'attend'

# 执行同学的行为
student(b)

attr(a,'class') <- 'assignment'
teacher(a)

attr(b,'class') <- 'homework'
student(b)

attr(a,'class') <- 'correcting'
teacher(a)

attr(b,'class') <- 'exam'
student(b)

# 定义一个变量，既是老师又是同学
ab<-'student_teacher'
# 分别设置不同对象的行为
attr(ab,'class') <- c('lecture','homework')
# 执行老师的行为
teacher(ab)

# 执行同学的行为
student(ab)

##-R inherit
# 给同学对象增加新的行为
student.correcting <- function(x) print("帮助老师批改作业")
# 辅助变量用于设置初始值
char0 = character(0)

# 实现继承关系
create <- function(classes=char0, parents=char0) {
  mro <- c(classes)
  for (name in parents) {
    mro <- c(mro, name)
    ancestors <- attr(get(name),'type')
    mro <- c(mro, ancestors[ancestors != name])
  }
  return(mro)
}

# 定义构造函数，创建对象
NewInstance <- function(value=0, classes=char0, parents=char0) {
  obj <- value
  attr(obj,'type') <- create(classes, parents)
  attr(obj,'class') <- c('homework','correcting','exam')
  return(obj)
}
# 创建父对象实例
StudentObj <- NewInstance()

# 创建子对象实例
s1 <- NewInstance('普通同学',classes='normal', parents='StudentObj')
s2 <- NewInstance('课代表',classes='leader', parents='StudentObj')

# 给课代表，增加批改作业的行为
attr(s2,'class') <- c(attr(s2,'class'),'correcting')

# 查看普通同学的对象实例
s1


# 查看课代表的对象实例
s2


##-polymorphic
# 创建优等生和次等生，两个实例
e1 <- NewInstance('优等生',classes='excellent', parents='StudentObj')
e2 <- NewInstance('次等生',classes='poor', parents='StudentObj')

# 修改同学考试的行为，大于85分结果为优秀，小于70分结果为及格
student.exam <- function(x,score) {
  p<-"考试"
  if(score>85) print(paste(p,"优秀",sep=""))
  if(score<70) print(paste(p,"及格",sep=""))
}

# 执行优等生的考试行为，并输入分数为90
attr(e1,'class') <- 'exam'
student(e1,90)

# 执行次等生的考试行为，并输入分数为66
attr(e2,'class') <- 'exam'
student(e2,66)

###-Process oriented programming

##-define teachers and students
# 辅助变量用于设置初始值
 char0 = character(1)

# 定义老师对象和行为
teacher_fun<-function(x=char0){
  if(x=='lecture'){
    print("讲课")
    }else if(x=='assignment'){
     print("布置作业")
      }else if(x=='correcting'){
        print("批改作业")
        }else{
         print("你不是teacher")
      }
}
# 定义同学对象和行为
student_fun<-function(x=char0){
  if(x=='attend'){
    print("听课")
    }else if(x=='homework'){
      print("写作业")
      }else if(x=='exam'){
        print("考试")
        }else{
          print("你不是student")
        }
}

# 执行老师的一个行为
 teacher_fun('lecture')

# 执行同学的一个行为
student_fun('attend')

##-difference between ordinary students and assistant
# 重定义同学的函数，增加角色判断
student_fun<-function(x=char0,role=0){
  if(x=='attend'){
    print("听课")
    }else if(x=='homework'){
      print("写作业")
      }else if(x=='exam'){
        print("考试")
        }else if(x=='correcting'){
          if(role==1){#课代表
           print("帮助老师批改作业")
            }else{
              print("你不是课代表")
              }
          }else{
            print("你不是student")
          }
}

# 以普通同学的角色，执行课代表的行为
student_fun('correcting')

# 以课代表的角色，执行课代表的行为
student_fun('correcting',1)

##-difference between top and inferior students
# 修改同学的函数定义，增加考试成绩参数
student_fun<-function(x=char0,role=0,score){
if(x=='attend'){
  print("听课")
    }else if(x=='homework'){
      print("写作业")
      }else if(x=='exam'){
        p<-"考试"
        if(score>85) print(paste(p,"优秀",sep=""))
        if(score<70) print(paste(p,"及格",sep=""))
        }else if(x=='correcting'){
          if(role==1){#课代表
            print("帮助老师批改作业")
            }else{
              print("你不是课代表")
              }
          }else{
            print("你不是student")
          }
}
# 执行考试函数，考试成绩为大于85分，为优等生
student_fun('exam',score=90)

# 执行考试函数，考试成绩为小于70分，为次等生
student_fun('exam',score=66)


###-S3

##-OOP in lm()
?lm
#lm
x<-c(1,2,3)
y<-c(1,3,8)
lmout<-lm(y~x)
class(lmout)
lmout

print
print.lm

unclass(lmout)

##-generic paradigm method
methods(print)

wrds<-"Which word is mispelled?"
aspout<-aspell("wrds")
print.aspell(aspout)

getAnywhere(print.aspell)

utils:::print.aspell(aspout)
methods(class="default")

##-program the object S3
lm
#j
j<-list(name="Joe",salary=55000,union=T)
class(j)<-"employee"
attributes(j)  #check
j

#print.employee
print.employee<-function(wrkr){
  cat(wrkr$name,"\n")
  cat("salary",wrkr$salary,"\n")
  cat("union member",wrkr$union,"\n")
}

methods(,"employee")

j

##-inherit
k<-list(name="Kate",salary=68000,union=F, hrsthismonth=2)
class(k)<-c("hrlyemployee","employee")

k
#class(z)<-c(if(is.matrix(y))"mlm","lm")


###-S4
##-program the S4
setClass("employee",
         representation(
           name="character",
           salary="numeric",
           union="logical")
)

#-Joe
joe<-new("employee",name="Joe", salary=55000,union=T)
joe

joe@salary
slot(joe, "salary")

joe@salary<-65000
joe

joe@salry<-48000

##-generic paradigm method in S4
joe
show(joe)

setMethod("show","employee",
          function(object){
            inorout<-ifelse(object@union,"is","is not")
            cat(object@name, "has a salary of", object@salary,
                "and", inorout, "in the union","\n")
          }
)
joe

john<-initialize(joe,name="john")
john

setClass("hrlyemployee",
         representation(hrsthismonth="numeric"),
         contains="employee")
Kate<-new("hrlyemployee",name="Kate",
          salary=680000,union=F,hrsthismonth=2)
Kate

##S4的泛型函数
work<-function(x) cat(x, "is working")
work('Conan')

# 定义Person对象
 setClass("Person",slots=list(name="character",age="numeric"))
# 定义泛型函数work，即接口
setGeneric("work",function(object) standardGeneric("work"))

# 定义work的现实，并指定参数类型为Person对象
 setMethod("work", signature(object = "Person"), function(object) cat(object@name , "is working") )

# 创建一个Person对象a
 a<-new("Person",name="Conan",age=16)
# 把对象a传入work函数
work(a)

# 直接查看work函数
 work
# 查看work函数的现实定义
 showMethods(work)
# 查看Person对象的work函数现实
 getMethod("work", "Person")
 selectMethod("work", "Person")
# 检查Person对象有没有work函数
 existsMethod("work", "Person")
hasMethod("work", "Person")