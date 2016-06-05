#####---lesson 8---#####

###-创建RC类
library(pryr)
# 定义一个RC类 
User<-setRefClass("User",fields=list(name="character")) 
# 查看User的定义 
User 
# 实例化一个User对象u1 
u1<-User$new(name="u1") 
#查看u1对象
u1 
# 检查User类的类型 
class(User) 
is.object(User) 
otype(User)
# 检查u1的类型 
class(u1) 
is.object(u1)  
otype(u1) 



##创建一个有继承关系的RC类
# 创建RC类User 
User<-setRefClass("User",fields=list(name="character")) 
# 创建User的子类Member 
Member<-setRefClass("Member",contains="User",fields=list(manager="User")) 
# 实例化User 
manager<-User$new(name="manager") 
# 实例化一个Son对象 
member<-Member$new(name="member",manager=manager) 
# 查看member对象 
member 
# 查看member对象的name属性 
member$name 
# 查看member对象的manager属性 
member$manager
# 检查对象的属性类型 
otype(member$name) 
otype(member$manager)


##RC对象的默认值
# 定义一个RC类 
User<-setRefClass("User", 
                  # 定义2个属性 
                  fields=list(name="character",level='numeric'), 
                  methods=list( 
                    # 构造方法 
                    initialize = function(name,level)
                    { 
                      print("User::initialize") 
                      # 给属性增加默认值 
                      name <<- 'conan' 
                      level <<- 1 
                    } 
                  ) ) 

# 实例化对象u1 
u1<-User$new() 
# 查看对象u1，属性被增加了默认值 
u1 


###对象赋值
#定义User类 
User<-setRefClass("User",fields=list(name="character",age="numeric",gender="factor")) 
#定义一个factor类型 
genderFactor<-factor(c('F','M')) 
#实例化u1 
u1<-User$new(name="u1",age=44,gender=genderFactor[1]) 
#查看age属性值 
u1$age 

##给U1的age属性赋值
# 重新赋值 
u1$age<-10 
# age属性改变 
u1$age 

u2<-u1
u2$age
u1$age<-20
u1$age
u2$age

u3<-u1$copy()
u3$age
u1$age<-10
u1$age
u3$age


###定义对象的方法
# 定义一个RC类，包括方法 
User<-setRefClass("User", 
fields=list(name="character",favorite="vector"), 
# 方法属性 
methods = list( 
# 增加一个兴趣 
addFavorite = function(x) { 
favorite <<- c(favorite,x) 
}, 
# 删除一个兴趣 
delFavorite = function(x) { 
favorite <<- favorite[-which(favorite == x)] 
}, 
# 重新定义兴趣列表 
setFavorite = function(x) { 
favorite <<- x 
} 
 ) 
) 

# 实例化对象u1 
u1<-User$new(name="u1",favorite=c('movie','football')) 
#查看u1对象 
u1 

##方法操作
# 删除一个兴趣 
u1$delFavorite('football') 
#查看兴趣属性 
u1$favorite 
#增加一个兴趣 
u1$addFavorite('shopping') 
u1$favorite 

#重置兴趣列表 
u1$setFavorite('reading') 
u1$favorite 



###RC对象系统的使用
##定义动物的数据结构和发声方法
# 创建Animal类，包括name属性，构造方法initialize()，叫声方法bark()。 
Animal<-setRefClass("Animal", 
fields=list(name="character"), 
methods=list( 
initialize = function(name) name <<- 'Animal', 
bark = function() print("Animal::bark") 
 ) 
) 
#创建Cat类，继承Animal类，并重写(Overwrite)了 initialize() 和 bark()。 
Cat<-setRefClass("Cat",contains="Animal", 
methods=list( 
initialize = function(name) name <<- 'cat', 
bark = function() print(paste(name,"is miao miao")) 
 ) 
) 
#创建Dog类，继承Animal类，并重写(Overwrite)了 initialize() 和 bark()。 
Dog<-setRefClass("Dog",contains="Animal", 
methods=list( 
initialize = function(name) name <<- 'dog', 
bark = function() print(paste(name,"is wang wang")) 
 ) 
) 
#创建Duck类，继承Animal类，并重写(Overwrite)了 initialize() 和 bark()。 
Duck<-setRefClass("Duck",contains="Animal", 
methods=list( 
initialize = function(name) name <<- 'duck', 
bark = function() print(paste(name,"is ga ga")) 
 ) 
) 
##实例化对象##
# 创建cat实例 
cat<-Cat$new() 
cat$name 
# cat叫声 
cat$bark() 

# 创建dog实例，并给dog起名叫Huang 
dog<-Dog$new() 
dog$initFields(name='Huang') 
dog$name 
# dog叫声 
dog$bark() 


#创建duck实例 
duck<-Duck$new() 
#duck叫声 
duck$bark() 

# 定义Animal类，增加limbs属性，默认值为4
Animal<-setRefClass("Animal",
                    fields=list(name="character",limbs='numeric'),
                    methods=list(
                      initialize = function(name) {
                        name <<- 'Animal'
                        limbs<<-4
                      },
                      bark = function() print("Animal::bark")
                    )
)
# 在Cat类的initialize()方法中，执行callSuper()方法，调用父类的同名方法
Cat<-setRefClass("Cat",contains="Animal",
                 methods=list(
                   initialize = function(name) {
                     callSuper()
                     name <<- 'cat'
                   },
                   bark = function() print(paste(name,"is miao miao"))
                 )
)
# 在Dog类的initialize()方法中，执行callSuper()方法，调用父类的同名方法
Dog<-setRefClass("Dog",contains="Animal",
                 methods=list(
                   initialize = function(name) {
                     callSuper()
                     name <<- 'dog'
                   },
                   bark = function() print(paste(name,"is wang wang"))
                 )
)
# 在duck类的定义wing属性， 并在initialize()方法，定义limbs和wing属性的默认值
Duck<-setRefClass("Duck",contains="Animal",
                  fields=list(wing='numeric'),
                  methods=list(
                    initialize = function(name) {
                      name <<- 'duck'
                      limbs<<- 2
                      wing<<- 2
                    },
                    bark = function() print(paste(name,"is ga ga"))
                  )
)

# 实例化cat对象，属性limbs为4
cat<-Cat$new();cat

# 实例化dog对象，属性limbs为4
dog<-Dog$new()
dog$initFields(name='Huang')
dog

# 实例化duck对象，属性limbs为2，wing为2
duck<-Duck$new();duck

# 定义类Animal，增加action()方法，用于通用的行为陆地上行动。
 Animal<-setRefClass("Animal",
                      fields=list(name="character",limbs='numeric'),
                       methods=list(
                         initialize = function(name) {
                           name <<- 'Animal'
                           limbs<<-4
                           },
                         bark = function() print("Animal::bark"),
                         action = function() print("I can walk on the foot")
                         )
                     )
# 定义Cat类，重写action()方法，并增加爬树的行动
Cat<-setRefClass("Cat",contains="Animal",
                    methods=list(
                      initialize = function(name) {
                        callSuper()
                        name <<- 'cat'
                        },
                      bark = function() print(paste(name,"is miao miao")),
                      action = function() {
                        callSuper()
                        print("I can Climb a tree")
                        }
                      )
                    )
# 定义Dog类，重写action()方法，并增加游泳行动
Dog<-setRefClass("Dog",contains="Animal",
                    methods=list(
                      initialize = function(name) {
                        callSuper()
                        name <<- 'dog'
                        },
                      bark = function() print(paste(name,"is wang wang")),
                      action = function() {
                        callSuper()
                        print("I can Swim.")
                        }
                      )
                   )
# 定义Duck类，重写action()方法，并增加游泳和短距离飞行
Duck<-setRefClass("Duck",contains="Animal",
                     fields=list(wing='numeric'),
                     methods=list(
                       initialize = function(name) {
                         name <<- 'duck'
                         limbs<<- 2
                         wing<<- 2
                         },
                       bark = function() print(paste(name,"is ga ga")),
                       action = function() {
                         callSuper()
                         print("I can swim.")
                         print("I also can fly a short way.")
                         }
                       )
                    )

#实例化cat
 cat<-Cat$new()
# cat的行动
 cat$action()

dog<-Dog$new()
dog$action()
                    
duck<-Duck$new()
duck$action()

####基于R6的面向对象编程####
###创建R6类
##安装R包
install.packages("R6")
library(R6)              
library(pryr)

##创建R6类
Person <- R6Class("Person",    # 定义一个R6类 
public=list( 
hello = function(){         # 定义公有方法hello 
print(paste("Hello")) 
  } 
 ) 
) 

Person                   # 查看Person的定义 
class(Person)             # 检查Person的类型 

##实例化Person对象
u1<-Person$new()   # 实例化一个Person对象u1 
u1             
class(u1)         #检查u1的类型

##用pryr包的otype检查Person类的类型和u1对象的实例化类型
otype(Person)   # 查看Person类型 
otype(u1)       # 查看u1类型 


###公有成员和私有成员
##设置公有成员
Person <- R6Class("Person", 
public=list( 
 name=NA,                           # 公有属性 
 initialize = function(name){       # 构建函数方法 
  self$name <- name 
  }, 
  hello = function(){                # 公有方法 
   print(paste("Hello",self$name)) 
    } 
  ) 
) 
conan <- Person$new('Conan')          # 实例化对象 
conan$hello()                         # 调用用hello()方法 

##设置私有成员 
Person <- R6Class("Person", 
  public=list(                       # 公有成员 
  name=NA, 
  initialize = function(name,gender){   
  self$name <- name 
  private$gender<- gender        # 给私有属性赋值 
  }, 
  hello = function(){
    print(paste("Hello",self$name)) 
    private$myGender()             # 调用私有方法 
  } 
  ),
  private=list(                      # 私有成员 
    gender=NA, 
    myGender=function(){ 
      print(paste(self$name,"is",private$gender)) 
    } 
  ) 
)
conan <- Person$new('Conan','Male')         # 实例化对象 
conan$hello()                               # 调用用hello()方法 
  
##私有性
conan$name            # 公有属性 
conan$gender          # 私有属性 
conan$myGender()      # 私有方法

##测试
Person <- R6Class("Person", 
  public=list( 
  name=NA, 
  initialize = function(name,gender){ 
  self$name <- name 
  private$gender<- gender 
   }, 
  hello = function(){ 
    print(paste("Hello",self$name))
    private$myGender() 
  }, 
  member = function(){              # 用于测试的方法 
    print(self) 
    print(private) 
    print(ls(envir=private)) 
  } 
  ), 
  private=list( 
    gender=NA, 
    myGender=function(){ 
      print(paste(self$name,"is",private$gender)) 
    } 
  ) 
) 
      
conan <- Person$new('Conan','Male') 
conan$member()                            # 执行member()方法 


      
 
###主动绑定
Person <- R6Class("Person", 
  public = list( 
  num = 100 
  ), 
  active = list(                      # 主动绑定 
    active  = function(value) { 
      if (missing(value)) return(self$num +10 ) 
      else self$num <- value/2 
    }, 
    rand = function() rnorm(1) 
  ) 
)

conan <- Person$new() 
conan$num                   # 查看公有属性 
conan$active                # 调用主动绑定的active()函数，结果为 num +10= 100+10=100 
##传参数
conan$active<-100    # 传参数 
conan$num            # 查看公有属性num 
conan$active         # 调用主动绑定的active()函数，结果为 num+10=50+10=60 
conan$active(100)    # 如果进行方法调用，其实会提示没有这个函数的  

conan$rand
conan$rand<-99

###R6类的继承关系
##创建父类Person
Person <- R6Class("Person", 
        public=list(                            # 公有成员 
        name=NA, 
        initialize = function(name,gender){ 
        self$name <- name 
        private$gender <- gender 
        }, 
        hello = function(){ 
        print(paste("Hello",self$name)) 
        private$myGender() 
        } 
         ), 
         private=list(                           # 私有成员 
        gender=NA, 
       myGender=function(){ 
       print(paste(self$name,"is",private$gender)) 
       } 
    ) 
) 
##创建子类Worker继承父类Person，并在子类增加bye()公有方法 
Worker <- R6Class("Worker", 
      inherit = Person,                # 继承，指向父类 
      public=list( 
       bye = function(){ 
         print(paste("bye",self$name)) 
      } 
    ) 
) 
##实例化父类和子类
u1<-Person$new("Conan","Male")        # 实例化父类 
u1$hello() 
u2<-Worker$new("Conan","Male")        # 实例化子类 
u2$hello() 
u2$bye() 

Worker <- R6Class("Worker",
                   inherit = Person,
                   public=list(
                     bye = function(){
                       print(paste("bye",self$name))
                       }
                     ),
                   private=list(
                     gender=NA,
                     myGender=function(){
                       print(paste("worker",self$name,"is",private$gender))
                       }
                     )
                   )

u2<-Worker$new("Conan","Male")
u2$hello() # 调用hello()方法

Worker <- R6Class("Worker",
                   inherit = Person,
                   public=list(
                     bye = function(){
                       print(paste("bye",self$name))
                       }
                     ),
                   private=list(
                     gender=NA,
                     myGender=function(){
                       super$myGender() # 调用父类的方法
                       print(paste("worker",self$name,"is",private$gender))
                       }
                     )
                   )

u2<-Worker$new("Conan","Male")
u2$hello()


###图书馆分类
Book <- R6Class("Book",            # 父类 
      private = list( 
      title=NA, 
      price=NA, 
      category=NA 
    ), 
    public = list( 
      initialize = function(title,price,category){ 
        private$title <- title 
        private$price <- price 
        private$category <- category 
      }, 
      getPrice=function(){ 
        private$price 
      } 
    ) 
) 
R <- R6Class("R",     # 子类R图书 
             inherit = Book 
) 
Java <- R6Class("JAVA",  # 子类JAVA图书 
                inherit = Book 
) 
Php <- R6Class("PHP",    # 子类PHP图书 
               inherit = Book 
) 
      

###实例化
r1<-R$new("R的极客理想-工具篇",59,"R") 
r1$getPrice() 
j1<-Java$new("Java编程思想",108,"JAVA") 
j1$getPrice() 
p1<-Java$new("Head First PHP & MySQL",98,"PHP") 
p1$getPrice() 

Book <- R6Class("Book",
                 private = list(
                   title=NA,
                   price=NA,
                   category=NA
                   ),
                 public = list(
                   initialize = function(title,price,category){
                     private$title <- title
                     private$price <- price
                     private$category <- category
                     },
                   getPrice=function(){
                     p<-private$price*self$discount()
                     print(paste("Price:",private$price,", Sell out:",p,sep=""))
                     },
                   discount=function(){
                     0.9
                     }
                   )
                 )

Java <- R6Class("JAVA",
                 inherit = Book,
                 public = list(
                   discount=function(){
                     0.7
                     }
                   )
                 )

 R <- R6Class("R",
                  inherit = Book,
                  public = list(
                    discount=function(){
                     super$discount()*0.7
                      }
                    )
                  )

 Php <- R6Class("PHP",
                    inherit = Book
                    )

r1<-R$new("R的极客理想-工具篇",59,"R")
r1$getPrice()

j1<-Java$new("Java编程思想",108,"JAVA")
j1$getPrice()

p1<-Php$new("Head First PHP & MySQL",98,"PHP")
p1$getPrice()

