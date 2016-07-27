5+5

5*
  2

5+3
4-5
1*10
1/10
2^3

2*(3+1)
5^(1+1)

1*2 # *3

5+5 # Adding 5 and 5

1<2
1>2
2>2
2>=2
2!=2

(1<10) & (10<100)
(1<10) & (10>100)
(1<10) | (10<100)
(1<10) | (10>100)

1 == 1
1 == 2
1 != 1
1 != 2

1 == 1
!(1 == 1)
(1 == 1) & (2 == 2)
(1 == 1) & !(2 == 2)

3*3
"*"(3,3)

sqrt(16)

# "oranges" + "apples"

sqrt(-2)

?sqrt

pi

class(TRUE)
class(1)
class(pi)
class("a")
class(sqrt)


library(raster)
r = raster("C:\\Data\\rainfall.tif")
r[120, 120] = 1000
#writeRaster(r, "C:\\Data\\rainfall2.tif")
