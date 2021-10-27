install.packages("tidyverse", dep = TRUE)

library(tidyverse)

install.packages("magic", dep = TRUE)

library(magic)

#Cuadrado Mágic
magic(6)

2^(-4)

725%/%7

725%%7

725/7

pi

2*pi

3^pi

pi^2

sqrt(200)

exp(1)

log(pi)

log(32, 2)

abs(-pi)

factorial(4)

choose(5, 3)

6^log(4,6)

log(4^6, 4)

choose(3,5)

sin(60*pi/180)

cos(60*pi/180)

sinpi(1/2) # = sin (pi/2) 

tan(pi) # -1.224647e-16 ~ 0

tan(pi/2) # 1.633124e+16 ~ Inf

asin(0.8660254) # arc sin en radianes

asin(0.8660254) * 180/pi # arc sin en grados

print(sqrt(2),10)
round(sqrt(2),3)
floor(sqrt(2))
ceiling(sqrt(2))
trunc(sqrt(2))


##Variables

x = (pi^2)/2
y <- cos(pi/4)

sin(pi/4) + cos(pi/4) -> z

x
y
z

edad <- 30
nombre <- "Leonardo Garcia"

##Función f(x) = x^3 - (3^x) * sen(x)
f = function(x) {
  x^3 - (3^x) * sin(x)
  }

f(4)
f(pi/2)

product <- function(x, y){
  x*y
}

product(5,7)


g <- function(x,y,z) {
  exp(x^2 + y^2 - z)
}

g(4, 3, 5)

l <- function(x, y, z){
  exp(x^2 + y^2)* sin(z)
}

l(1, 2, 3)
l(1, -1, pi)

ls()

opBasic = function(a,b) {
 print("suma")
 print(paste(sprintf("%i + %i =", a,b),a+b))
 print("resta")
 print(paste(sprintf("%i - %i =", a,b),a-b))
 print(paste(sprintf("%i - %i =", b,a),b-a))
 print("producto")
 print (paste(sprintf("%i * %i =", a,b),a*b))
 print("cosciente de la división entera")
 print(paste(sprintf("%i / %i =", a,b),a%/%b))
 print(paste("con resto =", a%%b))
 print("cosciente de la división entera")
 print(paste(sprintf("%i / %i =", b,a),b%/%a))
 print(paste("con resto =", b%%a))
}

opBasic(6,4)


class(3+2i)

(3+2i)*5

(3+2i)*(-1+3i)

(3+2i)/(-1+3i)

complex(real = pi, imaginary = sqrt(2))

complex(real = pi, imaginary = sqrt(2)) -> z1
z1

sqrt(as.complex(-5))

sqrt(3+2i)
exp(3+2i)
sin(3+2i)
cos(3+2i)

##Módulo = sqrt(Re(z)^2 + Im(z)^2)
Mod(z1)

##Argumento = arctan(im(z)/Re(z))
## = arccos(Re(z)/Mod(z))
## = arcsin(Im(z)/Mod(z))
## va de (-pi, pi]

Arg(-1+0i)
Arg(-1-2i)

##Conjugado = Re(z) - Im(z)i
Conj(z1)

#Parte Real y Parte Imaginaria
Re(z1)
Im(z1)

##Z = Mod(z) * (cos(Arg(z))+sin(Arg(z))i)
complex(modulus = 2, argument = pi/2) -> z2

z2

Mod(z2)
Arg(z2)
pi/2
