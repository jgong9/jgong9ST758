

# Homework 1, Due August 30, 2016 at the beginning of class

# write an R script to solve the problems in this assignment,
# save the R script as <unityid>HW1.R


# IMPORTANT: The homeworks will be graded by 
# me or the TA running source("<unityid>HWj.R")
# and looking at the printed output and any files that are output,
# so make sure you check that sourcing your file produces the output you expect
# We may also look at your code, so if you want to have the best chance
# of getting partial credit, make sure you provide informative comments.


## Problems below

## 1. 

# initialize vectors of logical, integer, double, complex, and character types
# of length 1000 and print out their sizes with object.size()

vec_log<-logical(1000)
vec_int<-integer(1000)
vec_dou<-double(1000)
vec_com<-complex(1000)
vec_cha<-character(1000)
ans1<-c(
  object.size(vec_log),
  object.size(vec_int),
  object.size(vec_dou),
  object.size(vec_com),
  object.size(vec_cha)
        )
names(ans1)<-c("logical","integer","double","complex","character")
print(ans1) #$ sizes of each object


## 2. 

# the following code creates a vector and a matrix of size 1000 x 1000
vec <- (1:1000)/1000
mat <- exp( - sqrt( outer( vec, -vec, "+" )^2 ) ) 



# write a double for loop to matrix multiply mat by vec
# and store the result in a vector named 'result'

result<-rep(0,1000)  #$ initialize the result vector for double for loop

time_loop<-system.time(
  for(i in 1:1000){
  for(j in 1:1000){
    result[i]<-result[i]+mat[j,i]*vec[j]
  }
}) #$ elapsed time of the for loop


time_operator<-system.time(
  mat%*%vec
) #$ elapsed time of matrix operator

print(rbind(time_loop,time_operator)) #$ recorded times

# print out the time it takes to multiply using the double for loop 
# and the time it takes to matrix multiply with the %*% operator
# ( use proc.time() or system.time() for this )



# compare the result to what you get by doing matrix multiplication
# by printing out the sum of squared diferences between result and (mat %*% vec)

sum((mat%*%vec-result)^2)
cat("The sum of squared differences between result and (mat %*% vec) is ",
    sum((mat%*%vec-result)^2))
#$ The sum of squared differences between result and (mat %*% vec) must be 0.
#$ They are same.

## 3. 
# There is a file trigfuns.eps in the course directory. Write R code to reproduce
# the plot as closely as possible

# save the result as 'hw1p3.eps'

dev.off()
postscript(file="hw1p3.eps")
x_tan<-seq(-pi,pi,length.out=1000)
y_tan<-tan(x_tan)

x_sin<-seq(-1,2*pi+1,length.out=1000)
y_sin<-sin(x_sin)

x_cos<-seq(-1,2*pi+1,length.out=1000)
y_cos<-cos(x_cos)

close.screen(all=TRUE)
split.screen(c(1,2))
split.screen(c(2,1),screen=2)

screen(1) 
par(mar=c(3,2.2,3,2))
plot(x_tan,y_tan,xlim=c(-pi,pi),ylim=c(-4,4),xlab='',ylab='',type='n',xaxt="n",lty=2,main="tangent")

axis(side=1,at=c(-pi/2,pi/2),labels=expression(-pi/2,pi/2),mgp=c(3,1,0))
axis(side=1,at=c(-pi,0,pi),labels=expression(-pi,0,pi),mgp=c(3,0.9,0))


curve(tan,-pi-2,-pi/2-0.000001,lwd=2,add=T)
curve(tan,-pi/2+0.000001,pi/2-0.000001,lwd=2,add=T)
curve(tan,pi/2+0.000001,pi+2,lwd=2,add=T)
abline(v=c(-pi/2,pi/2),lty=2)
#erase.screen() 


screen(4) 
par(mar=c(3,2,0,3))
plot(x_cos,y_cos,xlim=c(0,2*pi),ylim=c(-1.3,1.3),type='l',xaxt="n",yaxt='n')
axis(side=2,at=c(-1,0,1),labels=c(-1,0,1))
axis(side=1,at=c(pi/2,3*pi/2),labels=expression(pi/2,3*pi/2),mgp=c(3,1,0))
axis(side=1,at=c(0,pi,2*pi),labels=expression(0,pi,2*pi),mgp=c(3,0.9,0))

text(pi,0.2,"cosine")

#erase.screen() 

screen(3) 
par(mar=c(0,2,3,3))
plot(x_sin,y_sin,xlim=c(0,2*pi),ylim=c(-1.3,1.3),type='l',xlab="",xaxt='n',lty=4,yaxt='n')
axis(side=2,at=c(-1,0,1),labels=c(-1,0,1))
axis(side=1,at=c(0,pi/2,pi,3*pi/2,2*pi),labels=NA)

text(3*pi/2,0.2,"sine")
#erase.screen() 
#close.screen(all=TRUE)


dev.off() #$ save reproduced image


## 4. (optional for fun) make a plot of the olympic rings, matching the image
# below as close as possible, with a white background. (no axes or labels)

# https://en.wikipedia.org/wiki/Olympic_symbols#/media/File:Olympic_rings_without_rims.svg

# save the result as 'hw1p4.eps'

postscript(file="hw1p4.eps",horizontal=F,width=12,height=9)

z<-seq(0,2*pi,length.out=200)
x<-cos(z)*3
y<-sin(z)*3
plot(x[1:125],y[1:125],type='l',asp=1,ann=F,axes=F,xlim=c(-11,11),ylim=c(-7,5),lwd=15)
lines(x[150:175],y[150:175],col=1,lwd=15) #$ black
lines(x[25:175]-7,y[25:175],col=4,lwd=15) #$ blue

lines(x-3.5,y-3,col=7,lwd=15) #$ yellow
lines(x[c(175:200,1:50)]-7,y[c(175:200,1:50)],col=4,lwd=15) #$ blue
lines(x[125:150],y[125:150],col=1,lwd=15) #$ black

lines(x[1:125]+7,y[1:125],col=2,lwd=15) #$ red
lines(x[150:200]+7,y[150:200],col=2,lwd=15) #$ red

lines(x+3.5,y-3,col=3,lwd=15) #$ green
lines(x[125:150]+7,y[125:150],col=2,lwd=15) #$ red
lines(x[c(175:200,1:25)],y[c(175:200,1:25)],col=1,lwd=15) #$ black

dev.off() #$ save the Olympic Symbol image 

#$ end of HW1
