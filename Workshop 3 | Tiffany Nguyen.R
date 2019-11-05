# Acknowledgement: Daniel Truong (UCSD Computer Science Major, Class of 2022)

# Step 1
D<-10^-6         # Diffusion coefficient (in m^2/s), #2 change through automation
dt<-1e-4         # Time step (in seconds), #1 change exponent through automation
total_time<-100  # Total time that simulation will run (in seconds)
dots_x<-rep(0,100)   # Creates 100 molecules to follow (x component of position)
dots_y<-rep(0,100)   # Creates 100 molecules to follow (y component of position)
dtList<-c()
finalDist<-c()
dList<-c(-10^-6, -10^-8, -10^-10, -10^-12, -10^-14, -10^-16)

for (dt in seq(1e-4, 1, by=1e-2)){
dist<-sqrt(2*D*dt)
dots_x<-rep(0,100)
dots_y<-rep(0,100)
time = 0
while (time < total_time) {
  time = dt + time
  
distx<-rnorm(length(dots_x),mean=dist,sd=0.341*dist) # Do not change this line
disty<-rnorm(length(dots_y),mean=dist,sd=0.341*dist) # Do not change this line

anglex<-sample(seq(0,2*pi,by=0.001*pi),length(dots_x),replace=TRUE) # Do not change this line
angley<-sample(seq(0,2*pi,by=0.001*pi),length(dots_y),replace=TRUE) # Do not change this line

dots_x<-dots_x+distx*cos(anglex)   # Moves the molecules (x component)
dots_y<-dots_y+disty*sin(angley)   # Moves the molecules (y component)
}
finalDist<-c(finalDist, mean(dots_x)^2 + mean(dots_y)^2)
dtList<-c(dtList, dt)
}

plot(dtList, finalDist)  # Plots the positions of the molecules using Base Graphics

#Step 2
rm(list=ls())  # Clears workspace
#Run the whole thing to receive different positions each time

D<-10^-6         # Diffusion coefficient (in m^2/s), #2 change through automation
dt<-1e-4         # Time step (in seconds), #1 change exponent through automation
total_time<-3  # Total time that simulation will run (in seconds)

dots_x<-rep(0,100)   # Creates 100 molecules to follow (x component of position)
dots_y<-rep(0,100)   # Creates 100 molecules to follow (y component of position)
finalDist<-c()
dList<-c()
for (D2 in seq(1e-4, 1, by=1e-2)){
dist<-sqrt(2*D2*dt)
time = 0
dots_x<-rep(0,100)
dots_y<-rep(0,100)
  while (time < total_time) {
    time = dt + time
    
    distx<-rnorm(length(dots_x),mean=dist,sd=0.341*dist) # Do not change this line
    disty<-rnorm(length(dots_y),mean=dist,sd=0.341*dist) # Do not change this line
    
    anglex<-sample(seq(0,2*pi,by=0.001*pi),length(dots_x),replace=TRUE) # Do not change this line
    angley<-sample(seq(0,2*pi,by=0.001*pi),length(dots_y),replace=TRUE) # Do not change this line
    
    dots_x<-dots_x+distx*cos(anglex)   # Moves the molecules (x component)
    dots_y<-dots_y+disty*sin(angley)   # Moves the molecules (y component)
  }
  finalDist<-c(finalDist, mean(dots_x)^2 + mean(dots_y)^2)
  dList<-c(dList, D2)
}
plot(dList, finalDist) # Plots the positions of the molecules using Base Graphics

# Question 3
## Step 1 Graph
dtgg <- data.frame(finalDist,dtList)
library(ggplot2)
ggplot(dtgg, aes(dtList, finalDist)) + geom_point() + 
  labs(title = "Final Distances in Regards to Time Step", x="Time Step", y="Final Distance")

## Step 2 Graph
dgg <- data.frame(finalDist,dList)
library(ggplot2)
ggplot(dgg, aes(dList, finalDist)) + geom_point() + 
  labs(title = "Final Distances in Regards to Diffusion Coefficient", x="Diffusion Coefficient", y="Final Distance")

# Question 4
rm(list=ls())  # Clears workspace

#Run the whole thing to receive different positions each time
D<-10^-6         # Diffusion coefficient (in m^2/s), #2 change through automation
dt<-1e-4         # Time step (in seconds), #1 change exponent through automation
total_time<-100  # Total time that simulation will run (in seconds)

dots_x<-rep(0,100)   # Creates 100 molecules to follow (x component of position)
dots_y<-rep(0,100)   # Creates 100 molecules to follow (y component of position)
timeList<-c()
distances<-c()

dist<-sqrt(2*D*dt)   # Calculates the root mean squared distance that a molecule should travel based on D.

time = 0
old_int = -1
#Run from here to spread out the graph bc it's adding distances
while (time < total_time) {
  time = dt + time
  distx<-rnorm(length(dots_x),mean=dist,sd=0.341*dist) # Do not change this line
  disty<-rnorm(length(dots_y),mean=dist,sd=0.341*dist) # Do not change this line
  currDist = (distx^2) + (disty^2)
  digits <- floor(time*100)/100
  if (digits != old_int) {
    old_int = digits
    distances<-c(distances, currDist)
    timeList<-c(timeList, time)
  }
  
  anglex<-sample(seq(0,2*pi,by=0.001*pi),length(dots_x),replace=TRUE) # Do not change this line
  angley<-sample(seq(0,2*pi,by=0.001*pi),length(dots_y),replace=TRUE) # Do not change this line
  
  dots_x<-dots_x+distx*cos(anglex)   # Moves the molecules (x component)
  dots_y<-dots_y+disty*sin(angley)   # Moves the molecules (y component)
}

plot(dots_x,dots_y)  # Plots the positions of the molecules

library(ggplot2)
df <- data.frame("time"= timeList, "distances"= distances)
ggplot(df, aes(time,distances)) + geom_point() +
  labs(title = "Relation between Origin and Destination in 100 seconds", x="Time (sec)", y="Distance") +
    ylim(1.3e-09,2.1e-09)

