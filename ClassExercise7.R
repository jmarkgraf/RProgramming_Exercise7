## 6.4 -- Class activity

## I made this horrible piece of code.  Fix it.  This should involve:
## 1) Adding comments, appropriate indenting, naming conventions
## 2) Fixing any bugs you find.
## 3) Functionalizing this to work with other variables

## Codebook for the dataset: http://pages.wustl.edu/montgomery/polls-codebook

# load data
stuff <- url("http://pages.wustl.edu/montgomery/polls2012.txt")
dataset <- read.csv(stuff)

## 1) Function: Poll results by Polling Method -----------

# create empty plot
plot(NULL, main = "Polls organized population", 
     xlim=c(0,4), ylim=c(46, 54), 
     ylab = "Predicted probability of voting for Obama", xlab = "Voter type")

# loop over two different polling methods and plot poll results by polling method
for(i in 1:2){voter_type <- c("Likely Voters", "Registered Voters");  
polling_method <- c(75, 1)
points(y = dataset$Predict.Obama[dataset[ , "Population"] == voter_type[i]],
       x = jitter(rep(i, polling_method[i]), 8), col = i, pch = 19)}

## 2) Function: Poll results by Pollster -----------

# create empty plot
plot(NULL, main = "Polls organized pollster", 
     xlim=c(0,30), ylim=c(46, 54), 
     ylab = "Predicted probability of voting for Obama (%)", xlab = "Pollsters")

# loop over the 30 different pollsters and plot the results by pollster
for(i in 1:30){
  name_pollster <- c("ABC/Post",                           
                     "AP-GfK",                              
                     "ARG",                                 
                     "Angus-Reid",                          
                     "CBS",                                 
                     "CBS/Times",                           
                     "CNN",                                 
                     "DailyKos/SEIU/PPP (D)",               
                     "Democracy Corps (D)",                 
                     "FOX",                                 
                     "Gallup",                              
                     "Gravis Marketing",                    
                     "High Point University",               
                     "IBD/TIPP",                            
                     "Ipsos/Reuters (Web)",                 
                     "JZ Analytics/Newsmax",                
                     "Monmouth",                            
                     "NBC/WSJ",                             
                     "NPR",                                 
                     "PPP (D-Americans United for Change)", 
                     "Pew",                                 
                     "Politico/GWU/Battleground",           
                     "Purple Strategies",                   
                     "Rasmussen",                           
                     "UConn/Hartford Courant",              
                     "UPI/CVOTER",                          
                     "United Technologies/National Journal",
                     "Washington Times/JZ Analytics",     
                     "YouGov",                              
                     "YouGov/Economist")
  points(x= rep(i, sum(dataset[ , "Pollster"] == name_pollster[i])),  
         # deleted 'jitter()', because noise not needed
         y = dataset$Predict.Obama[dataset[ , "Pollster"] == name_pollster[i]], col=i, pch=19)}

# Minae - It seems to be hard to make function. The below is what I has learnt so far.  -----
X<-as.factor(dataset$Population)
X<-factor(x,levels=c("Likely Voters", "Registered Voters"))
Y<-dataset$Predict.Obama
Z<-cbind.data.frame(X,Y)
plot(NULL, main = "Polls organized population", xlim=c(0,4), ylim=c(46, 54))  # doesn't this replicate what we did in task 1?
points(Z$X,Z$Y,pch=19, col=Z$X)




## 3) Functionalizing this to work with other variables ------------

# function
poll_predictions <- function(x, depvar = dataset$Predict.Obama) {
  
  # Function name: poll_predictions()
  # Purpose: generic function to plot poll predictions over different variables
  # Function: 
  ##   1) defines parameters for plot ("length(unique(x))")
  ##   2) applies the function over unique values in x vector
  ##   3) plots points with y being predicted probability of winning the election for Obama
  # Args:
  ##  x: vector in "dataset"; needs to be entered as "dataset$[VECTOR]"
  # Author: Jonas Markgraf
  
  plot(NULL, xlim=c(0,length(unique(x))), ylim=c(46, 54))
  for(i in 1:length(unique(x))) {
    reference <- unique(x)
    points(x = rep(i, sum(x == reference[i])),
           y = depvar[x == reference[i]], col=i, pch=19)
  }
}

# testing function
poll_predictions(dataset$Population)
poll_predictions(dataset$Pollster)
poll_predictions(dataset$Mode)
poll_predictions(dataset$End.Date)
