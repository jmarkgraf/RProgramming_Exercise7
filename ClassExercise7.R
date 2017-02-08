## 6.4 -- Class activity

## I made this horrible piece of code.  Fix it.  This should involve:
## 1) Adding comments, appropriate indenting, naming conventions
## 2) Fixing any bugs you find.
## 3) Functionalizing this to work with other variables

## Codebook for the dataset: http://pages.wustl.edu/montgomery/polls-codebook

# load data
stuff <- url("http://pages.wustl.edu/montgomery/polls2012.txt")
dataset <- read.csv(stuff)

# create empty plot
plot(NULL, main = "Polls organized population", xlim=c(0,4), ylim=c(46, 54))


for(i in 1:2){voter_type <- c("Likely Voters", "Registered Voters");  
polling_method <- c(75, 1)
points(y = dataset$Predict.Obama[dataset[ , "Population"] == voter_type[i]],
       x = jitter(rep(i, polling_method[i]), 8), col = i, pch = 19)}

plot(NULL, main = "Polls organized pollster", xlim=c(0,30), ylim=c(46, 54))
for(i in 1:30){
  name_pollster <- c("ABC/Post"                           
                     , "AP-GfK"                              
                     , "ARG"                                 
                     , "Angus-Reid"                          
                     , "CBS"                                 
                     , "CBS/Times"                           
                     , "CNN"                                 
                     , "DailyKos/SEIU/PPP (D)"               
                     , "Democracy Corps (D)"                 
                     , "FOX"                                 
                     , "Gallup"                              
                     , "Gravis Marketing"                    
                     , "High Point University"               
                     , "IBD/TIPP"                            
                     , "Ipsos/Reuters (Web)"                 
                     , "JZ Analytics/Newsmax"                
                     , "Monmouth"                            
                     , "NBC/WSJ"                             
                     , "NPR"                                 
                     , "PPP (D-Americans United for Change)" 
                     , "Pew"                                 
                     , "Politico/GWU/Battleground"           
                     , "Purple Strategies"                   
                     , "Rasmussen"                           
                     , "UConn/Hartford Courant"              
                     , "UPI/CVOTER"                          
                     , "United Technologies/National Journal"
                     , "Washington Times/JZ Analytics"       
                     , "YouGov"                              
                     , "YouGov/Economist")
  points(rep(i, sum(dataset[ , "Pollster"] == name_pollster[i])),  # deleted 'jitter()', because not needed here
         y = dataset$Predict.Obama[dataset[ , "Pollster"] == name_pollster[i]], col=i, pch=19)}

# Minae - It seems to be hard to make function. The below is what I has learnt so far. 
X<-as.factor(dataset$Population)
X<-factor(x,levels=c("Likely Voters", "Registered Voters"))
Y<-dataset$Predict.Obama
Z<-cbind.data.frame(X,Y)
plot(NULL, main = "Polls organized population", xlim=c(0,4), ylim=c(46, 54))  # doesn't this replicate what we did in task 1?
points(Z$X,Z$Y,pch=19, col=Z$X)


