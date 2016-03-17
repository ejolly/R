#Most of the network plotting, extracting and analysis is based on this code, by Pablo Barbera, writer of the Rfacebook package:
#https://github.com/pablobarbera/Rdataviz/blob/master/code/06_maps.R

library('Rfacebook') #Facebook network analysis package
library('ggplot2') 
library('igraph') #network plot objects
library('maps') #US Map data
set.seed(100) #for replication
`%ni%`  <- Negate(`%in%`) #not in operation

#Get a Facebook API token from https://developers.facebook.com/tools/explorer
token <- 'XXXXX'

#### Pull and filter data ####

#Friends' info
me <- getUsers("me",token)
myfriends <- getFriends(token, simplify = F)
myfriends_info <- getUsers(myfriends$id, token = token, private_info=T)

#Retrieve all network data, i.e. matrix of booleans of whether a friend knows another friend
mat <- getNetwork(token = token, format= 'adj.matrix')
save(mat, "FB_Data.RData")

#Just load this for the future
load("FB_Data.RData")

#Filter out friends who are only friends with me or only friends with 1 other person in my network
lonely <- rowSums(mat) <=1
mat <- mat[!lonely,!lonely]

#### Prepare network and dataframes ####

#Create igraph object 
network <- graph.adjacency(mat, mode = "undirected")
#Create layout using Fruchterman-Reingold force-directed algorithm
l <- layout.fruchterman.reingold(network, niter = 1000, coolexp = 0.5) #layout algorithm

#Use random-walk algorithm for community detection
#Returns a dataframe with network info about each person 
fg <- walktrap.community(network,steps = 5)

#Build a dataframe from the x,y coordinates of the layout algorithm and cluster membership to for node plotting
D <- data.frame(l) 
names(D) <- c("x","y")
D$cluster <- factor(fg$membership)
D$names <- fg$names

#Build dataframe to handle edge plotting
edgeList <- get.edgelist(network, names = F)
edges <- data.frame(D[edgeList[,1], c("x","y")], D[edgeList[,2], c("x","y")])
names(edges) <- c("x1","y1","x2","y2")


#### Prepare US mapping and dataframes ####
trim <- function (x) gsub("^\\s+|\\s+$", "", x) #Function to remove leading and trailing whtsp
getState <- function(x) trim(tolower(strsplit(x,",")[[1]][2])) #return state from friends' location

#Figure out each friend's state from their current or hometown location and if the current and hometown locations are the same
myfriends_info$currentState <- unlist(lapply(myfriends_info$location,getState))
myfriends_info$homeState <- unlist(lapply(myfriends_info$hometown,getState))
myfriends_info$sameBool <- ifelse(myfriends_info$currentState==myfriends_info$homeState,1,0)

#Loop through each state and find out how many friends currently live their or are from their. Don't count the state twice if it's both.
#There's probably a better way to do this....
US <- map_data("state")
states <- unique(US$region)
for (i in states){
  counter <- 0
  for (j in 1:nrow(myfriends_info)){
    if (myfriends_info$sameBool[j] %in% 1){
      if (myfriends_info$currentState[j] %in% i){counter <- counter+1}
    } else{
      if (myfriends_info$currentState[j] %in% i) {counter  <- counter+1}
      if (myfriends_info$homeState[j] %in% i) {counter  <- counter+1}
    }
  }
  US$Count[US$region==i] <- counter
}

#For prettier plotting, log transform the state counts to deal with huge negative skew
US$Countplot <- log(US$Count2)
US$Countplot[US$Countplot==-Inf]  <- 0

#### Plots ####
#Unconstrained network plot solution
labels <- c("Highschoolery","Rochacha", "Dmouth/Upper Valley", "Fam friends", "Baruch peeps", "Beantown Jams", "Eastman", "India", "Harvardish","Beantown SGI", "Queens pals")
U.plot <- ggplot(D, aes(x,y, colour = cluster)) +
    geom_segment(aes(x=x1,y=y1,xend=x2,yend=y2), data=edges, size =.5, colour = "white",   
               alpha = .15) + 
    geom_point(size =0) + 
    geom_text(aes(label = names, colour = cluster),size = 3, family = "Helvetica", data= D, show_guide=F) +
    guides(colour = guide_legend(override.aes =list(size=5))) +
    scale_color_discrete() +
    theme(panel.background = element_rect(fill = "black"), 
          plot.background = element_rect(fill="black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          legend.background = element_rect(fill = "black"),
          legend.key = element_rect(fill = "black", colour = F),
          legend.title = element_blank(),
          legend.text = element_text(colour="white", size = 10),
          axis.ticks= element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank()) 
        
#Contiguous US plot
mapPlot <- ggplot() +
  geom_polygon(data=US, aes(x=long,y=lat,group=group, fill=Countplot), colour="gray80") +
  scale_fill_gradientn(colours=c("grey6", "navyblue", "firebrick", "coral2", "gold1")) +
  ggtitle("Friend density by location*") +
  theme(panel.background = element_rect(fill = "steelblue4"), 
        plot.background = element_rect(fill= "steelblue4"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.ticks= element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.text = element_blank(),
        legend.background = element_rect(fill = "steelblue4"),
        legend.title = element_blank(),
        legend.position = "top",
        legend.box = "horizontal",
        plot.title = element_text(colour="white", size = 11, lineheight = .01))

#### Some other things one can do ####

#Figure out what cluster people are located in and sort by cluster number
cl <- data.frame(names = fg$names, cluster = fg$membership, stringsasFactors =F)
cl <- cl[order(cl$cluster),]

#Identify central nodes within each cluster/community 
D$degree <- degree(network)
centralNodes <- lapply(communities(fg), function(x) x[which.max(D$degree[x])])
centralNames <- fg$names[unlist(centralNodes)] 

#Create new column of people's initials if desired for plotting purposes
for (i in 1:length(D$names)){
  z <- strsplit(D$names[i], " ")
  D$initials[i] <- paste(substring(z[[1]][1],1,1),substring(z[[1]][length(z[[1]])],1,1), sep='')
}

#Constrain the number of communities to plot based on the minumum number of people they need to contain
minC <- 10 #minimum cluster size
#Remove people that don't fit into a cluster with at least minC other people 
idx <- table(D$cluster) < minC
rmvL <- levels(D$cluster)[idx]
D <- D[D$cluster %ni% rmvL,]

#Or simply group small clusters into a single Misc cluster
largeClusters <- which(table(fg$membership)>=minC)
fg$membership[fg$membership %ni% largeClusters ]  <- "Misc"
D$cluster <- factor(fg$membership)


