#Random useful functions, some written, some "borrowed" from various places
#Source on startup

#Handy general purpose continuous color bar maker
#Create colorbar function
color.bar <- function(lut, min, max=-min, nticks=11, ticks=seq(min, max, len=nticks), title='') {
  scale = (length(lut)-1)/(max-min)

  dev.new(width=1.75, height=5)
  plot(c(0,10), c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main=title)
  axis(2, ticks, las=1)
  for (i in 1:(length(lut)-1)) {
    y = (i-1)/scale + min
    rect(0,y,10,y+1/scale, col=lut[i], border=NA)
  }
}
#usage:
#Create a continuous value colorramp based on values that are at least the min max ranges of the data
#library(colourschemes)
#colsRamp = rampInterpolate(limits=c(-4,4), ramp = c('deepskyblue','white','red'))
#Generate 100 values from that colorramp to make a color bar
#lut = colsRamp(seq(-4,4,length.out=100))
#Create the colorbar
#color.bar(lut, -4, 4,nticks=5,title = 'z-value')


#Variance inflation factor for lmer models
vif.mer= function (fit) {
  ## adapted from rms::vif

  v <- vcov(fit)
  nam <- names(fixef(fit))

  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }

  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}


readtxtFiles <- function(mypath,mypattern,direction){
  filenames <- list.files(path=mypath,pattern=mypattern,full.names = T)
  datalist <- lapply(filenames,function(x) read.table(file=x,header=T,sep="\t"))
  ifelse(direction=='r',
         return (do.call(rbind,datalist)),
         return (do.call(cbind,datalist)))
}

dpascal <- function(r,n,p) {
  c = choose(n-1,r-1)
  d = c * (p^r) * (1-p)^(n-r)
  return(d)
}

rcontrast<-function(t, df)
{r<-sqrt(t^2/(t^2 + df))
 print(paste("r = ", r))
}


omega_factorial <- function(n,a,b,SSa,SSb,SSab,SSr)
{
  MSa <- SSa/(a-1)
  MSb <- SSb/(b-1)
  MSab <- SSab/((a-1)*(b-1))
  MSr <- SSr/(a*b*(n-1))
  varA <- ((a-1)*(MSa-MSr))/(n*a*b)
  varB <- ((b-1)*(MSb-MSr))/(n*a*b)
  varAB <- ((a-1)*(b-1)*(MSab-MSr)/(n*a*b))
  varTotal <- varA + varB + varAB + MSr
  print(paste("Omega-Square A:   ", varA/varTotal))
  print(paste("Omega-Square B:   ", varB/varTotal))
  print(paste("Omega-Square AB:   ", varAB/varTotal))
}

## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  require(plyr)

  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }

  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )

  # Rename the "mean" column
  datac <- rename(datac, c("mean" = measurevar))

  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval:
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult

  return(datac)
}

#Repeated measures version of the same + normDatawithin helper function
normDataWithin <- function(data=NULL, idvar, measurevar, betweenvars=NULL,
                           na.rm=FALSE, .drop=TRUE) {
  library(plyr)
  # Measure var on left, idvar + between vars on right of formula.
  data.subjMean <- ddply(data, c(idvar, betweenvars), .drop=.drop,
                         .fun = function(xx, col, na.rm) {
                           c(subjMean = mean(xx[,col], na.rm=na.rm))
                         },
                         measurevar,
                         na.rm
  )
  # Put the subject means with original data
  data <- merge(data, data.subjMean)
  # Get the normalized data in a new column
  measureNormedVar <- paste(measurevar, "_norm", sep="")
  data[,measureNormedVar] <- data[,measurevar] - data[,"subjMean"] +
    mean(data[,measurevar], na.rm=na.rm)
  # Remove this subject mean column
  data$subjMean <- NULL
  return(data)
}

summarySEwithin <- function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL,
                            idvar=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {

  # Ensure that the betweenvars and withinvars are factors
  factorvars <- vapply(data[, c(betweenvars, withinvars), drop=FALSE],
                       FUN=is.factor, FUN.VALUE=logical(1))
  if (!all(factorvars)) {
    nonfactorvars <- names(factorvars)[!factorvars]
    message("Automatically converting the following non-factors to factors: ",
            paste(nonfactorvars, collapse = ", "))
    data[nonfactorvars] <- lapply(data[nonfactorvars], factor)
  }
  # Get the means from the un-normed data
  datac <- summarySE(data, measurevar, groupvars=c(betweenvars, withinvars),
                     na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  # Drop all the unused columns (these will be calculated with normed data)
  datac$sd <- NULL
  datac$se <- NULL
  datac$ci <- NULL
  # Norm each subject's data
  ndata <- normDataWithin(data, idvar, measurevar, betweenvars, na.rm, .drop=.drop)
  # This is the name of the new column
  measurevar_n <- paste(measurevar, "_norm", sep="")
  # Collapse the normed data - now we can treat between and within vars the same
  ndatac <- summarySE(ndata, measurevar_n, groupvars=c(betweenvars, withinvars),
                      na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  # Apply correction from Morey (2008) to the standard error and confidence interval
  #  Get the product of the number of conditions of within-S variables
  nWithinGroups    <- prod(vapply(ndatac[,withinvars, drop=FALSE], FUN=nlevels,
                                  FUN.VALUE=numeric(1)))
  correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )
  # Apply the correction factor
  ndatac$sd <- ndatac$sd * correctionFactor
  ndatac$se <- ndatac$se * correctionFactor
  ndatac$ci <- ndatac$ci * correctionFactor
  # Combine the un-normed means with the normed results
  merge(datac, ndatac)
}

mauchly_chi <- function(W, K, N){
  #W = Mauchly's W, e.g. from ezANOVA; K = number of levels of repeated measure; N= number of subs

  d <- 1 - ((2 * ((K - 1)^2)+(K - 1)+2)/(6*(K - 1)*(N - 1)))
  X <- -(N-1)*d*log(W)
  df <- (K*(K - 1)/2) - 1
  print(paste("Approx Chi-Square is: ", X))
  print(paste("With degrees of freedom: ",df))
}

#Kaiser-Meyer-Olkin measure of sampling adequacy
#Returns a value between 0 and 1 representing the ratio of corr to par corrs in data
#Looking for vals that are at least >= .5 but preferably >= .7 which indicates how reliable factor solutions will be
kmo <- function(x) {
  x <- subset(x, complete.cases(x))     # Omit missing values
  r <- cor(x)                           # Correlation matrix
  r2 <- r^2                             # Squared correlation coefficients
  i <- solve(r)                         # Inverse matrix of correlation matrix
  d <- diag(i)                          # Diagonal elements of inverse matrix
  p2 <- (-i/sqrt(outer(d, d)))^2        # Squared partial correlation coefficients
  diag(r2) <- diag(p2) <- 0             # Delete diagonal elements
  KMO <- sum(r2)/(sum(r2)+sum(p2))
  MSA <- colSums(r2)/(colSums(r2)+colSums(p2))
  return(list(KMO=KMO, MSA=MSA))
}

# KMO Kaiser-Meyer-Olkin Measure of Sampling Adequacy (alternate)
kmo2 = function( data ){

library(MASS)
X <- cor(as.matrix(data))
iX <- ginv(X)
S2 <- diag(diag((iX^-1)))

AIS <- S2%*%iX%*%S2                      # anti-image covariance matrix
IS <- X+AIS-2*S2                         # image covariance matrix
Dai <- sqrt(diag(diag(AIS)))
IR <- ginv(Dai)%*%IS%*%ginv(Dai)         # image correlation matrix
AIR <- ginv(Dai)%*%AIS%*%ginv(Dai)       # anti-image correlation matrix
a <- apply((AIR - diag(diag(AIR)))^2, 2, sum)
AA <- sum(a)
b <- apply((X - diag(nrow(X)))^2, 2, sum)
BB <- sum(b)
MSA <- b/(b+a)                        # indiv. measures of sampling adequacy

AIR <- AIR-diag(nrow(AIR))+diag(MSA)  # Examine the anti-image of the
# correlation matrix. That is the
# negative of the partial correlations,
# partialling out all other variables.

kmo <- BB/(AA+BB)                     # overall KMO statistic

# Reporting the conclusion
if (kmo >= 0.00 && kmo < 0.50){
  test <- 'The KMO test yields a degree of common variance unacceptable for FA.'

} else if (kmo >= 0.50 && kmo < 0.60){
  test <- 'The KMO test yields a degree of common variance miserable.'
} else if (kmo >= 0.60 && kmo < 0.70){
  test <- 'The KMO test yields a degree of common variance mediocre.'
} else if (kmo >= 0.70 && kmo < 0.80){
  test <- 'The KMO test yields a degree of common variance middling.'
} else if (kmo >= 0.80 && kmo < 0.90){
  test <- 'The KMO test yields a degree of common variance meritorious.'
} else {

  test <- 'The KMO test yields a degree of common variance marvelous.'     }

ans <- list( overall = kmo,
             report = test,
             individual = MSA)
return(ans)
}


#Check for sphericity because you want data to be non-sphericial to do factor analysis
#alternative is cortest.bartlett() in psych
Bartlett.test <- function(x) {
  method <- "Bartlett’s test of sphericity"
  data.name <- deparse(substitute(x))
  x <- subset(x, complete.cases(x))     # Omit missing values
  n <- nrow(x)
  p <- ncol(x)
  chisq <- (1-n+(2*p+5)/6)*log(det(cor(x)))
  df <- p*(p-1)/2
  p.value <- pchisq(chisq, df, lower.tail=FALSE)
  names(chisq) <- "X-squared"
  names(df) <- "df"
  return(structure(list(statistic=chisq, parameter=df, p.value=p.value,
                        method=method, data.name=data.name), class="htest"))
}

#Function to get color labels for dendrogram
colLab <- function(n) {
  if (is.leaf(n)) {
    a <- attributes(n)
    labCol <- labelColors[clusMember[which(names(clusMember) == a$label)]]
    attr(n, "nodePar") <- c(a$nodePar, lab.col = labCol)
  }
  n
}

#Create not in operator
`%ni%`  <- Negate(`%in%`)

#Create conversion between JAGS precision and std
prec.J <- function(s){
  return(1/(s^2))
}
sd.J <- function(p){
  sqrt(1/p)
}

#Function to invert R contrast matrix to contrast weights
contr.weights <- function(contr){
  contr <- cbind(intercept=1,contr)
  return(solve(contr))
}
