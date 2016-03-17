#Updating R and maintaining package dependencies can be a bit of a pain in the ass
#Here are few things that can help

#BEFORE UPDATING R
#grab all currently installed packages
setwd('~/Desktop')
packages <- installed.packages()[,'Package']
save(packages, file = 'Rpackages')

#AFTER UPDATING R
#Install all not currently installed packages
setwd('~/Desktop')
load('Rpackages')
for(p in setdiff(packages,installed.packages()[,'Package'])){
  install.packages(p)
}

#Sometimes there are issues where R.app can't load packages that RStudio can
#If that's the case open R.app and run:
update.packages(checkBuilt = T, ask=F)

#The best solution is to actually move the folder location of installed packages to something outside of the R install. By default packages get installed in /Library/Frameworks/R.framework/Versions/Current/Resources/library

#This is kinda shitty because each new version of R will create a new version of this folder which then requires running the code above and/or manually copying all the packages over

#As of the time of this writing (9/28/15) the current OSX version is 10.8 which can't support any versions of R > 3.2.1
#For future R installs it's really best to create a new library folder, update R and install all packages into that folder
#This should preserve installed packages across versions of R, leaving all that needs to be done is update.packages above

#TO MOVE PACKAGE LIBRARY
#1) Create a file called ~/.Renviron
#2) Put this in the file: R_LIBS = ~/Rlibs (or whatver new location you want)


