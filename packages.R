#Various packages used with super brief purposes

library("ggplot2") #for plotting
library("lawstat") #for levene's test
library("chemometrics") #for sd_trim
library("car") #particularly Anova for using type III SS and Levene's test
library("reshape") #for easier casting (data transforming) function
library("reshape2") #for melting and casting
library("psych") #for describe() and principal components analysis
library("pastecs") #for stat.desc, general descriptive stats
library('bootES') #for better bootstrapping
library('foreign') #for loading in spss and other file types
library('ppcor') #for partial and semi-partial correlations
library('QuantPsyc') #for lm.beta to get standardized betas of predictor vars in lm
library('HH') #for vif or variance inflation factor calculation of colinear predictors in lm
library('pwr') #for power estimates
library('compute.es') #effect sizes
library('ez') #ezANOVA (SPSS style) and visualization
library('effects') #for adjusted means when running ancova, using effect()
library('multcomp') #for post hoc tests
library('MASS') #stepAIC for stepwise regression, discriminant function analysis, non-metric MDS
library('nlme') #linear mixed modeling, and modeling cov structure with gls
library('lme4') #Better linear mixed modeling- literally the best
library("mvnormtest") #testing multivariate normality
library("mvoutlier") #plots for multivariate outliers
library("scatterplot3d") #3d plots
library("pROC") #for sensitivity/specificity curves
library("klaR") #for greedy wilks
library("leaps") #for regular subsets, model selection
library("GPArotation") #for rotation of factor loadings after PCA
library("corpcor") #for bartlett's test of multivariate sphericity and polychor() when dealing with dichotomous vars
library("lmerTest") #sig values by saitterwaite approximation for linear mixed models
library("pvclust") #calculate boostrapped p-vals for hierarchical clustering
library("corrplot") #Awesome visualization for correlation matrices
library('fpc') #For advanced clustering
library("coin") #permutation testing
library("pracma") #repmat function, like MATLAB
library("plyr") #lots of data frame subsetting,applying,transposing, also has arrange
library("BEST") #bayesian testing based on John Kruschke book
library("BayesianFirstAid") #common tests done with bayesian estimation
library("beep") #It's a beep sound!
library("signal") #Matlab like signal processing
library("exactRankTests") #non-parametric tests
library("Rfacebook") #facebook R api
library("igraph") #Network graph visualization
library('clickme') #basic interactive graphs
library('plotly') #more graphing options/interactives for interfacing with plotly online
library('ggvis') #more interactive ggplots
library('animint') #Convert ggplot objects to java's d3
library('sna') #social network analysis tools and visualization
library('maps') #Geographic mat info
library("cocor") #comparing correlations
library("nnet") #Multinomial regression
library("glmnet") #GLM with some penalization/regularization
library("mlogit") #different kinds of logit models
library("broom") #tidy, glance and augment functions for nicer model outputs as dataframes
library("rjags")
library("R2Jags") #Two libraries for using Jags for Bayesian MCMC estimates via Gibbs sampling
library("coda") #Diagnostics and plots on MCMC models from Jags
library("mcmcplots") #more MCMC plotting
library("LearnBayes") #basic bayesian stats functions
library("pscl") #zeroinflated regression
library("lsr") #cohensD effect size
library("EMT") #Exact multinomial test
library("doBy") #Easier summary stats by groups/factors
library("tm") #text mining
library("wordcloud") #make a word cloud
library("magrittr") #piping operations to make R syntax more readable, e.g %>%
library("sjPlot") #plotting mixed effects models from lmer
library('tm') #textmining functionallity
library('twitteR') #interfacing with twitter API
library('SnowballC') #for stemming words using tm
library('mediation') #mediation analyses, esp for mixed models
library('ggmap') #ggplot like syntax for maps; theme_nothing() for ggplot
library('MBESS') #confidence intervals on effect sizes
library('colourscheme') #rampInterpolate func for color ramp generation