How Contrasts Work in R
================
Eshin Jolly (<eshin.jolly.gr@dartmouth.edu>)
Januaray 7, 2016

#### The purpose of this simulation is to illustrate how contrasts in R work and how they relate to how regression models are setup with categorical predictors.

*('betas' and 'parameter estimates' are used interchangeably throughout!)*
There are essentially 2 ways to think about contrasts:

-   Contrast matrix (coefficients): These determine how categorical predictors are specified within a regression framework (e.g. dummy, sum-to-zero, helmert, polynomial); think of this as a snapshot view of how levels of your variables are represented in your design matrix
-   Contrast weights: These are values multiplied by cell means to compute specific comparisons (e.g. linear trend, specific cell mean differences)

When dealing with categorical data in an ANOVA frameworks we need to keep in mind that behind the scenes, the real workhorse is still just a multiple regression. Usually there are two things we would like to know given this:
 1. What do specific parameter estimates in the regression computation represent?
 2. How do we make specific ANOVA cell mean comparisons using these parameter estimates?

In R, contrasts specifically refer to the way *levels of categorical predictors are coded*. Think of this as short hand for how to specify the design matrix columns for a specific categorical variable. We can use these to figure out what specific parameter estimates mean, and even parameterize the regression model to make specific cell mean comparisons *a priori*. It's not entirely obvious however how to relate regression model parameterization to specific cell mean comparisons in R. This overview shows how that's done.

#### Mock Experiment

Let's imagine a between subjects experiment looking at the differences in performance on an exam between 4 races of students (One-way ANOVA with 4 levels): White, Black, Indian, and Asian. First let's generate some random data such that Asian \> Black \> Indian \> White in performance.

``` r
x <- rnorm(20,5,2) + 5 
y <- rnorm(20,5,2)
z <- rnorm(20,5,2) + 10
t <- rnorm(20,5,2) + 2

C <- data.frame(Score=c(x,y,z,t), Group = c(rep("Black",20),rep("White",20),rep("Asian",20),rep("Indian",20)))
C$Group <- as.factor(C$Group)
```

Here is the mean score of each group:

    ##    Group Score.mean
    ## 1  Asian  14.899113
    ## 2  Black   9.322770
    ## 3 Indian   7.293698
    ## 4  White   4.372173

#### Dummy Coded Regression

Ok now let's specify a regression model using R's default treatment contrasts. Here are **what parameter estimates represent** in this scheme:

-   Intercept = mean for Asian students
-   B1 = mean of Black students - mean of Asian students (i.e. level 2 of IV - reference group)
-   B2 = mean of Indian students - mean of Asian students (i.e. level 3 of IV - reference group)
-   B3 = mean of White students - mean of Asian students (i.e. level 4 of IV - reference group)

This happens because R will automatically take the alphabetically first factor level as the reference group

    ## 
    ## Call:
    ## lm(formula = Score ~ Group, data = C)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.6816 -1.1552  0.1589  1.0313  4.8812 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  14.8991     0.3998  37.263  < 2e-16 ***
    ## GroupBlack   -5.5763     0.5655  -9.862 3.04e-15 ***
    ## GroupIndian  -7.6054     0.5655 -13.450  < 2e-16 ***
    ## GroupWhite  -10.5269     0.5655 -18.617  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.788 on 76 degrees of freedom
    ## Multiple R-squared:  0.8298, Adjusted R-squared:  0.8231 
    ## F-statistic: 123.5 on 3 and 76 DF,  p-value: < 2.2e-16

Let's see how R coded each level of the categorical predictor, i.e. the *contrast matrix*. This has also been referred to as a "basis matrix" and can be thought of as a snap-shot of the design matrix for these specific factor levels (just imagine a column of 1s prepended as a lefthand column). Rows are **levels of the IV** and columns are **parameters to be estimated**. The column headings just so happen to reflect names of our conditions because in the this coding scheme, because when added together with the intercept, the result is the mean for that group. The values in the rows tell us what values to plugin to the model to get means for the respective row labels ([ref](http://www.clayford.net/statistics/exploring-unordered-contrasts-in-r/)). For example, to get the cell mean for Asian, multiply the 3 calculated betas by 0. To summarize, these are the values to multiply the **parameter estimates** by, in order to calculate a specific group's mean.

    ##        Black Indian White
    ## Asian      0      0     0
    ## Black      1      0     0
    ## Indian     0      1     0
    ## White      0      0     1

Here is what each cell mean computation would look like by multiplying the betas by this contrast matrix. It's easiest to read this across rows, to see how each level, *k* of the categorical predictor is represented by *k-1* parameter estimates:

-   Asian = I + 0\*B1 + 0\*B2 + 0\*B3
-   Black = I + 1\*B1 + 0\*B2 + 0\*B3
-   Indian = I + 0\*B1 + 1\*B2 + 0\*B3
-   White = I + 0\*B1 + 0\*B2 + 1\*B3

Ok so we know how to recover cell means using the betas from the regression output. We can also intuit what change each individual beta represents but we can also figure this out more formally. To do this we can convert the *contrast matrix* to a matrix of *contrast weights*. In order to do this we need to take the *contrast matrix*, append a column for the intercept and invert it. First we'll write a function to do that so we can use it for the future.

``` r
#Inversion function
contr.weights <- function(contr){
  contr <- cbind(intercept=1,contr)
  return(solve(contr))
}
contr.weights(contrasts(C$Group))
```

    ##           Asian Black Indian White
    ## intercept     1     0      0     0
    ## Black        -1     1      0     0
    ## Indian       -1     0      1     0
    ## White        -1     0      0     1

This shows the *contrast weights* matrix of what each parameter estimate represents. Each row is a parameter estimate (the first being the intercept). Reading across the row tells you what combination of factor levels (cell means) are represented by that beta. The row labels below intercept are a bit confusing so feel free to ignore them. The important thing is to understand that each row is a parameter estimate, which is some linear combination of cell means. Just reiterating what's said above, here is what each beta means:

-   intercept = the reference group, i.e. mean of Asian students = 1\*Asian + 0\*Black + 0\*Indian + 0\*White
-   B1 = contrast 1, i.e. Black \> Asian = -1\*Asian + 1\*Black + 0\*Indian + 0\*White
-   B2 = contrast 2, i.e. Indian \> Asian = -1\*Asian + 0\*Black + 1\*Indian + 0\*White
-   B3 = contrast 3, i.e. White \> Asian = -1\*Asian + 0\*Black + 0\*Indian + 1\*White

#### Interim summary

Ok so we've seen 2 difference kinds of matrices regarding betas. How are they related?
Typing `contrasts(factor)` produces a *contrast matrix* which is a snapshot view of how factor levels are *coded* in the design matrix. This can be augmented and inverted to get what we've been calling *contrast weights*: the linear combination of factor levels each beta *represents* in the regression output. Understanding this difference is critical to understanding how to specify custom contrasts in R.

#### Sum-to-zero Coded Regression

Ok now let's specify a regression model using R's effect (sum-to-zero) contrasts. In this scheme:

-   Intercept = grand mean of all students
-   B1 = mean of Asian students - grand mean
-   B2 = mean of Black students - grand mean
-   B3 = mean of Indian students - grand mean

``` r
contrasts(C$Group) <- contr.sum(4)
summary(lm(Score~Group,data=C))
```

    ## 
    ## Call:
    ## lm(formula = Score ~ Group, data = C)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.6816 -1.1552  0.1589  1.0313  4.8812 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   8.9719     0.1999  44.877  < 2e-16 ***
    ## Group1        5.9272     0.3463  17.117  < 2e-16 ***
    ## Group2        0.3508     0.3463   1.013    0.314    
    ## Group3       -1.6782     0.3463  -4.847 6.49e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.788 on 76 degrees of freedom
    ## Multiple R-squared:  0.8298, Adjusted R-squared:  0.8231 
    ## F-statistic: 123.5 on 3 and 76 DF,  p-value: < 2.2e-16

Let's how factor levels are represented in the design matrix under this scheme:

    ##        [,1] [,2] [,3]
    ## Asian     1    0    0
    ## Black     0    1    0
    ## Indian    0    0    1
    ## White    -1   -1   -1

This coding scheme is common for factorial ANOVA designs because it makes typical main effects and interaction tests easier. It's also neccesary because to properly partition variance for SS III F-tests (i.e. orthogonal). Here's how we recover cell means using this *contrast matrix*:

-   Asian = I + 1\*B1 + 0\*B2 + 0\*B3
-   Black = I + 1\*B1 + 1\*B2 + 0\*B3
-   Indian = I + 0\*B1 + 0\*B2 + 1\*B3
-   White = I + -1\*B1 + -1\*B2 + -1\*B3

Once again let's convert this to matrix of *contrast weights* to see what the parameter estimates actually reflect:

``` r
contr.weights(contrasts(C$Group))
```

    ##           Asian Black Indian White
    ## intercept  0.25  0.25   0.25  0.25
    ##            0.75 -0.25  -0.25 -0.25
    ##           -0.25  0.75  -0.25 -0.25
    ##           -0.25 -0.25   0.75 -0.25

Now we can more clearly see the actual comparisons that the parameter estimates refelect. The specific weights multiplied by each *mean* indicate how comparison are made between each group level and the grand mean:

-   intercept = the grand mean = .25\*Asian + .25\*Black + .25\*Indian + .25\*White
-   B1 = contrast 1, i.e. Asian - grand mean = .75\*Asian + -.25\*Black + -.25\*Indian + -.25\*White
-   B2 = contrast 2, i.e. Black - grand mean = -.25\*Asian + .75\*Black + -.25\*Indian + -.25\*White
-   B3 = contrast 3, i.e. Indian - grand mean = -.25\*Asian + -.25\*Black + .75\*Indian + -.25\*White

#### Custom Coded Regression

What if we wanted to do specific pairwise comparisons between levels of our factor (i.e. specific cell mean comparisons)? We might think to directly specify a custom *contrast matrix* For example this:

-   Intercept = grand mean
-   B1 = mean of Asian students - mean White students
-   B2 = mean of Black students - mean Indian students
-   B3 = mean of Indian students - mean of White students

One might think to do it like this:

``` r
contrasts(C$Group) <- cbind(c(1,0,0,-1),c(0,1,-1,0),c(0,0,1,-1))
summary(lm(Score~Group,data=C))
```

    ## 
    ## Call:
    ## lm(formula = Score ~ Group, data = C)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.6816 -1.1552  0.1589  1.0313  4.8812 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   8.9719     0.1999  44.877  < 2e-16 ***
    ## Group1        5.9272     0.3463  17.117  < 2e-16 ***
    ## Group2        0.3508     0.3463   1.013  0.31420    
    ## Group3       -1.3274     0.3998  -3.320  0.00139 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.788 on 76 degrees of freedom
    ## Multiple R-squared:  0.8298, Adjusted R-squared:  0.8231 
    ## F-statistic: 123.5 on 3 and 76 DF,  p-value: < 2.2e-16

Ehh, comparing this to cell means doesn't look like this gave us what we want. Instead it gives us:

-   Intercept = grand mean
-   B1 = mean of Asian students - grand mean
-   B2 = mean of Black students - grand mean
-   B3 = mean of Black and Indian students - mean of Asian and White students

See:

``` r
contr.weights(contrasts(C$Group))
```

    ##           Asian Black Indian White
    ## intercept  0.25  0.25   0.25  0.25
    ##            0.75 -0.25  -0.25 -0.25
    ##           -0.25  0.75  -0.25 -0.25
    ##           -0.50  0.50   0.50 -0.50

Notice what happened here: we operated on the *contrast matrix* when we intended to operate on the *contrast weights*. Don't confuse customizing what **parameter estimates represent** with **how factor levels are coded**. This is the essential difference between the two types of matrices in R. When specifying our desired comparisons as above, we were treating the *contrast coefficients* as if they were *contrast weights*. This can be dangerous because one might assume that their parameter estimates and t-tests are reflecting a comparison that they may not be. In order to correctly specify any kind of desired comparison (i.e. orthogonal or non-orthogonal) we need to make a contrast weight matrix and convert it to a contrast coefficient matrix ourselves. Here's how:

First specify an empty k x k matrix.

``` r
tmp <- matrix(NA,4,4) 
```

In this matrix rows are going to correspond to the intercept followed by k-1 desired comparisons, and columns are going to correspond to levels of the factor (Asian, Black, White). In this case we're making the intercept the grand mean, by filling the row with 1/k.

``` r
tmp[1,] <- 1/4 #Intercept 
tmp[2,] <- c(1,0,0,-1) #Asian > White
tmp[3,] <- c(0,1,-1, 0) #Black > Indian
tmp[4,] <- c(0,0,1,-1) #Indian > White
tmp
```

    ##      [,1] [,2]  [,3]  [,4]
    ## [1,] 0.25 0.25  0.25  0.25
    ## [2,] 1.00 0.00  0.00 -1.00
    ## [3,] 0.00 1.00 -1.00  0.00
    ## [4,] 0.00 0.00  1.00 -1.00

Here we can see the contrast weight matrix that we want, which correctly specifies our desired comparisons:

-   intercept = the grand mean = .25\*Asian + .25\*Black + .25\*Indian + .25\*White
-   B1 = contrast 1, i.e. Asian \> White = 1\*Asian + 0\*Black + 0\*Indian + -1\*White
-   B2 = contrast 2, i.e. Black \> Indian = 0\*Asian + 1\*Black + -1\*Indian + 0\*White
-   B3 = contrast 3, i.e. Indian \> White = 0\*Asian + 0\*Black + 1\*Indian + -1\*White

Now we can convert it and see the contrast coefficient matrix that R needs to make this happen.

``` r
tmp <- solve(tmp) #invert matrix
tmp <- tmp[,2:4] #Drop intercept and pass this to contrasts
contrasts(C$Group) <- tmp
contrasts(C$Group)
```

    ##         [,1]  [,2] [,3]
    ## Asian   0.75 -0.25 -0.5
    ## Black  -0.25  0.75  0.5
    ## Indian -0.25 -0.25  0.5
    ## White  -0.25 -0.25 -0.5

These are the *contrast coefficients* from the *contrast matrix* that R is using to represent the levels of the categorical variable in a particular way that allows us to perform our desired comparisons (i.e. specify exactly what our *betas represent*). To calculate cell means under this coding scheme here's what we would do:

-   Asian = I + .75\*B1 + -.25\*B2 + -.5\*B3
-   Black = I + -.25\*B1 + .75\*B2 + .5\*B3
-   Indian = I + -.25\*B1 + -.25\*B2 + .5\*B3
-   White = I + -.25\*B1 + -.25\*B2 + -.5\*B3

Let's finally run the model

    ## 
    ## Call:
    ## lm(formula = Score ~ Group, data = C)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.6816 -1.1552  0.1589  1.0313  4.8812 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   8.9719     0.1999  44.877  < 2e-16 ***
    ## Group1       10.5269     0.5655  18.617  < 2e-16 ***
    ## Group2        2.0291     0.5655   3.588 0.000587 ***
    ## Group3        2.9215     0.5655   5.167 1.86e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.788 on 76 degrees of freedom
    ## Multiple R-squared:  0.8298, Adjusted R-squared:  0.8231 
    ## F-statistic: 123.5 on 3 and 76 DF,  p-value: < 2.2e-16

Now our parameter estimates reflect the specific pairwise comparisons we wanted. Because this *contrast matrix* also sums to zero, it would be an adequate parameterization for proper SS III ANOVA analyses. The key thing to notice is how we specified our desired contrasts. It's the same way we were trying to do so before, but this time we supplied them to the correct matrix of contrast weights and not the matrix of contrast coefficients.

#### Takeaways

-   There are infinite ways to specify contrasts with categorical predictors, but for a predictor with *k* levels, only *k-1* are possible to specify in a single non-over parameterised regression model at one time. R will help specify these possible contrasts according to specific coding schemes (e.g. treatment, sum, poly) or will help figure out other possible contrasts if provided \< k-1 comparisons
-   Using the square matrix inversion approach we can easily move between the two matrices of *contrast coefficients* or *contrast weights* to specify/examine how levels are coded, or what comparisons parameter estimates are reflecting, respectively
-   In a balanced design with no interacting factors like this, an F-test (ANOVA) on Group would produce the same result irrespective of the coding scheme utilized. This would change if the groups were unbalanced or interactions were involved. To obtain valid type III SS ANOVA results in these situation, categorical predictors **have** to be coded using a sum-to-zero or orthogonal scheme
-   When specifying custom contrasts to R directly, the sign of each level will dictate the nature of the comparison being made, irrespective of the weights being used. In other words, giving R [-1,1] or [-10,10] will result in the same hypothesis test and t-statistic; the parameter estimates and s.e. will of course be different
-   It's useful to specify the desired weights using the inversion method above, because this will determine the correct values R (and your design matrix generally) needs to make a parameter estimate reflect the desired test. For example, a mean difference between 2 levels, is specified as [1,-1] in the *contrast weights* matrix, but [.5,-.5] or [1 ] in the *contrast coefficients* matrix
-   Specifying the correct weights/coefficients is critical when desired tests are not statistically independent of each other. If they are misspecified R will still produce a model, but this model will not be performing the desired comparisons
