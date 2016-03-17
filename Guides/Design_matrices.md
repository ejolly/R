How different tests look as design matrices
================
Eshin Jolly (<eshin.jolly.gr@dartmouth.edu>)
January 13, 2016

### The purpose of this guide is to understand and be able to read design matrices, i.e. the language of linear algebra behind regression models.

Typically a design matrix (X) has a structure where each row = a data point and each column = an independent predictor (IV). Of these the first column (usually denoted by 1's) is considered the "intercept." This matrix is multiplied by a vector of parameter estimates (betas) to predict a DV. During the regression operation, these betas are computed via a particular algorithm (e.g. OLS). The values in the columns of X can be continuous variable values, or category codes when an IV is not a continuous variable. For any given categorical IV, there are only k-1 orthogonal "contrast codes" that can be specified without overparameterizing a regression model. It's also not necessary to specificy *all* k-1 possible category codes, in case for example only a subset of possible contrasts are desired.

#### Math behind regression

\[\beta=(X^T X)^{-1}X^TY\]

``` r
Y <- dv
X <- model.matrx( ~ IV1 + IV2...)
solve(t(X) %*% X) %*% t(X) %*% Y
```

#### Independent sample t-test (N=5; each group has 5 subjects)

##### Dummy coded

Here the first column (intercept) is the mean for group 2 because the mean for group 1 (first 5 rows) is explicity modeled. This is also why the second regressor is the difference between group 1 and group 2.

    ##       G2 G1 - G2
    ##  [1,]  1       1
    ##  [2,]  1       1
    ##  [3,]  1       1
    ##  [4,]  1       1
    ##  [5,]  1       1
    ##  [6,]  1       0
    ##  [7,]  1       0
    ##  [8,]  1       0
    ##  [9,]  1       0
    ## [10,]  1       0

Contrast for the mean of group 1 = [1 1]
Contrast for the mean of group 2 = [1 0]
Mean difference = [1 1] - [1 0] = [0 1]

##### Sum-to-zero coded

Here the first column is the grand mean and the second column is deviation from the grand mean for group 1. Deviation from the grand mean == half the mean difference between groups.

    ##       Grand Mean G1 - GM
    ##  [1,]          1       1
    ##  [2,]          1       1
    ##  [3,]          1       1
    ##  [4,]          1       1
    ##  [5,]          1       1
    ##  [6,]          1      -1
    ##  [7,]          1      -1
    ##  [8,]          1      -1
    ##  [9,]          1      -1
    ## [10,]          1      -1

Contrast for the mean of group 1 = [1 1]
Contrast for the mean of group 2 = [1 -1]
Mean difference = [1 1] - [1 -1] = [0 2]

#### Paired sample t-test (N=5)

A paired t-test can be thought of running an independent t-test on mean-centered pairs of data points. Mean centering reduces the residuals while keeping the overall mean difference unaffected. So the design matrix needs to include regressors for the mean of each pair of data points.
Here each pair of rows are data points from the same source (e.g. subject). The first column is no longer an intercept but the actual mean difference we're interested in. All the other regressors are the means for each pair of data points, an no inferences should be conducted on them.

    ##       Mean Diff MS1 MS2 MS3 MS4 MS5
    ##  [1,]         1   1   0   0   0   0
    ##  [2,]        -1   1   0   0   0   0
    ##  [3,]         1   0   1   0   0   0
    ##  [4,]        -1   0   1   0   0   0
    ##  [5,]         1   0   0   1   0   0
    ##  [6,]        -1   0   0   1   0   0
    ##  [7,]         1   0   0   0   1   0
    ##  [8,]        -1   0   0   0   1   0
    ##  [9,]         1   0   0   0   0   1
    ## [10,]        -1   0   0   0   0   1

Contrast for the mean difference of conditions = [1 0 0 0 0 0]

### One-way ANOVA (1x4 and N=8; 2 subjects in each condition)

#### Cell means setup

**Super simple for tests on individual betas, and for comparing cell means. Trickier for traditional ANOVA style F tests**
In this type of setup each level of the IV gets its own indicator variable, i.e. betas = group means. A good rule of thumb is: if each column contains **only** 0s and 1s, and each row has a single 1, then the design matrix is of this form where regressors can be thought of as representing means of each level of the IV. This parameterization is only possible if the intercept in **not** modeled, otherwise it would be overparameterized. Here each pair of rows is two sample from that level of the IV.

    ##      A1 A2 A3 A4
    ## [1,]  1  0  0  0
    ## [2,]  1  0  0  0
    ## [3,]  0  1  0  0
    ## [4,]  0  1  0  0
    ## [5,]  0  0  1  0
    ## [6,]  0  0  1  0
    ## [7,]  0  0  0  1
    ## [8,]  0  0  0  1

Individual betas represent means of each level of the IV (cell means).
Example comparisons: Contrast for the mean of group 2 - mean of group 3 = [0 1 -1 0]
Traditional F-test: H0 = m1 = m2 = m3 = m4, we can rewrite this as:
m1-m4 = m2-m4 = m3-m4 = 0, and vstack the contrasts in a matrix form =
[1 0 0 -1
 0 1 0 -1
 0 0 1 -1]
 There are other ways to specify to this contrast matrix to get the F-test too.

#### Sum-to-zero (factor effects) setup

**Good for traditional ANOVA style analyses, individual betas are harder to interpret**
In this setup the first column is the intercept which represents the overall or grand mean of all factor levels. There are k-1 remaining columns, where k is the number of levels of the categorical variable. Rows in each of these columns are coded with 1 or 0 such that each level is represented in the design matrix once, **except** for one of the levels which is always coded as a -1. Because the intercept is the overall mean, this last level is coded as -1 to prevent the model from being overparameterized (i.e. uneccesarily representing that level's mean). Here each pair of rows is two sample from that level of the IV.

    ##      Grand Mean A1-GM A2-GM A3-GM
    ## [1,]          1     1     0     0
    ## [2,]          1     1     0     0
    ## [3,]          1     0     1     0
    ## [4,]          1     0     1     0
    ## [5,]          1     0     0     1
    ## [6,]          1     0     0     1
    ## [7,]          1    -1    -1    -1
    ## [8,]          1    -1    -1    -1

Individual betas are less obvious to understand but represent deviations from the grand mean.
Example contrasts: Contrast for the mean of group 1 - mean of group 4 =
[1 1 0 0] - [1 -1 -1 -1] = [0 2 1 1]
Main effect F-test, use contrasts that are part of that F-test:
[0 1 0 0
 0 0 1 0
 0 0 0 1]
Because only betas 2-4 are involved in the F-test (b1 = grand mean)

#### Dummy-coded setup

**Most common setup for categorical predictors (typical software defaults). Same pros and cons as cell means parameterization.**
In this setup the first column is the intercept which represents a specific factor level mean. All other columns represent mean differences between that reference factor level and the respective factor level being representing.

    ##      Ref Level A2-Ref A3-Ref A4-Ref
    ## [1,]         1      0      0      0
    ## [2,]         1      0      0      0
    ## [3,]         1      1      0      0
    ## [4,]         1      1      0      0
    ## [5,]         1      0      1      0
    ## [6,]         1      0      1      0
    ## [7,]         1      0      0      1
    ## [8,]         1      0      0      1

Example contrasts: Contrast for the mean of group 2 - mean of group 1 (reference) = [0 1 0 0]
Comparison of group 4 - group 2 = [1 0 0 1] - [1 1 0 0] = [0 -1 0 1]
Traditional F-test: H0 = m1 = m2 = m3 = m4, we can rewrite this as:
m1-m4 = m2-m4 = m3-m4 = 0, and vstack the contrasts in a matrix form =
[1 0 0 -1
 0 1 0 -1
 0 0 1 -1]
There are other ways to specify to this contrast matrix to get the F-test too.

### Two-way ANOVA (2x3 and N=12; 2 subjects in each condition)

#### Sum-to-zero (factor effects) setup

**Here the benefit of this parameterization for F-tests really shines.**
Remember the first column is the intercept which represents the overall or grand mean of all factor levels. There are kA-1 + kB-1 + (kA-1 \* kB-1) remaining columns, where k is the number of levels of each categorical variable as (1, 0, or -1). Interactions are formed simply by multiplying codes of factor levels with each other.
Here each pair of rows is two sample from that level of the IV. Columns 2-3 are codes for factor A; 4 codes for factor B and 5-6 are codes for the interaction between factors (columns 2`*`4 and 3`*`4)

    ##       GM A1 - GM A2-GM B1-GM A1*B1-GM A2*B1-GM
    ##  [1,]  1       1     0     1        1        0
    ##  [2,]  1       1     0     1        1        0
    ##  [3,]  1       1     0    -1       -1        0
    ##  [4,]  1       1     0    -1       -1        0
    ##  [5,]  1       0     1     1        0        1
    ##  [6,]  1       0     1     1        0        1
    ##  [7,]  1       0     1    -1        0       -1
    ##  [8,]  1       0     1    -1        0       -1
    ##  [9,]  1      -1    -1     1       -1       -1
    ## [10,]  1      -1    -1     1       -1       -1
    ## [11,]  1      -1    -1    -1        1        1
    ## [12,]  1      -1    -1    -1        1        1

Individual betas are even less obvious to understand but still represent deviations from the grand mean. The elegance comes because summing factor level deviations from the grand mean is equivalent to computing the main effect (half the mean difference of factor levels).
Example contrasts: Main effect of A:
[0 1 0 0 0 0
 0 0 1 0 0 0]
Main effect of B: [0 0 0 1 0 0]
Interaction:
[0 0 0 0 1 0
 0 0 0 0 0 1]
Cell means are easy too, cell mean of A1B1:
[1 1 0 1 1 0]

#### fMRI coded 2x2 (Num trials=12, 3 in each cell and 3 'baseline')

**This is common in fMRI designs where 'baseline' is another factor level modeling the intercept.**
The first column is the intercept which represents baseline in an event-related fMRI design. Because of this each other column represents a factor level in the ANOVA, similar to the cell means parameterization. So column 2 = A1B2, 3 = A2B1, 4 = A1B2, 5 = A2B2. In this parameterization, everything is referenced to the baseline and the cells of the ANOVA are sort of treated as a "collapsed" One-way. This makes it easy to test main effects and interactions using contrasts later. This coding scheme is essentially a dummy-coding scheming where the reference is conveniently informative and independent of the other conditions (i.e. baseline/fixation).

    ##       baseline A1B1-b A2B1-b A1B2-b A2B2-b
    ##  [1,]        1      0      0      0      0
    ##  [2,]        1      0      0      0      0
    ##  [3,]        1      0      0      0      0
    ##  [4,]        1      1      0      0      0
    ##  [5,]        1      1      0      0      0
    ##  [6,]        1      1      0      0      0
    ##  [7,]        1      0      1      0      0
    ##  [8,]        1      0      1      0      0
    ##  [9,]        1      0      1      0      0
    ## [10,]        1      0      0      1      0
    ## [11,]        1      0      0      1      0
    ## [12,]        1      0      0      1      0
    ## [13,]        1      0      0      0      1
    ## [14,]        1      0      0      0      1
    ## [15,]        1      0      0      0      1

Individual betas represent cell-mean difference relative to the reference (baseline).
Cell mean contrasts are simple:
A1B1 = [0 1 0 0 0]
A1B1 - A2B2 = [0 1 0 0 0] - [0 0 0 0 1] = [0 1 0 0 -1]
So are main effects:
Main effect of A:
[0 1 0 1 0] - [0 0 1 0 1] = [0 1 -1 1 -1]
Main effect of B:
[0 1 1 0 0] = [0 0 0 1 1 ] = [0 1 1 -1 -1]
Interaction:
[0 1 -1 1 -1] \* [0 1 1 -1 -1] = [0 1 -1 -1 1]
