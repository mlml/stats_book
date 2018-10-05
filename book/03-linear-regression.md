


# Linear regression

**Preliminary code**

This code is needed to make other code below work:

```r
library(gridExtra) # for grid.arrange() to print plots side-by-side
require(languageR)
library(dplyr)
library(ggplot2)
library(arm)

## loads alternativesMcGillLing620.csv from OSF project for Wagner (2016) data
alt <- read.csv(url("https://osf.io/6qctp/download"))

## loads french_medial_vowel_devoicing.txt from OSF project for Torreira & Ernestus (2010) data
df <- read.delim(url("https://osf.io/uncd8/download"))

## loads halfrhymeMcGillLing620.csv from OSF project for Harder (2013) data
halfrhyme <- read.csv(url("https://osf.io/37uqt/download"))
```
<!-- TODO later: just wrap this into actual text -->

<!-- Old links: -->
<!-- alt <- read.delim('datasets/alternatives.txt') -->
<!-- df <- read.delim('datasets/french_medial_vowel_devoicing.txt') -->

<script src="js/hideOutput.js"></script>

**Note**: Most answers to questions/exercises not listed in text are in [Solutions](#c2solns).

This chapter introduces linear regression. The broad topics we will cover are:

1. Regression: general introduction

2. Simple linear regression

3. Multiple linear regression

4. Linear regression assumptions, model criticism, and interpretation

5. Model comparison

## Regression: General introduction

First, what is "regression"?    @chatterjee2012regression define it as  "a conceptually simple method for investigating functional relationships among variables":

* The variable to be explained is called the *response*, often written $Y$
    
* The explanatory variables are called *predictors*, often written $X_1, X_2, \ldots$.
    
Here is an example of a study that uses regression analysis:

<center>
![](images/cow_milk.png)
</center>

> **Questions**:
>
> * What is the response variable in this study?
>
> * What could be a predictor variable?

### Linear models

The relationship between variables is captured by a regression *model*:
\begin{equation*}
  Y = f(X_1, X_2, ..., X_p) + \epsilon
\end{equation*}

In this model, $Y$ is approximated by a function of the predictors, and the difference between the model and reality is called the *error* ($\epsilon$).

Throughout this book we will be dealing exclusively with *linear models* (including "generalized linear models", like logistic regression), where the model takes the form:

\begin{equation}
Y = \beta_0 + \beta_1 X_1 + \beta_2 X_2 + \cdots + \beta_p X_p + \epsilon
    (\#eq:linreg1)
\end{equation}

The $\beta$'s are called *regression coefficients*.

This turns out to be an extremely general class of model, which can be applied to a wide range of phenomena. Some important kinds of models that don't appear at first glance to fit Equation \@ref(eq:linreg1)  can be linearized into this form.

### Terminology

We will consider two broad types of regression in this book:

1. *linear regression*, where the response ($Y$) is a continuous variable.  An example would be modeling reaction time (`RTlexdec`) as a function of word frequency (`WrittenFrequency`) for [the `english` dataset](#engdata).

2. Later we will consider *logistic regression*, where the response ($Y$) is binary: 0 or 1.  An example would be modeling whether tapping occurs or not (`tapping`) as a function of `vowelDuration` and `speakingRate` in [the `tapping` dataset](#tapdata).

Regression with just one predictor is called *simple*, while regression with multiple predictors is called *multiple*. The two examples just given would be "simple linear regression" and "multiple logistic regression".

Predictors can be *continuous*, such as milk consumption, or *categorical*---also called "factors"---such as participant gender, or word type.

Certain special cases of linear models that are common go by names such as:

* *Analysis of variance*: continuous $Y$, categorical predictors. (Models variability between groups)

* *Analysis of covariance*: continuous $Y$, mix of categorical and continuous predictors.

We are not covering these cases in this book, but ANOVAs (and ANCOVAs) are widely used in language research, and you may have seen them before. ANOVAs can be usefully thought of as just a **special case** of regression, as discussed in @vasishth2011foundations, @levy2012probabilistic, @gelman2007data.  Once you understand linear regression well, understanding ANOVA analyses is relatively straightforward.


### Steps and assumptions of regression analysis

Regression analyses have five broad steps, as usefully discussed by @chatterjee2012regression:

1. Statement of the problem
    * Ex: Does milk consumption affect height gain?
    
2. Selection of potentially relevant variables
    * Ex: Height gain, daily milk consumption, age, and sex.
    
3. Data collection
    * Ex: data from an existing database.
    
4. Model specification & fitting
    * Ex: height gain = $\beta_0 + \beta_1 \cdot$ milk consumption $+ \beta_2 \cdot$ sex $+$ error
    
5. Model validation and criticism


It is important to remember that the **validity of a regression analysis depends on the assumptions of the data and model**.  

For example, if you are modeling data where $Y$ has a maximum value, fitting a simple linear regression (= a line) to this data doesn't make conceptual sense, a priori.  Here's an example using the `english` dataset:


```r
library(languageR) ## makes the 'english' dataset available

ggplot(aes(x = Familiarity, y = CorrectLexdec), data = english) + geom_point(, alpha=0.1) + geom_smooth( method='lm')
```

<img src="03-linear-regression_files/figure-html/unnamed-chunk-2-1.png" width="480" style="display: block; margin: auto;" />

Because `CorrectLexdec` has a maximum value of 30, fitting a line doesn't make sense---the predicted value when `Familiarity`=6 is above 30, but this is impossible given the definition of `CorrectLexdec`.
    
For now, we will not go into what the assumptions of linear regression are, and just assume that they are met.  After introducing simple and multiple linear regressions, we'll come back to this issue in Section \@ref(linear-regression-assumptions):

* What are the assumptions (of linear regression)?
    
* For each assumption, how do we determine whether it's valid?
    
* How much of a problem is it, and what can be done, if the assumption is not met?

Note that R almost never checks whether your data and model meet regression analysis assumptions, unlike other software (e.g. SPSS, sometimes).

## Simple linear regression

The simplest application of simple linear regression (SLR) is to model an association between two continuous variables. 

#### Example: `english` data, young participants only {-}

* $X$: `WrittenFrequency` (= predictor)

* $Y$: `RTlexdec` (= response)
    

```r
young <- filter(english, AgeSubject=='young')

ggplot(young, aes(WrittenFrequency, RTlexdec)) +
  geom_point(size=0.5)
```

<img src="03-linear-regression_files/figure-html/unnamed-chunk-3-1.png" width="480" style="display: block; margin: auto;" />

<!-- TODO later: when "previous chapters" put into book, add this back in: -->
<!-- In previous chapters, we have described this kind of association using a _correlation -->

One way to describe this kind of association which you may be familiar with is a _correlation_ coefficient which gives us two types of information about the relationship:

1. *direction*: $r = -0.434$ 
    * $\implies$ negative relationship 
    
2. *strength*: $r^2 = 0.189$ 
    * $\implies$ weak relationship ($0 \le r^2 \le 1$)

A simple linear regression gives a *line of best fit*:


```r
ggplot(young, aes(WrittenFrequency, RTlexdec)) +
  geom_point(size=0.5) + 
  geom_smooth(method="lm", se=F)
```

<img src="03-linear-regression_files/figure-html/unnamed-chunk-4-1.png" width="480" style="display: block; margin: auto;" />

which gives some information not captured by a correlation coefficient:

* Prediction of $Y$ for a given $X$
    
* Numerical description of relationship
    
Regression gives both types of information, and more.

### SLR: Continuous predictor

The formula for simple linear regression, written for the $i^{\text{th}}$ observation, is:

\begin{equation}
  y_i = \underbrace{\beta_0}_{\text{intercept}} + \underbrace{\beta_1}_{\text{slope}} x_i + \epsilon_i
  (\#eq:linreg2)
\end{equation}

In this expression:

* $\beta_0, \beta_1$ are *coefficients*

* For the $i^{\text{th}}$ observation
    * $x_i$ is the value of the *predictor*

    * $y_i$ is the value of the *response*

    * $\epsilon_i$ is the value of the *error* (or *residual*)


This is our first linear model of a random variable ($Y$) as a function of a predictor variable ($X$).  The actual model, written not for individual observations, is written:

$$
  Y = \beta_0 + \beta_1 X + \epsilon
$$

That is, we use notation like $X$ for a variable, and notation like $x_5$ for actual values that it takes on.

For example, for [the `english` dataset](#engdata), with $Y$ = `RTlexdec` and  $X$ = `WrittenFrequency`:
    

```r
head(dplyr::select(english, WrittenFrequency, RTlexdec))
```

```
##   WrittenFrequency RTlexdec
## 1         3.912023 6.543754
## 2         4.521789 6.397596
## 3         6.505784 6.304942
## 4         5.017280 6.424221
## 5         4.890349 6.450597
## 6         4.770685 6.531970
```

* $x_2$=4.5217886 (predictor value for second observation)

* $y_1$=6.5437536 (response value for first observation)

> **Question**
>
> * What is $y_5$?

### SLR: Parameter estimation

To get a line of best fit, we want: $\beta_0$ and $\beta_1$, the *population values*.  Recall that we can't actually observe these (Sec.\@ref(sample-population)), so we obtain *sample estimates*, written $\hat{\beta}_0, \hat{\beta}_1$.

Once sample estimates are specified, Equation \@ref(eq:linreg2) gives *fitted values* for each observation, written $\hat{y}_i$:
$$
  \hat{y}_i = \hat{\beta}_0 + \hat{\beta}_1 x_i
$$

Note that there are no residuals in this equation---$\epsilon_i$ are again population values, which we can't observe. Our estimates of the residuals, given an estimated line of best it, are written $e_i$ (*error*):
$$
  e_i = y_i - \hat{y}_i
$$


This diagram shows the relationship between some of these quantities, for a single observation:

<center>
![](images/slr_continuous_model.png){width=600}
</center>

Our goal is to find coefficient values that minimize the difference between observed and expected values---the magnitudes of error ($|e_i|$), which are minimized by minimizing the squared errors ($e_i^2$).

We choose $\hat{\beta}_0$, $\hat{\beta}_1$ that minimize the sum of the $e_i^2$; these are called the *least-squares estimates*.

One useful property of the resulting regression line is that it always passes through the point (mean($X$), mean($Y$)).   A consequence is that SLR is easily thrown off by observations which are outliers in $X$ or $Y$ (why?).

#### Example {-}

Here is an example of fitting an SLR model in R, of reaction time vs. frequency for young speakers in the `english` dataset:


```r
young <- filter(english, AgeSubject == "young")
m <- lm(RTlexdec ~ WrittenFrequency, young)
```

The model output is:

```r
m
```

```
## 
## Call:
## lm(formula = RTlexdec ~ WrittenFrequency, data = young)
## 
## Coefficients:
##      (Intercept)  WrittenFrequency  
##          6.62556          -0.03711
```

The interpretation of the two coefficients is:

* $\beta_0$: the predicted $Y$ value when $X = 0$
    
    ```r
    m$coefficients[1]
    ```
    
    ```
    ## (Intercept) 
    ##    6.625556
    ```
    
* $\beta_1$: predicted change in $Y$ for every unit change in $X$
    
    ```r
    m$coefficients[2]
    ```
    
    ```
    ## WrittenFrequency 
    ##      -0.03710692
    ```

The regression line is:
\begin{equation}
\hat{y}_i = \hat{\beta}_0 + \hat{\beta}_1 x_i
\end{equation}

In terms of the variables used in this example:
\begin{equation}
  \text{Predicted RTlexdec}_i = 6.625 - 0.037 \cdot \text{WrittenFrequency}_i
\end{equation}

> **Questions**:
>
> What is the predicted `RTlexdec` when:
>
> * `WrittenFrequency` = 5?
>    
> * `WrittenFrequency` = 10?

<!-- 6.44, 6.25 -->
    
### Hypothesis testing

The least-squared estimators are normally-distributed.  These are sample estimates, so we can also approximate the standard errors of the estimators:
\begin{equation*}
  SE(\hat{\beta}_0) \quad SE(\hat{\beta}_1)
\end{equation*}

and apply $t$-tests to test for significance and obtain confidence intervals (CI) for the coefficients.

In particular, we are testing the null hypotheses of no relationship:

* $H_0~:~\beta_1 = 0$

(and similarly for $\beta_0$).
    
We then apply a $t$-test, using test statistic:
\begin{align}
  t_1 &= \frac{\hat{\beta}_1}{SE(\hat{\beta}_1)} \\
  df &= n - 2 \nonumber
\end{align}
(where $n$ = number of observations).

The resulting $p$-value tells us how surprised are we to get a slope this far from zero ($\beta_1$) under $H_0$.  (With this high a standard error, given this much data.)

To see the results of these hypothesis tests in R:

```r
summary(lm(RTlexdec~WrittenFrequency, young))
```

```
## 
## Call:
## lm(formula = RTlexdec ~ WrittenFrequency, data = young)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.34664 -0.05523 -0.00546  0.05167  0.34877 
## 
## Coefficients:
##                    Estimate Std. Error t value Pr(>|t|)    
## (Intercept)       6.6255559  0.0049432 1340.34   <2e-16 ***
## WrittenFrequency -0.0371069  0.0009242  -40.15   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.08142 on 2282 degrees of freedom
## Multiple R-squared:  0.414,	Adjusted R-squared:  0.4137 
## F-statistic:  1612 on 1 and 2282 DF,  p-value: < 2.2e-16
```
The $t$-values and associated $p$-values are in the `Coefficients:` table, where the first row is for $\hat{\beta}_0$ and the second row is for $\hat{\beta}_1$.

> **Questions**:
>
> * Why does the column for the $p$-value say `Pr(>|t|)`?

Having the SEs of the coefficients also lets us compute 95\% confidence intervals for the least-squared estimators.  Going from the null hypothesis ($H_0$) above: if the 95\% CI of the slope ($\beta_1$) does not include 0, we can reject $H_0$ with $\alpha = 0.05$.

In R, you can get confidence intervals for a fitted model as follows:

```r
confint(lm(RTlexdec~WrittenFrequency, young))
```

```
##                        2.5 %      97.5 %
## (Intercept)       6.61586227  6.63524948
## WrittenFrequency -0.03891921 -0.03529463
```

Neither CI includes zero, consistent with the very low $p$-values in the model table above.

#### Example: Small subset {-}

For the full dataset of `english` young speakers, it's a little silly to do hypothesis testing given how much data there is and the clarity of the pattern---the line of best fit has a tiny confidence interval.  Just for exposition, let's look at the line of best fit for a subset of just $n=100$ points:


```r
set.seed(2903) # This makes the following "random" sampling step always give the same resuult
young_sample <- young %>% sample_n(100)

ggplot(young_sample, aes(WrittenFrequency, RTlexdec)) + 
  geom_point() + 
  geom_smooth(method="lm")
```

<img src="03-linear-regression_files/figure-html/unnamed-chunk-12-1.png" width="480" style="display: block; margin: auto;" />

> **Questions**:
>
> * What does `geom_smooth(method="lm")` do?

In this plot, the shading around the line is the 95\% confidence interval.  "Can we reject $H_0$?" is equivalent to asking, "Can a line with 0 slope cross the shaded area through the range of $x$ (and going through (mean($X$), mean($Y$))?

### Quality of fit

Here is the model we have been discussing, plotted on top of the empirical data:

    

```r
young <- filter(english, AgeSubject == "young")

ggplot(young, aes(WrittenFrequency, RTlexdec)) + 
  geom_point(size=0.25) + 
  geom_smooth(method="lm") + 
  theme_bw()
```

<img src="03-linear-regression_files/figure-html/unnamed-chunk-13-1.png" width="480" style="display: block; margin: auto;" />

We often want a metric quantifying how well a model fits the data---the *goodness of fit*.

For simple linear regression, we can derive such a metric by first defining three quantities:

* *SST*: Total sum of squares

* <span style="color:blue">*SSR*</span>: Sum of squares due to regression

* <span style="color:orange">*SSE*</span>: Sum of squares due to error

<center>
![](images/slr_quality_of_fit.png)
</center>

(Source: slides from *Business Statistics: A First Course (Third edition)*)

<!-- TODO FUTURE: make own figure or get public domain figure -->

The fundamental equality is:
*SST* = <span style="color:blue">*SSR*</span> + <span style="color:orange">*SSE*</span>

Intuitively we want a measure of how much of SST is accounted for by SSR.  This is  $R^2$: the proportion of total variability in $Y$ accounted for by $X$:
\begin{equation}
  R^2 = \frac{SS_{\text{fit}}}{SS_{\text{total}}}
\end{equation}

$R^2$ always lies between 0 and 1, which can conceptually be thought of as:

* 0: none of the variance in $Y$ is accounted for by $X$
* 1: all of the variance `` ``

It turns out $R^2$ is also the square of (Pearson's) correlation, $r$.

#### Example {-}

Here is the SLR model fitted to a subset of 100 data points, repeated for convenience:


```r
set.seed(2903)

d <- english %>% filter(AgeSubject=="young") %>% sample_n(100)
summary(lm(RTlexdec~WrittenFrequency, d))
```

```
## 
## Call:
## lm(formula = RTlexdec ~ WrittenFrequency, data = d)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.127857 -0.045072 -0.005826  0.042917  0.235034 
## 
## Coefficients:
##                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)       6.61733    0.02190 302.236  < 2e-16 ***
## WrittenFrequency -0.03501    0.00419  -8.354 4.43e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.07121 on 98 degrees of freedom
## Multiple R-squared:  0.4159,	Adjusted R-squared:   0.41 
## F-statistic: 69.79 on 1 and 98 DF,  p-value: 4.43e-13
```

In the model table, note the values of the sample statistic under  `t value` and its significance in the `Pr(>|t|)` column in the `WrittenFrequency` row, as well as of the correlation statistic `Multiple R-squared`.

This is a hypothesis test for Pearson's $r$ for the same data, checking whether it is significantly different from 0:


```r
cor.test(d$WrittenFrequency, d$RTlexdec)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  d$WrittenFrequency and d$RTlexdec
## t = -8.354, df = 98, p-value = 4.43e-13
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  -0.7467533 -0.5135699
## sample estimates:
##       cor 
## -0.644931
```

Note that $t$, $p$, and $r$ (the square root of `Multiple R-squared`) are exactly the same.  Thus, fitting a simple linear regression and conducting a correlation test give us two ways of finding the same information.

### Categorical predictor

Simple linear regression easily extends to the case of a binary $X$ (a *factor*).

#### Example {-}

* `english` data

* Predictor: `AgeSubject`
    
* Response: `RTlexdec`
    
Everything is the same as for the case where $X$ is continuous, except now we have:

* $x_i = 0$: if `AgeSubject == "old"`
    
* $x_i = 1$: if `AgeSubject == "young"`
    
The regression equation is exactly the same as Equation \@ref(eq:linreg2):

$$
  y_i = \beta_0 + \beta_1 x_i + \epsilon_i
$$

Only the interpretation of the coefficients differs:

* $\beta_0$: **mean** `RTlexdec` when `AgeSubject == "old"` (since $x_i = 0 \iff$ `AgeSubject == "old"`)
    
* $\beta_1$: **difference in mean** `RTlexdec` between `young` and `old`
    
Hypothesis tests, $p$-values, CIs, and goodness of fit work exactly the same as for a continuous predictor.    

#### Example {-}

Suppose we want to test whether the difference in group means is statistically significantly different from 0:


```r
english %>% ggplot(aes(AgeSubject, RTlexdec)) +
  geom_boxplot() + 
  stat_summary(fun.y="mean", geom="point", color="blue", size=3) +
  theme_bw()
```

<img src="03-linear-regression_files/figure-html/unnamed-chunk-16-1.png" width="288" style="display: block; margin: auto;" />

> **Question**: 
>
> * What goes in the blanks?


```r
m1 <- lm(_____ ~ _____, english)
summary(m1)
```


<!-- ```{r} -->
<!-- m1 <- lm(RTlexdec ~ AgeSubject, english) -->
<!-- summary(m1) -->
<!-- ``` -->

Note the $t$ and $p$-values for `AgeSubjectyoung`, we'll need them in a second.

### SLR with a binary categorical predictor vs. two-sample $t$-test

Conceptually, we just did the same thing as a two-sample $t$-test---tested the difference between two groups in the value of a continuous variable. Let's see what the equivalent $t$-test gives us:


```r
t.test(RTlexdec ~ AgeSubject, english, var.equal=T)
```

```
## 
## 	Two Sample t-test
## 
## data:  RTlexdec by AgeSubject
## t = 67.468, df = 4566, p-value < 2.2e-16
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  0.2152787 0.2281642
## sample estimates:
##   mean in group old mean in group young 
##            6.660958            6.439237
```
(The `var.equal` option forces the $t$-test to assume equal variances in both groups, which is one assumption of linear regression.)

You should find that both tests give identical $t$ and $p$-values.  So, a $t$-test can be thought of as a special case of simple linear regression.

#### Bonus: Linear vs. smooth regression lines

We have forced an SLR fit in the plots above using the `method='lm'` flag, but by default `geom_smooth` uses a *nonparametric smoother* (such as LOESS, the `geom_smooth` default for small samples):


```r
young <- filter(english, AgeSubject=='young')

set.seed(2903)
young_sample <- young %>% sample_n(100)

day7_plt1 <- ggplot(young_sample, aes(WrittenFrequency, RTlexdec)) + 
  geom_point(size=0.5) +
  geom_smooth(method="lm") + 
  ggtitle("Linear regression line and 95% CI")

day7_plt2 <- ggplot(young_sample, aes(WrittenFrequency, RTlexdec)) + 
  geom_point(size=0.5) +
  geom_smooth() + 
  ggtitle("Smooth regression line and 95% CI")

grid.arrange(day7_plt1, day7_plt2, ncol = 2)
```

<img src="03-linear-regression_files/figure-html/unnamed-chunk-19-1.png" width="672" style="display: block; margin: auto;" />


Note the differences in the two plots:

* Linear/nonlinear

* CI widths related to distance from mean, versus **amount of data nearby**

(Hence "**L**ocal" in LOESS.)


## Multiple linear regression

In *multiple linear regression*, we use a linear model to predict a continuous response with $p$ predictors ($p>1$):

$$
  Y = \beta_0 + \beta_1 X_i + \beta_2 X_2 + \cdots + \beta_p X_p + \epsilon
$$

Each predictor $X_i$ can be continuous or categorical.

#### Example: RT ~ frequency + age {- #ex1}

For [the `english` dataset](#engdata), let's model reaction time as a function of word frequency and participant age.  Recall that in addition to the word frequency effect, older speakers react more slowly than younger speakers:



```r
english %>% ggplot(aes(AgeSubject, RTlexdec)) + 
  geom_boxplot() + 
  stat_summary(fun.y="mean", geom="point", color="blue", size=3) +
  theme_bw()
```

<img src="03-linear-regression_files/figure-html/unnamed-chunk-20-1.png" width="384" style="display: block; margin: auto;" />

The response and predictors are:

* $Y$: `RTlexdec`
    
* $X_1$: `WrittenFrequency` (continuous)
    
* $X_2$: `AgeSubject` (categorical) (0: `old`, 1: `young`)

Because $p=2$, the regression equation for observation $i$ is    
$$
  y_i = \beta_0 + \beta_1 x_{i1} + \beta_2 x_{i2} + \epsilon_i
$$
where $x_{ij}$ means the value of the $j^{\text{th}}$ predictor for the $i^{\text{th}}$ observation.

To fit this model in R:


```r
m2 <- lm(RTlexdec~WrittenFrequency+AgeSubject, english)
```

Summary of the model:

```r
summary(m2)
```

```
## 
## Call:
## lm(formula = RTlexdec ~ WrittenFrequency + AgeSubject, data = english)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.34622 -0.06029 -0.00722  0.05178  0.44999 
## 
## Coefficients:
##                    Estimate Std. Error t value Pr(>|t|)    
## (Intercept)       6.8467921  0.0039792 1720.64   <2e-16 ***
## WrittenFrequency -0.0370103  0.0007033  -52.62   <2e-16 ***
## AgeSubjectyoung  -0.2217215  0.0025930  -85.51   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.08763 on 4565 degrees of freedom
## Multiple R-squared:  0.6883,	Adjusted R-squared:  0.6882 
## F-statistic:  5040 on 2 and 4565 DF,  p-value: < 2.2e-16
```

This model tells us that the least-squares solution for the regression line is:
$$
  \texttt{RTlexdec} = \underbrace{6.846}_{\beta_0} + \underbrace{(- 0.037)}_{\beta_1} \cdot \texttt{WrittenFrequency} + \underbrace{(- 0.221)}_{\beta_2} \cdot \texttt{AgeSubject} + \text{error}
$$

> **Questions**: 
>
> * What RT does the model predict for an observation with `WrittenFrequency`=5 and `AgeSubject`='old'?

<div class="fold s o">

```r
6.846 + (-0.037 * 5) + (-0.221 * 0)
```

```
## [1] 6.661
```
</div>

Note that in this MLR model, the interpretation of each coefficient is:

* $\beta_0$: predicted value when all predictors = 0
    
* $\beta_1$, $\beta_2$: change in a predictor **when others are held constant**

For example, the difference between old and young speakers in RT is 0.221, when word frequency is held constant.

### Goodness of fit metrics

With $R^2$ defined as for simple linear regression, in terms of sums of squares, the exact same measure works to quantify goodness of fit of a multiple linear regression:
$$
  R^2 = \frac{SS_{\text{fit}}}{SS_{\text{total}}}
$$
sometimes called *multiple $R^2$*.

An alternative to $R^2$ when there's more than one predictor (MLR) is *adjusted $R^2$*, defined as:
$$
  R^2 = 1 - \frac{SS_{\text{fit}}/(n - p - 1)}{ SS_{\text{total}}/(n - 1) }
$$
where $p$ is the number of predictors and $n$ is the number of observations.

In this expression, the sum-of-squares term can be thought of as a ratio comparing the amount of variance explained by two models: the "full" model (the one with $p$ predictors) and the "baseline" model (the one with just the intercept).  Each model's sum of squares is scaled by its degrees of freedom; intuitively, this gives a measure of how much variance is explained **given the number of predictors**. (We expect that if you throw more predictors in a model, more variance can be explained, just by chance.)

The adjusted $R^2$ measure only increases if the $p$ additional predictors improve the model more than would be expected by chance.  

* **Pro**: Adjusted $R^2$ is more appropriate as a metric for comparing different possible model---unlike "multiple $R^2$", adjusted $R^2$ doesn't automatically increase whenever new predictors are added.  

* **Con**: Multiple $R^2$ is no longer interpretable as "fraction of the variation accounted for by the model".

R reports both adjusted and non-adjusted versions, as seen in the model summary above.

### Interactions and factors

The models we have considered so far assume that each predictor affects the response **independently**.  For example, in [the example above](#c2ex1) (`RT ~ frequency + age`), our model assumes that the slope of the frequency effect on RT is the same for old speakers as for young speakers.  This looks like it might be approximately true:

```r
ggplot(english, aes(WrittenFrequency, RTlexdec)) + 
  geom_point(size=0.5) + 
  geom_smooth(method="lm", se=F) +
  facet_wrap(~AgeSubject)
```

<img src="03-linear-regression_files/figure-html/unnamed-chunk-24-1.png" width="480" style="display: block; margin: auto;" />
in that there seems to be a similarly negative slope for both groups.  That is, a model of this form seems approximately correct:
$$
  Y = \beta_0 + \beta_1 X_1 + \beta_2 X_2 + \epsilon
$$
($\epsilon$ = error).

Here is a (fake) example where the independence assumption is definitely not true:

* $Y$: Job performance (continuous)
    
* $X1$: Training (categorical)
    
* $X2$: Autonomy (categorical)
    
<img src="03-linear-regression_files/figure-html/unnamed-chunk-25-1.png" width="576" style="display: block; margin: auto;" />

The effect of training on job performance is larger for high-autonomy participants. In this case, we say there is an *interaction*  between training and autonomy: the value of one predictor modulates the effect of the other.  This interaction is modeled by adding an extra term to the regression equation:
$$
  Y = \beta_0 + \beta_1 X_1 + \beta_2 X_2 + \beta_3 X_1 X_2 + \epsilon
$$
which is the *product* of the two terms.  Note that
$$
\beta_2 X_2 + \beta_3 X_1 X_2 = (\beta_2 + \beta_3 X_1) X_2
$$
so the interaction coefficient $\beta_3$ modulates the slope of $X_2$: depending on the value of $X_1$, $X_2$ has a different effect on $Y$.

#### Example {-}

Returning to [the example above](#c2ex1), suppose we'd like to know how much the slope of `WrittenFrequency` does actually differ between old and young speakers, and whether the difference is statistically significant ($\alpha = 0.05$). 


```r
english %>% ggplot(aes(WrittenFrequency, RTnaming)) + 
  geom_point(size=0.5) + 
  geom_smooth(aes(group=AgeSubject, color=AgeSubject), method="lm", se=F)
```

<img src="03-linear-regression_files/figure-html/unnamed-chunk-26-1.png" width="480" style="display: block; margin: auto;" />

  `X1:X2` means "interaction between `X1` and `X2`, and the notation used in R for interactions is `X1*X2`, which expands automatically to `X1 + X2 + X1:X2`.  (The non-interaction terms are sometimes called *main effects*.)  Note that in R these are equivalent:

```
lm(RTnaming ~ WrittenFrequency * AgeSubject, english)
lm(RTnaming ~ WrittenFrequency + AgeSubject + WrittenFrequency:AgeSubject, english)
```

To fit a model including an interaction between frequency and age:


```r
m3 <- lm(RTnaming ~ WrittenFrequency * AgeSubject, english)
```

In the summary of this model, of interest is the `WrittenFrequency:AgeSubjectyoung` row, which is the interaction effect:

```r
summary(m3)
```

```
## 
## Call:
## lm(formula = RTnaming ~ WrittenFrequency * AgeSubject, data = english)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.160510 -0.033425 -0.002963  0.030855  0.181032 
## 
## Coefficients:
##                                    Estimate Std. Error t value Pr(>|t|)
## (Intercept)                       6.5517608  0.0029118 2250.09  < 2e-16
## WrittenFrequency                 -0.0116031  0.0005444  -21.31  < 2e-16
## AgeSubjectyoung                  -0.3651823  0.0041179  -88.68  < 2e-16
## WrittenFrequency:AgeSubjectyoung  0.0046191  0.0007699    6.00 2.13e-09
##                                     
## (Intercept)                      ***
## WrittenFrequency                 ***
## AgeSubjectyoung                  ***
## WrittenFrequency:AgeSubjectyoung ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.04796 on 4564 degrees of freedom
## Multiple R-squared:  0.9278,	Adjusted R-squared:  0.9278 
## F-statistic: 1.956e+04 on 3 and 4564 DF,  p-value: < 2.2e-16
```

We see that there is indeed a significant interaction between `WrittenFrequency` and `AgeSubject`.

> **Questions**:
>
> * What does it mean that this coefficient is positive? 
>
> * For this regression model including an interaction, what is the model for an observation with `WrittenFrequency=3` and `AgeSubject=='old'`? [Solution](#c2sol1).
>
> * What is the model for an observation with `WrittenFrequency=3` and `AgeSubject=='young'`? [Solution](#c2sol2).

### Plotting interactions

In general, making plots is indispensable for interpreting interactions.  It is possible, with practice, to interpret interactions from the regression table, but examining a good plot is usually also necessary and much faster.

In later chapters we will cover how to actually visualize model predictions---exactly what the model predicts for different combinations of predictor values. You can usually get a reasonable approximation of this by making the relevant empirical plot, such as:


```r
english %>% ggplot(aes(WrittenFrequency, RTnaming)) +
  geom_smooth(method='lm', aes(color=AgeSubject)) +
  xlab('Log written frequency') +
  ylab('Naming RT in log(s)')
```

<img src="03-linear-regression_files/figure-html/unnamed-chunk-29-1.png" width="480" style="display: block; margin: auto;" />

It is often better to use empirical plots to visualize interactions---even though, strictly speaking, you are not plotting the model's predictions.

* **Pros**: Empirical plots are more intuitive, and if you have a robust effect it should probably show up in an empirical plot.

* **Cons**: Empirical plots don't show actual model predictions, and in particular don't control for the effects of other predictors.

### Categorical factors with more than two levels

We are often interested in categorical predictors with more than two levels.  For example, for the [Dutch `regularity` data](#dregdata), we might wonder whether the size of a verb's morphological family size is affected by what auxiliary it takes in the past tense.

```r
regularity %>% ggplot(aes(Auxiliary, FamilySize)) + 
  geom_boxplot() + 
  stat_summary(fun.y="mean", geom="point", color="blue", size=3)
```

<img src="03-linear-regression_files/figure-html/unnamed-chunk-30-1.png" width="480" style="display: block; margin: auto;" />
The relevant variable, `Auxiliary`, has three levels.  Let's see how this kind of variable is dealt with in a regression model.

#### Exercise {-}

1. Fit a regression model predicting `FamilySize` from `Auxiliary`.

2. What does the intercept ($\beta_0$) represent?

3. What do the two coefficients for `Auxiliary` ($\beta_1$, $\beta_2$) represent?

Hint: Compare $\beta$ coefficients with group means, which you can check using `summarise()` from `dplyr`.

Solution to (1):
<div class="fold s o">

```r
m4 <- lm(FamilySize ~ Auxiliary, regularity)
summary(m4)
```

```
## 
## Call:
## lm(formula = FamilySize ~ Auxiliary, data = regularity)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.9133 -0.7982 -0.0250  0.7442  3.5983 
## 
## Coefficients:
##                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)       2.59000    0.04902  52.839   <2e-16 ***
## Auxiliaryzijn     0.39274    0.26780   1.467   0.1430    
## Auxiliaryzijnheb  0.32329    0.12594   2.567   0.0105 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.177 on 697 degrees of freedom
## Multiple R-squared:  0.01169,	Adjusted R-squared:  0.008855 
## F-statistic: 4.123 on 2 and 697 DF,  p-value: 0.0166
```
</div>

Solution to (2): The value of `FamilySize` when `Auxiliary`="hebben".


Solution to (3): The predicted difference in `FamilySize` between `Auxiliary`="zijn" and "hebben", and between `Auxiliary`="zijnheb" and "hebben".


### Releveling factors

It's often useful for conceptual understanding to change the ordering of a factor's levels. For the `regularity` example, we could make `zijn` the base level of the `Auxiliary` factor:

```r
regularity$Auxiliary <-relevel(regularity$Auxiliary, "zijn")

m5 <- lm(FamilySize ~ Auxiliary, regularity)
summary(m5)
```

```
## 
## Call:
## lm(formula = FamilySize ~ Auxiliary, data = regularity)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.9133 -0.7982 -0.0250  0.7442  3.5983 
## 
## Coefficients:
##                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)       2.98274    0.26328  11.329   <2e-16 ***
## Auxiliaryhebben  -0.39274    0.26780  -1.467    0.143    
## Auxiliaryzijnheb -0.06946    0.28771  -0.241    0.809    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.177 on 697 degrees of freedom
## Multiple R-squared:  0.01169,	Adjusted R-squared:  0.008855 
## F-statistic: 4.123 on 2 and 697 DF,  p-value: 0.0166
```

> **Questions**:
>
> * What is the interpretation of the intercept and the two `Auxiliary` coefficients in this new model?

## Linear regression assumptions {#linear-regression-assumptions}

Up to now, we have discussed regression models without worrying about the assumptions that are made by linear regression, about your data and the model.  We will cover six main assumptions, the first four have to do with the form of the model and errors:

1. Linearity

2. Independence of errors

3. Normality of errors

4. Constancy of errors (*homoscediasticity*)

followed by two assumptions about the predictors and observations:

5. Linear independence of predictors

6. Observations have roughly equal influence on the model

We'll discuss each in turn.


Our presentation of regression assumptions and diagnostics is indebted to Chapters 4, 6 and 9 of @chatterjee2012regression, where you can find more detail.

### Visual methods

Visualization is crucial for checking model assumptions, and for data analysis in general.  A famous example illustrating this is *Anscombe's quartet*: a set of four small datasets of $(x,y)$ pairs with:

* The same mean and variance for $x$ 
    
* The same mean and variance for $y$
    
* A correlation($x$, $y$) = 0.816
    
* The same regression line ($y = 3 + 0.5\cdot x$)
    
in each case---and yet the datasets show qualitatively different patterns, as can be seen by plotting $y$ against $x$:
<center>    
![](images/anscombe.png)    
</center>

(Source: unknown, but definitely taken from somewhere)

<!-- TODO FUTURE: find or fix -->

With more than one predictor it becomes  difficult to check regression assumptions by just plotting the data, and visual methods such as *residual plots* (presented below) are crucial.

### Assumption 1: Linearity

The first assumption of a linear regression model is that the relationship between the response ($Y$) and predictors ($X_i$) is... linear.

While obvious, this assumption is very important: if it is violated, the model's predictions can be in serious error.

The linearity assumption can be partially checked by making a scatterplot of $Y$ as a function of each predictor $X_i$.  It is hard to exhaustively check linearity for MLR, because nonlinearity might only become apparent when $Y$ is plotted as a function of several predictors.

#### Example {-}

Consider relative pitch, intensity, and duration in [the `alternatives` dataset](#altdata).

We can make *pairwise plots* of these variables using the `pairscor.fnc()` function `languageR`, to see if any of these variables might be a function of the other two.
    

```r
pairscor.fnc(with(alt, cbind(rpitch, rintensity, rduration)))
```

<img src="03-linear-regression_files/figure-html/unnamed-chunk-33-1.png" width="480" style="display: block; margin: auto;" />

> **Questions**: 
>
> * Is this the case?

Let's examine the relationship between realtive duration and relative intensity more closely:

```r
alt %>% ggplot(aes(rduration, rintensity)) + 
  geom_point()
```

<img src="03-linear-regression_files/figure-html/unnamed-chunk-34-1.png" width="480" style="display: block; margin: auto;" />

We can try to fit a line to this data, but if we compare to using a nonlinear smoother, it seems clear that the relationship is not linear:

```r
alt %>% ggplot(aes(rduration, rintensity)) + 
  geom_point() + geom_smooth(col='red', se=F) +
  geom_smooth(method="lm", col="blue", se=F)
```

<img src="03-linear-regression_files/figure-html/unnamed-chunk-35-1.png" width="480" style="display: block; margin: auto;" />

In particular, it looks like there is a quadratic trend.  This means that we can in fact fit a linear regression, we just need to include coefficients for both `rduration` and its square, like so:


```r
mq <- lm(rintensity ~ rduration + I(rduration^2), alt)
summary(mq)
```

```
## 
## Call:
## lm(formula = rintensity ~ rduration + I(rduration^2), data = alt)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -16.3199  -3.5197  -0.2448   2.9515  19.1480 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)      5.8280     0.2622  22.228   <2e-16 ***
## rduration        3.8841     0.3263  11.904   <2e-16 ***
## I(rduration^2)  -3.1381     0.3429  -9.151   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.05 on 627 degrees of freedom
## Multiple R-squared:  0.358,	Adjusted R-squared:  0.3559 
## F-statistic: 174.8 on 2 and 627 DF,  p-value: < 2.2e-16
```

We will cover more nonlinear functions of predictors in [a later chapter](#nonlinear-effects). The important point here is that a model with just `rduration` as a predictor would have violated the linearity assumption, but a model with both `rduration` and `rduration^2` as predictors doesn't (arguably).

### Assumption 2: Independence of errors {#c2ioe}

All regression equations we have considered contain an *error* or *residual* term: $\epsilon_i$ for the $i^{\text{th}}$ observation.  A crucial assumption is that these $\epsilon_i$ are **independent**: knowing the error for one observation shouldn't tell you anything about the error for another observation.

Unfortunately, violations of this assumption are endemic in realistic data.  The simplest example is in time series, or longitudinal data---such as pitch measurements taken every 10 msec in a speech signal.

> **Questions**:
>
> * Can you think of why this might be the case?

In linguistics and psycholinguistics, violations of the independence assuption are common because most datasets include multiple observations per participant or per item (or per word, etc.).  Crucially, violations of the independence assumption are often *anti-conservative*: CIs will be too narrow and $p$-values too small if the lack of independence of errors is not taken into account by the model.

Some solutions to these issues:

* [**Paired-t-tests**](#paired-t-test), where applicable (binary predictor; two measures per participant)
    
* **Mixed-effects regression** (more general solution, major focus later this term)
    
Until we cover mixed-effects regression, we will be getting around the fact that the independence-of-errors assumption usually doesn't hold for linguistic data, in one of two ways:

1. Selectively using datasets where this assumption **does** hold, such as `regularity`.

2. Analyzing datasets where this assumption does **not** hold, such as `tapping` or `english`, using analysis methods that do assume indepedence of errors (such as linear regression), with the understanding that our regression models are probably giving results that are "wrong" in some sense.

### Assumption 3: Normality of errors

The next major assumption is that the errors $\epsilon_i$ are normally distributed, with mean 0 and a fixed variance.  This assumption is impossible to check directly, because we never observe the true *errors* $\epsilon_i$, only the *residuals* $e_i$.  The residuals are no longer normally distributed (even if the errors are), because some observations will be more influential than others in determining the fitted responses $\hat{y}_i$ when fitting the least-squares estimates of the regression coefficients. 

#### Example {-}


```r
young <- filter(english, AgeSubject=='young')

set.seed(2903)
young_sample <- young %>% sample_n(100)

ggplot(young_sample, aes(WrittenFrequency, RTlexdec)) + 
  geom_point(size=0.5) +
  geom_smooth(method="lm") + 
  ggtitle("Linear regression line and 95% CI")
```

<img src="03-linear-regression_files/figure-html/unnamed-chunk-37-1.png" width="672" style="display: block; margin: auto;" />

The width of the confidence interval increases for points further from (average of `WrittenFrequency`, average of `RTlexdec`), because these points are more influential, causing the variance of the residuals to increase---thus, the variance is not constant.

In order to correct for non-normality, the residuals are transformed in a way which accounts for the different influence of different observations (see e.g. @chatterjee2012regression 4.3), to *studentized* or *standardized residuals*.^[The studentized and standardized residuals, or "externally studentized" and "internally studentized" residuals (in @chatterjee2012regression), differ slightly in how they estimate the error variance:  a leave-one-out estimate versus an estimate using all observations. This difference shouldn't matter much except when certain observations are highly influential or in small datasets.]  (In R, by applying `rstudent` or `rstandard` to a fitted model.)  

In general, we **check assumptions about errors by examining the distribution of  standardized residuals**.  This is because **if** the normality of errors assumption holds, **then** the standardized residuals will be normally distributed with mean 0 and fixed variance.  So if they are not, we know the normality of errors assumption does not hold.  (If they are, it's not a guarantee that the normality of errors assumption holds, but we hope for the best.)

#### Example {- #c2ex2}


This exercises uses the `halfrhyme` data, briefly described [here](#halfdata).  Let's  abstract away from what the variables actually mean, and just think of them as $Y$ and $X$:

```r
ggplot(aes(x=cohortSize, y=rhymeRating),  data=filter(halfrhyme, conditionLabel=='bad')) +
  geom_point() + geom_smooth(method='lm') + 
  geom_jitter() + xlab("X") + ylab("Y")
```

<img src="03-linear-regression_files/figure-html/unnamed-chunk-38-1.png" width="672" />

The distribution of the standarized residuals for the regression of $Y$ as a function of $X$ is:


```r
halfrhyme.sub <- filter(halfrhyme, conditionLabel=='bad' & !is.na(cohortSize))
mod <- lm(rhymeRating ~ cohortSize, data=halfrhyme.sub)
halfrhyme.sub$resid <- rstandard(mod)
ggplot(aes(x=resid), data=halfrhyme.sub) + geom_histogram() + xlab("Residual (standardized)")
```

<img src="03-linear-regression_files/figure-html/unnamed-chunk-39-1.png" width="672" />
 
> **Questions**:
> 
> * Why do the residuals have this distribution?

This example illustrates probably the most common source of non-normal residuals: a highly non-normal distirbution of the predictor or response.

#### Effect and solution

Non-normality of residuals is a pretty common violation of regression assumptions.  How much does it actually matter?   @gelman2007data (p. 46) argue "not much", at least in terms of the least-squares estimates of the regression line (i.e., the regression coefficient values), which is often what you are interested in.  

However, non-normality of residuals, especially when severe, can signal other issues with the data, such as the presence of outliers, or the predictor or response being on the same scale. (Example: using non-log-transformed word frequency as a predictor.)  Non-normality of residuals can often be dealt with by **transforming the predictor or response** to have a more normal distribution (see Sec. \@ref(transforming-to-normality)). 

Non-normality of residuals can also signal other errors, such as an important predictor missing.

#### Exercise {-}

1. Using the `english` data, plot `RTlexdec` as a function of `WrittenFrequency`, and add a linear regression line.

<div class="fold s o">

```r
ggplot(english, aes(x = WrittenFrequency, y = RTlexdec)) + 
  geom_point(size = 0.5) + 
  geom_smooth(method = "lm", se=F)
```

<img src="03-linear-regression_files/figure-html/unnamed-chunk-40-1.png" width="480" style="display: block; margin: auto;" />
</div>

2. Do you think the residuals of this model are normally distributed? Why/why not?

3. Now plot a histogram of the standardized residuals of the mode. Does the plot confirm your first impressions?


```r
m8 <- lm(RTlexdec ~ _______, english)
m8.resid.std <- rstandard(______)
hist(______, breaks = 50)
```

<div class="fold s o">

```r
day9_plt1 <- ggplot(english, aes(WrittenFrequency, RTlexdec)) + 
  geom_point(size=0.5) + 
  geom_smooth(method="lm", se=F)

m8 <- lm(RTlexdec~WrittenFrequency, english)
m8.resid.std <- rstandard(m8)

day9_plt2 <- ggplot(data.frame(m8.resid.std), aes(x = m8.resid.std)) + 
  geom_histogram(bins = 50)

grid.arrange(day9_plt2, day9_plt1, ncol = 2)
```

<img src="03-linear-regression_files/figure-html/unnamed-chunk-42-1.png" width="768" style="display: block; margin: auto;" />
</div>

4. Now add `AgeSubject` to the model, and plot a histogram of its standardized residuals. What has changed? Why so?

<div class="fold s o">
<img src="03-linear-regression_files/figure-html/unnamed-chunk-43-1.png" width="768" style="display: block; margin: auto;" />
</div>

This exercise shows one reason that examining the residual distribution is useful.  If we didn't already know what the missing predictor was, the non-normality of the residual distribution gives us a way to look for an explanatory variable.  (Look at observations in each mode of the distribution, see what they have in common.)

### Assumtion 4: Constancy of variance

*Homoscedasticity* is one of the trickier regression assumptions to think about: the assumption that $\epsilon_i$ is normally distributed with the same variance, across all values of the predictors.  

For example, in our example modeling reaction time as a function of subject age and word frequency, it is assumed that the amount of variability in reaction time is similar for old speakers and young speakers, for high frequency words and young speakers, for observations of high frequency words for old speakers, and so on. 

In [the example above](#c2ex2) from the `halfrhyme` data, the homoscedasticity assumption is violated: lower values of $\hat{y}$ show higher variance in the residuals.

In this case, the model shows *heteroscedasticity*.
    
**If** homoscedasticity holds, **then** the standardized residuals are uncorrelated with the predictor values, and with the fitted values, and there should be a constant spread (variance) of residual values (y-axis) for each fitted or predictor value (x-axis). Thus, it is common to plot (standardized) residuals  versus fitted values and versus predictors. (The fitted values-residuals plot is one of the diagnostic plots that shows up if you `plot(mod)` in R, where `mod` is a fitted model.)  The desired pattern is a flat line, with the same variance for different x-axis values.

For the `halfrhyme` data, the fitted value-residuals plot looks like:


```r
halfrhyme.sub <- filter(halfrhyme, conditionLabel=='bad' & !is.na(cohortSize))
mod <- lm(rhymeRating ~ cohortSize, data=halfrhyme.sub)
halfrhyme.sub$resid <- rstandard(mod)
halfrhyme.sub$fitted <- fitted(mod)
ggplot(aes(x=fitted, y=resid), data=halfrhyme.sub) + geom_point() + geom_smooth() + xlab("Fitted values") + ylab("Standardized residuals")
```

<img src="03-linear-regression_files/figure-html/unnamed-chunk-44-1.png" width="672" />

There is greater variance for higher fitted values, indicating heteroscedacticity.

#### Effect and solution

In general, estimates of least-squares coefficients in the presence of heteroscedasticity are unbiased, but standard errors will be under- or over-estimated.  This means that confidence intervals will be too narrow/wide and $p$-values too low/high.

Heteroscedasticity is endemic in some types of data, such as from lexical statistics (@baayen2008analyzing, p. 35).  In other types of data, such as economic data, heteroscedasticity is so common that dealing with it is a primary concern in statistical analysis.  Heteroscedasticity is discussed less frequently than other regression assumptions for linguistic data, but it is unclear whether this is because heteroscedasticity is less common than in other types of data or just has not been focused on by language scientists.

<!-- I don't know (MS) whether heteroscedasticity is actually less common in linguistic data than in other kinds of data, or whether it's just not something that language scientists have focused on.  Certainly it is discussed less frequently than other regression assumptions. -->

<!-- Morgan: Do you want to leave this part (^^^) in here ??? -->
    
One can often correct for heteroscedasticity by using various transformations of the response and predictors to get better estimates (@chatterjee2012regression, Ch. 4).  For example, in the `halfrhyme` example, it turns out that a stronger effect of $X$ on $Y$ (lower $p$-value) can be detected once variance is stabilized.

### Interim summary

* **Linearity**

    * Serious violation if not met.
    
    * Fit data with non-linear trend (e.g. quadratic)
    
    * Transformed predictor/response to normality (e.g. log-transform)
    
* **Independence of error**:

    * In linguistic data: use mixed-effects regression^[Or another method that accounts for non-independence of errors, such as repeated measures ANOVA.]
    
* **Normality of errors**:

    * Not too serious violation if not met, but may signal issues with model/data
    
    * Remove outliers; transform $X$/$Y$ to normality
    
* **Constancy of variance**:

    * Not commonly checked in linguistic data
    
    * Leads to uncertain regression estimates
    
    * Transform predictor/response to normality


### Transforming to normality

Normality of the distribution of the response and predictors ($Y$ and $X_i$) is **not** an assumption of linear regression.  This is a common misconception, perhaps because normality is an assumption of other basic statistical inference tools, such as $t$-tests.  

However, there is still good reason to be circumspect if $Y$ or $X_i$ are not normally distributed, because this can often lead to violations of regression assumptions. This is why it is recommended to transform the predictors and response to normality to fix violations of the linearity, normality of errors, and homoscedasticity assumptions.   Because non-normality of $Y$ or $X_i$ can easily lead to violations of regression assumptions, it is sometimes recommended to transform them to normality just to be safe. This makes it less likely that a regression assumption will be violated, but also changes the interpretation of the transformed variable, which may make it harder to interpret the model's results.

For linguistic data, *logarithmic* transformations are often useful when working with skewed distributions, because many kinds of linguistic data are roughly *log-normally* distributed, meaning the log-transformed variable is normally distributed. Some examples:

* Lexical statistics (e.g. lexical frequency, probability)
    
* Reaction times (e.g. naming latencies)
    
* Duration measures in phonetics (syllable, phrase durations)

Other transformations besides log are also used: reaction times are sometimes inverse or inverse-log-transformed (1/RT, log(1/RT)), and durations are sometimes square-root-transformed.

#### Example: Distribution of raw vs. log lexical frequency {-}


```r
english$WrittenFrequency_raw <- exp(english$WrittenFrequency)
english$WrittenFrequency_log <- english$WrittenFrequency

day10_plt1 <- ggplot(english, aes(x = WrittenFrequency_raw)) + 
  geom_histogram() + 
  xlab("Raw written Frequency")
day10_plt2 <- ggplot(english, aes(x = WrittenFrequency_log)) + 
  geom_histogram() + 
  xlab("Log written Frequency")

grid.arrange(day10_plt1, day10_plt2, ncol = 2)
```

<img src="03-linear-regression_files/figure-html/unnamed-chunk-45-1.png" width="768" style="display: block; margin: auto;" />

R note: ggplot functions such as `scale_x_log10()` can be used to plot data in its raw units on a log scale, which is often more interpretable. Ex:

```r
 ggplot(english, aes(x = WrittenFrequency_raw)) + geom_histogram() + scale_x_log10() +
  xlab("Written Frequency (per million words)") + annotation_logticks(sides='b')
```

<img src="03-linear-regression_files/figure-html/unnamed-chunk-46-1.png" width="672" />

#### Exercise {- #c2ex3}

Consider the raw and log frequency measures for the `english` dataset, for young speakers:

```r
english$WrittenFrequency_raw <- exp(english$WrittenFrequency)
english$WrittenFrequency_log <- english$WrittenFrequency

young <- filter(english, AgeSubject=="young")
```


1. Plot `RTnaming` as a function of raw written frequency, and of log written frequency, with a green  LOESS (smooth) regression line added to each plot (using  `geom_smooth(color='green')`).

<div class="fold s o">

```r
plt5 <- ggplot(young, aes(WrittenFrequency_raw, RTnaming)) + 
  geom_point(size=0.5) + 
  xlab('Raw written Frequency') + 
  geom_smooth(se=F, color="green")

plt6 <- ggplot(young, aes(WrittenFrequency_log, RTnaming)) + 
  geom_point(size=0.5) + 
  xlab('Log written Frequency') + 
  geom_smooth(se=F, color="green")
grid.arrange(plt5, plt6, ncol = 2)
```

<img src="03-linear-regression_files/figure-html/unnamed-chunk-48-1.png" width="768" style="display: block; margin: auto;" />
</div>

2. Is the linearity assumption met in each case?

<!-- The point here is that the linearity assumption isn't met when $Y$ is untransformed word frequency, or at least it's unclear, whereas the assumption looks safe for log-transformed $Y$. -->

### Assumption 5: Linear independence of predictors

A crucial assumption of linear regression is that the predictors are *linearly independent*, meaning it isn't possible to write one predictor as a linear function of the others.  If you can do so, it's impossible to disentangle the effect of different predictors on the response. For example, temperatures in farenheit and centigrade are related by a linear function (F = $9/5$C + 32), so F and C are linearly dependent.

Another way of thinking of linear dependence is that in a linear regression where you predict one predictor as a function of the others, the $R^2$ would be 1.
Linear dependence of predictors will either give a model error or a weird model output  if you fit a linear regression in R, because the math to find least-squares estimates of coefficients doesn't work out if there is linear dependence: there is no longer a unique optimal solution. (For example, if the slope of F and C in a model were $\beta_1 = 2$ and $\beta_2 = 0$, then a different model using slopes of 0 and $10/9$ would also work.)^[In technical terms: the *design matrix* $X$ (where each column is the values of predictor $i$, across all observations) is not *full rank*, and the matrix $X^T X$, which is used to find the least-squares regression coefficient estimates, is *singular* (alternatively, there is a *singularity*)  Even if you don't care about the math of linear regression, it is useful to know that when you encounter these terms in R output it usually means some variable can be perfectly predicted from the others, and this is a problem.]  

#### Exercise {-}

Define a new variable in the `english` dataset that is the average of `WrittenFrequency` and `Familiarity`, then fit a linear regression of `RTlexdec` as a function of this new variable, `WrittenFrequency`, and `Familiarity`.  What looks odd in the model output?

### Collinearity 

Full linear dependence of predictors is usually a sign that something is conceptually wrong with your data, or your model structure.  In the farenheit/celsius example, it doesn't make conceptual sense to have both as predictors of anything.

However, it is very common for there to be **partial** dependence between predictors---that is, $0 < R^2 < 1$ when you regress one predictor on the others. This is called *multicollinearity*, or just *collinearity*.^[Note that you can have $R^2<1$ even when one predictor is completely predictable from other predictors---as long as this dependence is not a linear function.  Sometimes it does not make conceptual sense to have two predictors related in this way in a model (e.g. VOT and log(VOT)), while in other settings it does (e.g. linear and quadratic terms for the same variable).]  Collinearity is ubiquitous in lingusitic data, and can significantly affect the estimates and interpretations of regression coefficients---particularly when collinearity is "high" (say, $|R|>0.8$) However, (high) **collinearity is not a violation of the assumptions of linear regression**! 

This figure may be useful to get an intuitive sense of what collinearity is, if you think of X1-X4 as four predictors which affect Y, and may be highly interrelated (right figure) or independent (left figure).
    
![](images/collinearity.png) 

(Source: http://www.creative-wisdom.com/computer/sas/collinear_stepwise.html)

There can be collinearity among several predictors---hence the term "multicollinearity"---where one predictor is a linear function of several others. 

#### Exercise {-}

Suppose we are trying to predict the duration of the first vowel of every word in a dataset of conversational speech, using these four predictors:

1. log(speaking rate)

2. log(# of syllables in the word)

3. log(duration of word)

4. log(first syllable duration)

where "speaking rate" is defined as "syllables per second".

Why are these predictors linearly dependent?

#### Exercise {-}

<!-- > **Questions**:  -->
<!-- > -->
<!-- > * Is `RTlexdec` affected by a word's familiarity (`Familiarity`) for young speakers? -->

Let us simulate a small-scale experiment ($n=25$), using data from young speakers in the `english` dataset.  We measure `RT`, take `Familiarity` norms from a previous study, and would like to control for the following variables:

* `WrittenFrequency`
    
* `LengthInLetters`

Because both are expected to correlate with `Familiarity`, and to affect `RT`. 

We first examine whether there is collinearity among predictors, then assess whether familiarity affects lexical decision RT.


```r
young <- filter(english, AgeSubject == "young")
set.seed(2903)
d <- sample_n(young, 25)

pairscor.fnc(dplyr::select(d, RTlexdec, Familiarity, WrittenFrequency, LengthInLetters))
```

<img src="03-linear-regression_files/figure-html/unnamed-chunk-49-1.png" width="672" style="display: block; margin: auto;" />

> **Questions**: 
>
> * What correlations are present in the data? Is there collinearity among predictors?

Now, fit a model of just familiarity on RT:

```r
m.fam <- lm(RTlexdec ~ Familiarity, d)
summary(m.fam)
```

```
## 
## Call:
## lm(formula = RTlexdec ~ Familiarity, data = d)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.094956 -0.035753  0.002142  0.050448  0.093090 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.645471   0.038231  173.83  < 2e-16 ***
## Familiarity -0.047697   0.009502   -5.02 4.44e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.05699 on 23 degrees of freedom
## Multiple R-squared:  0.5228,	Adjusted R-squared:  0.502 
## F-statistic:  25.2 on 1 and 23 DF,  p-value: 4.442e-05
```

This model finds a highly significant effect of word familiarity on RT.

> **Questions**:
>
> * What is the interpretation of this model---what relationship is it capturing from the grid of plots above?

Now, 

```r
m.fam_freq <- lm(RTlexdec ~ Familiarity + WrittenFrequency + LengthInLetters, d)
summary(m.fam_freq)
```

```
## 
## Call:
## lm(formula = RTlexdec ~ Familiarity + WrittenFrequency + LengthInLetters, 
##     data = d)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.090416 -0.040823 -0.008442  0.042555  0.094739 
## 
## Coefficients:
##                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)       6.66953    0.07094  94.013   <2e-16 ***
## Familiarity      -0.03333    0.01821  -1.831   0.0814 .  
## WrittenFrequency -0.01017    0.01117  -0.911   0.3728    
## LengthInLetters  -0.00643    0.01410  -0.456   0.6530    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.05838 on 21 degrees of freedom
## Multiple R-squared:  0.5429,	Adjusted R-squared:  0.4776 
## F-statistic: 8.313 on 3 and 21 DF,  p-value: 0.0007782
```

This model finds that **none** of the three variables significantly affect RT

> **Questions**:
>
> * Why has the `Familiarity` effect changed between the two models? (Examine both the coefficient estimates and SEs.)
>
> * How is it possible that none of the three variables significantly affect RT, but together they do predict half the variation in RT ($R^2 = 0.54$)?

This kind of situation is called a *credit assignment problem*: we can tell that some combination of predictors **together** affects the response, but not whether an **individual** predictor does, after controlling for other predictors.  

#### Effects of collinearity

In general, collinearity increases Type II error: the probability of concluding a predictor has no effect on the response, when it actually does.  Collinearity does not in general affect the actual values of coefficient estimates, just their standard errors.^[However, when standard errors are high, the coefficient estimates themselves are by definition very uncertain, so in this sense collinearity "affects" coefficient values.]  

Importantly, **collinearity is a property of the data**---not a violation of model assumptions.  To the extent that an effect is "missed" (a Type II error) in the presence of collinearity, this is because of the structure of the data.  In the example above, it makes intuitive sense that it is harder to detect a "real" effect of `Familiarity` when `frequency` is added as a predictor, because of the correlation between `Familiarity` and `frequency`.

Because collinearity isn't a violation of model assumptions, high collinearity cannot be diagnosed using diagnostic plots, such as those considered for Assumptions 1-4 above.  

#### Diagnosing collinearity    

How can one then detect collinearity, and decide if it could be affecting (the standard errors of) regression coefficients?

Some warning signs that there may be substantial collinearity in your data (@chatterjee2012regression, 9.4):

* **Unstable coefficients**: large changes in values of the $\hat{\beta}_i$ when predictors/data points are added/dropped.

* **Nonsensical coefficients**: signs of $\hat{\beta}_i$ don’t conform to prior expectations.

* **Unexpected non-significance**: values of $\hat{\beta}_i$ for predictors expected to be important (e.g. from EDA) have large SEs, low $t$-values, and high $p$-values. 

These diagnostics all follow from the fact that when data is highly collinear, a linear relationship between predictors **almost** holds, so there are many regression coefficient estimates that give models **almost** as good as the least-squared estimates.

The degree of collinearity can be quantified using the *condition number* (`collin.fnc` in `languageR`), applied to the design matrix (where each column = values of one predictor, across the dataset).  For example, for `frequency` and `AoA` in the example above, the condition number is:

```r
library(languageR)
collin.fnc(dplyr::select(d, Familiarity, WrittenFrequency, LengthInLetters))$cnumber
```

```
## [1] 16.0162
```

As a rule of thumb, condition numbers can be interpreted as (@baayen2008analyzing, p. 200, citing @belsley1980regression):
    
* CN < 6: "no collinearity"
        
* CN < 15: "acceptable collinearity"
        
* CN > 30: "potential harmful collinearity"
        
#### Is collinearity a problem?

There are two philosophies here.  

The first is represented by @chatterjee2012regression and @baayen2008analyzing: (high) collinearity **is** a problem---it causes unstable coefficient estimates, increases Type II error, and can slow down or foil model fitting.  Therefore, it should be somehow dealt with, for example by changing the predictors included in the model ("residualizing" or dimensionality reduction strategies such as "principal components").

The second is represented by  @gelman2007data and @levy2012probabilistic: collinearity **is not** a problem---the issues above reflect a lack of information in your data, and how hard it is to detect effects of these variables based on this dataset.  Therefore, you should either fit the model with your original predictors (which acknowledges the lack of information), or collect more data.
    
While we think there is real merit to the second view, there
are certainly (very common) circumstances in which it is appropriate to take steps to decrease collinearity.  Most common: centering all predictor variables decreases collinearity, without any loss of information.  "Residualizing" one predictor on another is generally not a good idea, because it complicates the interpretation of the regression coefficients in unintended ways [@wurm2014residualizing]. 


### Assumption 6: Observations 

Two linear regression assumptions about **observations** are that they are:

1. Equally **reliable**

2. Roughly equally **influential**

The first assumption is hard to check in practice, and we won't consider it further.  The second assumption is important: observations should have "a roughly equal role in determining regression results and in influencing conclusions" (@chatterjee2012regression, p. 88).  This is because our goal in statistical analysis is usually to make inferences about **population** values of parameters (like the slope of a line of best fit), abstracting away from our finite sample. If certain observations are much more influential than others, they skew the regression results to reflect not the population, but the particular sample we happened to draw.

For example, consider the relationship between raw and log-transformed word frequency, and naming latency, in [the `english` dataset](#engdata). (The same data as  in [this example above](#c2ex3), but now fitting a simple line of best fit in each case.)

<img src="03-linear-regression_files/figure-html/unnamed-chunk-53-1.png" width="768" style="display: block; margin: auto;" />

When raw frequency is used as the predictor (left plot), the handful of "extreme" observations with frequency above 25000 have a much greater effect on the slope of the line than other observations, resulting in larger confidence intervals. 

Often, a non-normally distributed response or predictors leads to some points influencing the model much more than others, and the problem can be fixed by transforming to normality---as for the word frequency example (right plot).  After log-transforming frequency, removing the seven most extreme X values hardly affects the fitted line.
 
The presence of highly-influential observations can lead to either Type I or Type II errors: the influential observations might  be responsible for a spurious result, or they might obscure a pattern that would be clear if the influential observations were excluded.

This isn't always the case, though---often, some points are inherently more influential than others (say, a couple participants with behavior very different from others), and we need to decide how to proceed in fitting and interpreting the model.

### Measuring influence {#lin-reg-measuring-influence}

*Cook's distance* is a metric of how influential an observation is in a given model, defined as the product of two terms:

1. The (squared) standardized residual of the observation

2. A measure of the *leverage* of this observation---how much it affects the fitted values ($\hat{y}_i$)

Cook's distance (CD) is roughly **how much the regression coefficients change** when the model is fitted with all data except this observation. Data points with significantly higher CD than other points are highly influential, and should be flagged and examined as potential outliers.
    
#### Example {-}

Let's take a sample of the young-speaker subset of [`english`](#c2engdata) and tweak it, to get an intuition of what it means for observations to have high influence.


```r
set.seed(2903)
young_subset <- filter(english, AgeSubject=="young") %>% 
  sample_n(100)

# Tweak data to illustrate point 
young_subset[young_subset$Familiarity>6,]$RTnaming <- 6.35

ggplot(young_subset, aes(Familiarity, RTnaming)) +
  geom_point(size=3) + 
  geom_smooth(method="lm", se=F)
```

<img src="03-linear-regression_files/figure-html/unnamed-chunk-54-1.png" width="480" style="display: block; margin: auto;" />

> **Questions**: 
>
> * Which points do you think have high influence on the model relating `Familiarity` and `RTnaming`?

Let's check our intuition by coloring the points according to their CD values:
<div class="fold o">

```r
m <- lm(RTnaming ~ Familiarity, young_subset)
young_subset$CD <- cooks.distance(m)

p3 <- ggplot(young_subset, aes(Familiarity, RTnaming)) + 
  geom_point(size=3, aes(color=CD)) + 
  geom_smooth(method="lm")

p4 <- qplot(young_subset$CD)

grid.arrange(p3, p4, ncol = 2)
```

<img src="03-linear-regression_files/figure-html/unnamed-chunk-55-1.png" width="864" style="display: block; margin: auto;" />
</div>

Indeed, two points are outliers in terms of Cook's distance.  We expect to get a very different regression line by removing these two points (why?):


```r
young_subset2 <- filter(young_subset, CD < 0.4)

p5 <- ggplot(young_subset2, aes(Familiarity, RTnaming)) +
  geom_point(size=3) + 
  geom_smooth(method="lm")

grid.arrange(p5, p3, ncol = 2)
```

<img src="03-linear-regression_files/figure-html/unnamed-chunk-56-1.png" width="768" style="display: block; margin: auto;" />

After removal of these two outliers, the regression line has negative slope instead of flat slope.

### Outliers

Highly influential points are one example of *outliers*. It is also possible for points to have extreme values of the predictor or response, and yet not be highly influential.  There are different philosophies on how to detect outliers (visual inspection versus numeric measures), and what to do with them.  At a minimum:

* During data analysis, potential outliers should be flagged, examined, and considered for exclusion.

* Note what effect excluding outliers **would** have on the analysis.

* Report how outliers were dealt with, and ideally what effect they have on the analysis, in any write-up.

<!-- We may return later to different strategies for detecting and dealing with outliers.   -->

In many cases inspection of gross outliers will reveal data points that should be clearly excluded, such as a participant who always gave the same response, or data coding errors that cannot be corrected. In other cases the decision is more subjective.

### Regression assumptions: Reassurance

<!-- We have just spent quite a while discussing assumptions that are made by linear regression models (and one assumption that isn't made, but is important anyway---multicollinearity).   -->

It may seem a bit daunting that there are so many ways for a regression model to **not** be appropriate.
<!-- it is risky to  to actually fit a model to your data.   -->
It is important to take a step back, and remember: 
<!-- Some reassurance:  -->
when doing data analysis, you should just satisfy the assumptions of the statistical tool being used as best you can, and then  **fit and report your model anyway**.  You should not fit regression models blindly, and it's important to be aware of the assumptions being made by these models, and their limitations.  But the more you learn about regression (or any statistical technique), the easier it is to become paralyzed with fear by everything that could be wrong with your data.    Keep in mind the dictum of George Box: "Essentially, all models are wrong, but some are useful."  That is, every model is just that: a **model** of reality, that is only as useful as the insight it can offer about the questions of interest.  Often, minor violations of model assumptions don't affect the qualitative conclusions you can draw from the model.


## Model comparison {#lm-model-comparison}

So far in our discussion of regression, we have assumed that the form of the model is given: we already know what predictors will be used, and what terms will be included in the model.  In practice, with real data this is usually not clear---it is necessary to choose among several possible models of the same data, that differ in which terms they include. Choosing between models is called *variable selection* (or *model selection*). In order to perform variable selection, we need a way to perform *model comparison*: comparing two or more candidate models to assess which one is "better", in the sense of how well it fits the data relative to its expressive power (e.g. number of predictors).

We wish to compare models of the same dataset, with:

* Same response ($Y$)

* Different sets of predictors ($X_i$)

Model comparison techniques differ on whether they can compare "nested" models only, or both nested and "non-nested" models.

### Nested model comparison

Two models are *nested* if one is a subset of the other, in terms of the set of predictors included.

For example, these two models of `RT` (in the `english` dataset) are nested:

\begin{align*}
   \quad M_1 ~:~ Y &= \beta_0 + \beta_1 \cdot \texttt{WrittenFrequency} + \epsilon\\
   \quad M_2 ~:~ Y &= \beta_0 + \beta_1 \cdot \texttt{WrittenFrequency} + \beta_2 \cdot \texttt{Familiarity} + \epsilon
\end{align*}

$M_2$ is called the *full* or *superset* model, and $M_1$ the *reduced* or *subset* model.

To compare these two models, we wish to perform a hypothesis test of:
$$
H_0 : \beta_2 = 0
$$

More generally, we wish to compare two nested models:

* $M_1$: predictors $X_1, ..., X_q$
    
* $M_2$: predictors $X_1, ..., X_p$ (where $q < p$)
    
By performing this hypothesis test:

$$
H_0~:~\beta_{p + 1} = \beta_{p + 2} = \cdots = \beta_q = 0
$$
    
We fit both models, and obtain a sum of squared residuals for each: $RSS_1$, $RSS_2$, which serves as a measure of how well the model fits the data.  The RSS of a regression model can be scaled by $n-p-1$, where $p$ is the number of predictors in the model, to give a measure of how well the model fits the data **given the sample size and number of predictors**:
$$
RSS/(n-p-1)
$$
In addition, the difference in RSS values between two nested models, with $q$ and $p$ predictors ($p>q$), can be scaled to give a measure of how much RSS has gone down, **relative to what's expected** given the additional predictors:
$$
(RSS_1 - RSS_2)/(p-q)
$$

Thus, this test statistic:
$$
F = \frac{(RSS_1 - RSS_2)/(p - q)}{RSS_2 / (n - p - 1)}
$$
gives a measure of how much the unexplained variance is reduced in the full model, with respect to the reduced model. Intuitively, $R$ divides how much effect "dropping" the $q$ extra predictors has on explained variance.  Note that $RSS_2$ will always be smaller than $RSS_1$, since adding predictors to a model can't give a **worse** fit to the data. What our hypothesis test checks is: does the superset model fit the data **significantly** better than the subset model, given the added complexity?

It turns out that this test statistic follows an $F$ distribution (under the null hypothesis above) with $p-q$, $n-p$ degrees of freedom, written $F_{p-q,n-p}$.  So, we can perform hypothesis testing using an [$F$-test](https://en.wikipedia.org/wiki/F-test), which may be familiar if you have seen ANOVAs.

#### Example {- #c2ex1}

This model comparison addresses the question: does adding `AgeSubject` and `LengthInLetters` to `m1` significantly reduce its unexplained variance?


```r
m1 <- lm(RTlexdec ~ WrittenFrequency, english)
m2 <- lm(RTlexdec ~ WrittenFrequency + AgeSubject + LengthInLetters, english) 
anova(m1, m2)
```

```
## Analysis of Variance Table
## 
## Model 1: RTlexdec ~ WrittenFrequency
## Model 2: RTlexdec ~ WrittenFrequency + AgeSubject + LengthInLetters
##   Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
## 1   4566 91.194                                  
## 2   4564 35.004  2     56.19 3663.1 < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

The highly significant $F$-test means that adding these two variables does significantly improve the model. 

**Practical notes**: 

* In experimental literature (at least in linguistics), "model comparison" is often used as shorthand for "nested model comparison via an F test" when comparing two linear regressions, because this is the most common way to do so.  We will sometimes use this shorthand as well.

* The results of such a hypothesis test are usually reported in parentheses, like "Subject age and the word's length in letters together affect RT beyond word frequency $(F(2, 4564) = 3663, p<0.0001)$".

> **Questions**:
>
> * What are the reduced model and full model in the $F$ test reported at the bottom of every linear regression's output?
>
> 
> ```r
> summary(m2)
> ```
> 
> ```
> ## 
> ## Call:
> ## lm(formula = RTlexdec ~ WrittenFrequency + AgeSubject + LengthInLetters, 
> ##     data = english)
> ## 
> ## Residuals:
> ##      Min       1Q   Median       3Q      Max 
> ## -0.34438 -0.06041 -0.00695  0.05241  0.45157 
> ## 
> ## Coefficients:
> ##                    Estimate Std. Error t value Pr(>|t|)    
> ## (Intercept)       6.8293072  0.0079946 854.245   <2e-16 ***
> ## WrittenFrequency -0.0368919  0.0007045 -52.366   <2e-16 ***
> ## AgeSubjectyoung  -0.2217215  0.0025915 -85.556   <2e-16 ***
> ## LengthInLetters   0.0038897  0.0015428   2.521   0.0117 *  
> ## ---
> ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
> ## 
> ## Residual standard error: 0.08758 on 4564 degrees of freedom
> ## Multiple R-squared:  0.6887,	Adjusted R-squared:  0.6885 
> ## F-statistic:  3366 on 3 and 4564 DF,  p-value: < 2.2e-16
> ```
>
> Hint: fit `m0 <- lm(RTlexdec ~ 1, english)` and compare to `m2`.

### Non-nested model comparison {#non-nested-model-comparison}

In non-nested model comparison, one model isn't a subset of the other.  For example, the two models `RT ~ Frequency` and `RT ~ Familiarity` would be non-nested.

Non-nested models can no longer be compared using sums-of-squares (using an $F$ statistic). Instead, a very different approach is used: *information criteria*.  Instead of testing the hypothesis that certain model coefficients are zero, information criteria compare models based on two general goals:

1. Fit the data as well as possible

2. Have as few predictors as possible ("parsimony")
    
Different information criteria measures combine model likelihood ($L$) and number of predictors ($p$; the fewer the better) into a single value, of which the most common are:

* Akaike information criterion (AIC):

$$ AIC = 2p - 2 \log(L)$$

* Bayesian information criterion (BIC)

$$ BIC = p\log(n)  - 2 \log(L) $$

To apply either criterion, you calculate its value for each model in a set of candidate models, and pick the model with the lowest value.

Note that information criteria can be used to compare models regardless of whether they are nested or non-nested.


#### Example {- #c2ex4}


```r
m1 <- lm(RTlexdec ~ WrittenFrequency, english) 
m2 <- lm(RTlexdec ~ WrittenFrequency + AgeSubject, english)
m3 <- lm(RTlexdec ~ WrittenFrequency + AgeSubject + LengthInLetters, english)
m4 <- lm(RTlexdec ~ WrittenFrequency + LengthInLetters, english)
```

Using AIC and BIC to compare the models:


```r
AIC(m1, m2, m3, m4)
```

```
##    df       AIC
## m1  3 -4908.994
## m2  4 -9274.590
## m3  5 -9278.948
## m4  4 -4909.437
```


```r
BIC(m1, m2, m3, m4)
```

```
##    df       BIC
## m1  3 -4889.713
## m2  4 -9248.883
## m3  5 -9246.814
## m4  4 -4883.729
```

By AIC, we would choose `m3`, while by BIC we would choose `m2`.  This illustrates a couple of points:

* Different model selection criteria don't necessarily give the same answer.

* BIC tends to choose more parsimonious models than AIC.


### Variable selection {#c2varselect}

Now that we have seen some methods to compare models with different sets of predictors, we can turn to variable selection: how to decide what predictors to keep in a given model.

There is no single best way to do variable selection, out of context---each method has pros and cons, and what method to use depends on the goals of your study.  Your goal could be:

1. **Prediction** (estimating $Y$ for unseen data as accurately as possible)

2. **Explanation** (choosing the right model from one of several possible pre-specified choices)

3. **Exploratory** study

In #1 and #2, our higher-level goal is generalization about "the world" (unseen data). In #3, we are interested primarily in the data we have, and don't claim that our results generalize to the world.  We'll assume going forward that our goal is #1 or #2---many studies in language sciences are in fact exploratory, but at present this isn't a goal that leads to publication.

<!-- My (MS) impression is that inference of coefficient values is usually the goal in language sciences---we are usually interested in accurately determining the values of some parameters of interest, of which null-siginficance hypothesis testing is one part ("is the parameter zero?").  But, the choice of #2 over #1 may just be convention.   -->
<!-- TODO future: uncomment and flesh this out --> 
<!-- In other fields, prediction is seen as more important, and indeed there is a debate within the stats-for-language-sciences literature on which of #1 and #2 to prioritize, by default (REFS).  --> 

<!-- The choice of whether to prioritize (1) or (2)  has consequences for which variable selection method to use, but the issues are complex.  For example: roughly speaking, AIC tends to perform better for prediction, while BIC performs better for inference in the sense of choosing  -->
<!-- TODO Morgan: references for the last couple sentences -->

It is important to know that different variable selection methods result in different final models, appropriate for different goals, because  most papers that report variable selection simply say what they did, without justification.  It is up to the reader to think, what might the final model be if a different variable selection method were used?

There are three components of any variable selection method:

1. How models are compared (e.g. $F$ test on nested models, AIC)

2. How the quality of a single model is evaluated

3. How it's decided which of a set of models to choose, based on #1 and #2.

We'll consider a couple methods here, and cover others in later chapters.

#### Method 1: Nested model comparison

As noted above, "model comparison" is often used as shorthand for "comparison of nested models using F tests", when referring to linear regressions.

#### Exercise {-}

Which model of `m1`, `m2`, `m3` (from [the example above](#c2ex4)) would you choose based on nested model comparison ($F$ test)?

Note: `anova(x,y,z)` can be used as shorthand when `x`, `y`, `z` are nested models, and so on (`anova(w,x,y,z)`...).

#### Method 2: Stepwise variable selection

A common type of variable selection uses "choose automatically" for Component 3 above (how do we decide which model to prefer).  In *stepwise* variable selection, we decide whether to add or drop terms based on any model comparison procedure (Component 1): AIC, BIC, $F$ test with $p<0.05$, etc.

There are various flavors of stepwise model selection, such as  *forwards*, where you start with an empty model (intercept only) and add terms, and *backwards*, where you start with a full model and drop terms. 

#### Example: Stepwise backwards selection using AIC {-}

This is what the `step` function in R does by default.

1. Start with the complete regression model (all possible predictors) and obtain AIC

2. At each "step" we try all possible ways of removing one of the predictors, and whichever yields the lowest AIC value is kept.

3. Repeat steps 1-2 until we end up with a model with lower AIC value than any of the other possible models that you could produce by deleting one of its predictors.

Stepwise model selection is very popular, probably because it is easy to do using statistical software.  It can be helpful to select among a huge set of predictors as a first pass, but has serious drawbacks.
    
#### Exercise: 25 potential predictors {- #c2tfpp}

Fit the same model with 25 potential predictors to two random halves of [the `english` data](#c2engdata), then perform stepwise backwards selection:


```r
## split the English data in half, randomly:
set.seed(2903)
english.1 <- english %>% sample_n(round(nrow(english)/2))
english.2 <- english[!(row.names(english) %in% row.names(english.1)),]

## fit full model to each half-dataset
m1 <- lm(RTlexdec ~ (WrittenFrequency + Familiarity + AgeSubject + LengthInLetters + FamilySize)^3, english.1)
m2 <- lm(RTlexdec ~ (WrittenFrequency + Familiarity + AgeSubject + LengthInLetters + FamilySize)^3, english.2)

## trace=0 suppresses ouput
m1.stepped <- step(m1, trace=0)
m2.stepped <- step(m2, trace=0)

## the two resulting models
summary(m1.stepped)
```

```
## 
## Call:
## lm(formula = RTlexdec ~ WrittenFrequency + Familiarity + AgeSubject + 
##     LengthInLetters + FamilySize + WrittenFrequency:Familiarity + 
##     WrittenFrequency:AgeSubject + WrittenFrequency:LengthInLetters + 
##     WrittenFrequency:FamilySize + Familiarity:AgeSubject + Familiarity:LengthInLetters + 
##     Familiarity:FamilySize + AgeSubject:LengthInLetters + LengthInLetters:FamilySize + 
##     WrittenFrequency:Familiarity:AgeSubject + WrittenFrequency:AgeSubject:LengthInLetters, 
##     data = english.1)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.42008 -0.05175 -0.00326  0.04572  0.37444 
## 
## Coefficients:
##                                                   Estimate Std. Error
## (Intercept)                                       7.189260   0.045538
## WrittenFrequency                                 -0.084197   0.011610
## Familiarity                                      -0.058426   0.014137
## AgeSubjectyoung                                  -0.413999   0.058362
## LengthInLetters                                  -0.004709   0.008692
## FamilySize                                       -0.112072   0.018225
## WrittenFrequency:Familiarity                      0.008855   0.001203
## WrittenFrequency:AgeSubjectyoung                  0.045885   0.011738
## WrittenFrequency:LengthInLetters                  0.004581   0.002131
## WrittenFrequency:FamilySize                       0.005662   0.001863
## Familiarity:AgeSubjectyoung                       0.030455   0.008445
## Familiarity:LengthInLetters                      -0.006493   0.002830
## Familiarity:FamilySize                            0.007705   0.003262
## AgeSubjectyoung:LengthInLetters                   0.013042   0.011006
## LengthInLetters:FamilySize                        0.006179   0.003240
## WrittenFrequency:Familiarity:AgeSubjectyoung     -0.006894   0.001396
## WrittenFrequency:AgeSubjectyoung:LengthInLetters -0.003181   0.002059
##                                                  t value Pr(>|t|)    
## (Intercept)                                      157.875  < 2e-16 ***
## WrittenFrequency                                  -7.252 5.61e-13 ***
## Familiarity                                       -4.133 3.71e-05 ***
## AgeSubjectyoung                                   -7.094 1.74e-12 ***
## LengthInLetters                                   -0.542 0.588063    
## FamilySize                                        -6.149 9.17e-10 ***
## WrittenFrequency:Familiarity                       7.360 2.56e-13 ***
## WrittenFrequency:AgeSubjectyoung                   3.909 9.53e-05 ***
## WrittenFrequency:LengthInLetters                   2.150 0.031699 *  
## WrittenFrequency:FamilySize                        3.039 0.002405 ** 
## Familiarity:AgeSubjectyoung                        3.606 0.000317 ***
## Familiarity:LengthInLetters                       -2.295 0.021842 *  
## Familiarity:FamilySize                             2.362 0.018245 *  
## AgeSubjectyoung:LengthInLetters                    1.185 0.236150    
## LengthInLetters:FamilySize                         1.907 0.056609 .  
## WrittenFrequency:Familiarity:AgeSubjectyoung      -4.940 8.39e-07 ***
## WrittenFrequency:AgeSubjectyoung:LengthInLetters  -1.545 0.122548    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.07727 on 2267 degrees of freedom
## Multiple R-squared:  0.755,	Adjusted R-squared:  0.7533 
## F-statistic: 436.6 on 16 and 2267 DF,  p-value: < 2.2e-16
```

```r
summary(m2.stepped)
```

```
## 
## Call:
## lm(formula = RTlexdec ~ WrittenFrequency + Familiarity + AgeSubject + 
##     LengthInLetters + FamilySize + WrittenFrequency:Familiarity + 
##     WrittenFrequency:LengthInLetters + WrittenFrequency:FamilySize + 
##     Familiarity:AgeSubject + Familiarity:LengthInLetters + Familiarity:FamilySize + 
##     LengthInLetters:FamilySize + WrittenFrequency:LengthInLetters:FamilySize, 
##     data = english.2)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.30263 -0.05327 -0.00526  0.04654  0.50932 
## 
## Coefficients:
##                                               Estimate Std. Error t value
## (Intercept)                                  7.0214673  0.0616493 113.894
## WrittenFrequency                            -0.0338445  0.0137689  -2.458
## Familiarity                                 -0.0768169  0.0139772  -5.496
## AgeSubjectyoung                             -0.2047469  0.0113464 -18.045
## LengthInLetters                              0.0228065  0.0137176   1.663
## FamilySize                                  -0.0362279  0.0365663  -0.991
## WrittenFrequency:Familiarity                 0.0072404  0.0009183   7.884
## WrittenFrequency:LengthInLetters            -0.0024614  0.0029950  -0.822
## WrittenFrequency:FamilySize                 -0.0096833  0.0055442  -1.747
## Familiarity:AgeSubjectyoung                 -0.0042687  0.0028607  -1.492
## Familiarity:LengthInLetters                 -0.0044449  0.0028657  -1.551
## Familiarity:FamilySize                       0.0137811  0.0031320   4.400
## LengthInLetters:FamilySize                  -0.0059273  0.0082370  -0.720
## WrittenFrequency:LengthInLetters:FamilySize  0.0020024  0.0012937   1.548
##                                             Pr(>|t|)    
## (Intercept)                                  < 2e-16 ***
## WrittenFrequency                              0.0140 *  
## Familiarity                                 4.32e-08 ***
## AgeSubjectyoung                              < 2e-16 ***
## LengthInLetters                               0.0965 .  
## FamilySize                                    0.3219    
## WrittenFrequency:Familiarity                4.87e-15 ***
## WrittenFrequency:LengthInLetters              0.4113    
## WrittenFrequency:FamilySize                   0.0808 .  
## Familiarity:AgeSubjectyoung                   0.1358    
## Familiarity:LengthInLetters                   0.1210    
## Familiarity:FamilySize                      1.13e-05 ***
## LengthInLetters:FamilySize                    0.4718    
## WrittenFrequency:LengthInLetters:FamilySize   0.1218    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.08024 on 2270 degrees of freedom
## Multiple R-squared:  0.7444,	Adjusted R-squared:  0.7429 
## F-statistic: 508.4 on 13 and 2270 DF,  p-value: < 2.2e-16
```

Compare the two resulting models:

* How similar or different are the resulting terms in the model, and coefficient values?

* Why is the degree of (non-)similarity a problem?

----

In general, fully automatic model selection procedures are **dangerous**: they can easily find spurious effects, because automatically dropping terms with high $p$-values leads to inflation (= lower values) of significances for remaining terms.  Stepwise procedures can also (to a lesser extent) miss true effects.   Some statisticians recommend that automatic model selection should never be used at all, as it is simply too easy and too dangerous.  At a minimum, automatic model selection procedures should never be used **alone** for variable selection---that is, in the absence of another method, such as careful exploratory data analysis.  Despite this, stepwise procedures are widely used in language research, and you should critically examine any model arrived at in this way.^[For example: are there terms that should have been included, based on previous work---regardless of the result of a model comparison?  Are there higher-order interactions with borderline significance, that could be spuriously included due to automated selection?]

#### Method 3: @gelman2007data

@gelman2007data (p. 69) suggest a holistic approach to variable selection, that takes common sense into account in evaluating model quality (Component 2 above), in addition to quantitative model comparison (Component 1):

1. Include all predictors that, **for substantive reasons**, are expected to be important (e.g. speech rate when modeling vowel duration).

2. For predictors with large effects, consider interactions as well.

3. Now decide whether to exclude predictors, one at a time:

    * Not significant, coefficient has expected sign: Consider leaving in.
    
    * Not significant, wrong sign: Remove.
    
    * Significant, wrong sign: Think hard if something's wrong.
    
    * Significant, right sign: Keep in.
    
This approach requires you to have a sense of what the "right" and "wrong" sign of different coefficients are, either for substantive reasons (previous work suggests an effect direction), or from exploratory data analysis.  If your sense of "right" and "wrong" for a given model coefficient is based **purely** on EDA, it's important to remember that you may be simply modeling this dataset well (goal = exploratory analysis) rather than obtaining results that generalize to unseen data (goal = prediction or explanation).

#### Exercise: Phrase medial devoicing in European French {- #c2ex5}

This dataset is described in [the appendix](#devdata). We are interested in whether function words are shorter than content words---considering just words which are a consonant-vowel sequence---after controlling for the consonant and vowel identities (since different Cs/Vs are intrinsically longer than others) and how fast the speaker is speaking. 

In terms of the variables in this dataset:

* Is there is an effect of `func` on `syldur`...

* ... after controlling for `c1`, `v`, and `speechrate`?

**Part 1**: Model comparison

Use (nested) model comparison to compare these three models:

1. `syldur~(speechrate+c1+v)^2`

2. `syldur~(speechrate+c1+v)^2+func`

3. `syldur~(speechrate+c1+v)^2*func`

> **Questions**:
>
> * Which model ends up being chosen, and what do you conclude about the research question?

<div class="fold s o">

```r
#model comparison
m1 <- lm(syldur~(speechrate+c1+v)^2, df)
m2 <- lm(syldur~(speechrate+c1+v)^2+func, df)
m3 <- lm(syldur~(speechrate+c1+v)^2*func, df)
anova(m1, m2, m3)
```

```
## Analysis of Variance Table
## 
## Model 1: syldur ~ (speechrate + c1 + v)^2
## Model 2: syldur ~ (speechrate + c1 + v)^2 + func
## Model 3: syldur ~ (speechrate + c1 + v)^2 * func
##   Res.Df    RSS Df Sum of Sq      F   Pr(>F)   
## 1    529 528092                                
## 2    528 518990  1    9102.4 9.3121 0.002392 **
## 3    523 511219  5    7770.7 1.5900 0.161123   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(m2)
```

```
## 
## Call:
## lm(formula = syldur ~ (speechrate + c1 + v)^2 + func, data = df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -130.682  -21.233    0.304   20.544  103.960 
## 
## Coefficients: (1 not defined because of singularities)
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    247.5718    23.8023  10.401  < 2e-16 ***
## speechrate     -15.6166     3.2983  -4.735 2.82e-06 ***
## c1k            -89.2652    26.6617  -3.348 0.000872 ***
## c1p            -67.4682    39.4741  -1.709 0.088006 .  
## c1s            -27.7955    26.1708  -1.062 0.288683    
## c1t            -68.6392    27.3605  -2.509 0.012416 *  
## vu               4.6582    31.5829   0.147 0.882802    
## vy              16.2882    20.2343   0.805 0.421194    
## funcf          -13.8005     4.5350  -3.043 0.002458 ** 
## speechrate:c1k   9.5259     3.7368   2.549 0.011077 *  
## speechrate:c1p   9.2280     5.8037   1.590 0.112428    
## speechrate:c1s   4.4121     3.5902   1.229 0.219637    
## speechrate:c1t   7.5638     3.7154   2.036 0.042268 *  
## speechrate:vu   -0.6849     3.9631  -0.173 0.862851    
## speechrate:vy   -1.6412     2.6474  -0.620 0.535579    
## c1k:vu          19.8107    22.1307   0.895 0.371104    
## c1p:vu          -1.3369    25.6737  -0.052 0.958489    
## c1s:vu           0.9348    22.3409   0.042 0.966639    
## c1t:vu               NA         NA      NA       NA    
## c1k:vy          -3.1728    13.8966  -0.228 0.819491    
## c1p:vy          -9.7528    15.2713  -0.639 0.523338    
## c1s:vy         -18.2987    11.6317  -1.573 0.116279    
## c1t:vy          -5.4756    11.9582  -0.458 0.647219    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 31.35 on 528 degrees of freedom
## Multiple R-squared:  0.3012,	Adjusted R-squared:  0.2734 
## F-statistic: 10.84 on 21 and 528 DF,  p-value: < 2.2e-16
```
</div> 

**Part 2**: Stepwise backwards selection

Apply stepwise backwards model selection to this model:

* `syldur~(speechrate+c1+v+func)^2`

using `step`.  


<div class="fold s o">

```r
#Backwards selection
m = lm(syldur~(speechrate+c1+v+func)^2, df)
summary(step(m, trace=0))
```

```
## 
## Call:
## lm(formula = syldur ~ speechrate + c1 + func + speechrate:c1 + 
##     c1:func, data = df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -129.449  -19.780    0.762   21.343  104.048 
## 
## Coefficients: (2 not defined because of singularities)
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)     255.813     20.798  12.300  < 2e-16 ***
## speechrate      -16.508      2.991  -5.520  5.3e-08 ***
## c1k             -87.803     25.813  -3.401 0.000720 ***
## c1p             -71.475     37.858  -1.888 0.059566 .  
## c1s             -41.730     23.963  -1.741 0.082180 .  
## c1t             -64.292     24.776  -2.595 0.009719 ** 
## funcf           -21.398      5.558  -3.850 0.000132 ***
## speechrate:c1k   10.350      3.559   2.908 0.003785 ** 
## speechrate:c1p    9.105      5.564   1.637 0.102310    
## speechrate:c1s    5.058      3.422   1.478 0.139894    
## speechrate:c1t    7.322      3.498   2.093 0.036832 *  
## c1k:funcf        -1.833      8.565  -0.214 0.830655    
## c1p:funcf            NA         NA      NA       NA    
## c1s:funcf        17.373      7.218   2.407 0.016427 *  
## c1t:funcf            NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 31.14 on 537 degrees of freedom
## Multiple R-squared:  0.2987,	Adjusted R-squared:  0.283 
## F-statistic: 19.06 on 12 and 537 DF,  p-value: < 2.2e-16
```
</div>


> **Questions**:
>
> * Which model ends up being chosen, and what do you conclude about the research question?
> 
> * What would be different about your answer to the research question using stepwise backwards selection versus nested model comparison? Why?

**Part 3**: Gelman & Hill's method

This method requires more domain specific knowledge, so we'll walk through it together.

First, all four possible predictors are included as main effects---they are all expected to affect vowel duration for substantive reasons:


```r
# G&H
m1 <- lm(syldur~speechrate+c1+v+func, df)
summary(m1)
```

```
## 
## Call:
## lm(formula = syldur ~ speechrate + c1 + v + func, data = df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -131.151  -21.337    0.863   20.419   98.228 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 208.8600     7.9734  26.195  < 2e-16 ***
## speechrate   -9.5866     0.9546 -10.043  < 2e-16 ***
## c1k         -26.6525     5.8330  -4.569 6.07e-06 ***
## c1p         -11.0561     6.9893  -1.582  0.11426    
## c1s          -2.9871     5.3464  -0.559  0.57659    
## c1t         -18.3920     5.8075  -3.167  0.00163 ** 
## vu           12.3446     6.1420   2.010  0.04494 *  
## vy           -2.0698     3.4933  -0.592  0.55377    
## funcf       -13.3345     3.2684  -4.080 5.19e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 31.47 on 541 degrees of freedom
## Multiple R-squared:  0.2787,	Adjusted R-squared:  0.2681 
## F-statistic: 26.13 on 8 and 541 DF,  p-value: < 2.2e-16
```

Every predictor has a significant effect. Let's suppose that `speechrate` in particular has a large effect (we'll discuss soon how to assess effect size).  We thus consider interactions with speech rate:


```r
m2 <- lm(syldur~speechrate*(func+c1+v), df)
summary(m2)
```

```
## 
## Call:
## lm(formula = syldur ~ speechrate * (func + c1 + v), data = df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -130.275  -20.390    0.522   20.501  103.274 
## 
## Coefficients:
##                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)       255.785     22.952  11.144  < 2e-16 ***
## speechrate        -16.386      3.274  -5.005 7.58e-07 ***
## funcf             -29.289     17.059  -1.717  0.08658 .  
## c1k               -86.729     28.237  -3.071  0.00224 ** 
## c1p               -66.594     38.304  -1.739  0.08269 .  
## c1s               -29.059     26.392  -1.101  0.27137    
## c1t               -60.538     27.829  -2.175  0.03004 *  
## vu                 -6.158     27.997  -0.220  0.82599    
## vy                  3.596     18.342   0.196  0.84463    
## speechrate:funcf    2.364      2.384   0.992  0.32172    
## speechrate:c1k      8.949      4.009   2.232  0.02604 *  
## speechrate:c1p      8.235      5.645   1.459  0.14522    
## speechrate:c1s      3.813      3.775   1.010  0.31291    
## speechrate:c1t      6.203      3.965   1.564  0.11829    
## speechrate:vu       2.217      3.953   0.561  0.57512    
## speechrate:vy      -1.013      2.585  -0.392  0.69529    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 31.32 on 534 degrees of freedom
## Multiple R-squared:  0.2945,	Adjusted R-squared:  0.2747 
## F-statistic: 14.86 on 15 and 534 DF,  p-value: < 2.2e-16
```

There seems to be a significant interaction with preceding consonant, in particular preceding /k/ means less duration decrease with increasing rate.  There isn't an immediately obvious explanation for this interaction, so it's not "wrong sign", and it's fine to leave in the model.
<!-- This makes sense, because in languages with "short lag" stops (like French p/t/k), duration of these stops depends minimally on speech rate.   -->
<!-- So, we flag the `speechrate:c1` term for inclusion. -->

<!-- TODO future: include reference here and make more understandable if not a phonetician -->

`func` also has a large effect, and is of primary interest, so we consider its potential interactions:


```r
m3 <- lm(syldur~func*(speechrate+c1+v), df)
summary(m3)
```

```
## 
## Call:
## lm(formula = syldur ~ func * (speechrate + c1 + v), data = df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -132.663  -21.550    1.495   18.620  101.766 
## 
## Coefficients: (4 not defined because of singularities)
##                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)      223.1977    11.2756  19.795  < 2e-16 ***
## funcf            -46.3965    14.9707  -3.099  0.00204 ** 
## speechrate       -11.8593     1.4817  -8.004 7.47e-15 ***
## c1k              -18.3159     7.5125  -2.438  0.01509 *  
## c1p              -11.6366     6.9622  -1.671  0.09522 .  
## c1s               -7.8271     5.5722  -1.405  0.16070    
## c1t              -13.3023     6.4106  -2.075  0.03846 *  
## vu                11.5966     6.8487   1.693  0.09098 .  
## vy                 0.8120     4.3953   0.185  0.85350    
## funcf:speechrate   3.4852     1.9453   1.792  0.07376 .  
## funcf:c1k          0.3863    10.7540   0.036  0.97136    
## funcf:c1p              NA         NA      NA       NA    
## funcf:c1s         19.3740     8.7143   2.223  0.02661 *  
## funcf:c1t              NA         NA      NA       NA    
## funcf:vu               NA         NA      NA       NA    
## funcf:vy               NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 31.22 on 538 degrees of freedom
## Multiple R-squared:  0.2941,	Adjusted R-squared:  0.2797 
## F-statistic: 20.38 on 11 and 538 DF,  p-value: < 2.2e-16
```

There's a significant `func`:`c1` interaction. It's not clear what is the "right" or "wrong" sign here, but since `func` is of primary interest, it is prudent to  include this term in the model, in case it's controlling for something that might otherwise spuriously give a significant `func` effect.

Our final model is then:

```r
m4 <- lm(syldur~speechrate+c1+func+speechrate:c1+func:c1, df)
summary(m4)
```

```
## 
## Call:
## lm(formula = syldur ~ speechrate + c1 + func + speechrate:c1 + 
##     func:c1, data = df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -129.449  -19.780    0.762   21.343  104.048 
## 
## Coefficients: (2 not defined because of singularities)
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)     255.813     20.798  12.300  < 2e-16 ***
## speechrate      -16.508      2.991  -5.520  5.3e-08 ***
## c1k             -87.803     25.813  -3.401 0.000720 ***
## c1p             -71.475     37.858  -1.888 0.059566 .  
## c1s             -41.730     23.963  -1.741 0.082180 .  
## c1t             -64.292     24.776  -2.595 0.009719 ** 
## funcf           -21.398      5.558  -3.850 0.000132 ***
## speechrate:c1k   10.350      3.559   2.908 0.003785 ** 
## speechrate:c1p    9.105      5.564   1.637 0.102310    
## speechrate:c1s    5.058      3.422   1.478 0.139894    
## speechrate:c1t    7.322      3.498   2.093 0.036832 *  
## c1k:funcf        -1.833      8.565  -0.214 0.830655    
## c1p:funcf            NA         NA      NA       NA    
## c1s:funcf        17.373      7.218   2.407 0.016427 *  
## c1t:funcf            NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 31.14 on 537 degrees of freedom
## Multiple R-squared:  0.2987,	Adjusted R-squared:  0.283 
## F-statistic: 19.06 on 12 and 537 DF,  p-value: < 2.2e-16
```

> **Questions**:
>
> * What do you conclude about the research question, based on the final model?
>
> * What does the signifcant `func`:`c1` interaction mean for the research question?  (Does the effect of `func` have the same direction regardless of `c1`?)

### Interpretability issues

A crucial aspect of interpreting regression model results is being able to interpret and compare coefficient values.

Consider this model for [the `english` data](#c2engdata):


```r
m6 <- lm(RTlexdec ~ WrittenFrequency + LengthInLetters + AgeSubject, english)
summary(m6)
```

```
## 
## Call:
## lm(formula = RTlexdec ~ WrittenFrequency + LengthInLetters + 
##     AgeSubject, data = english)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.34438 -0.06041 -0.00695  0.05241  0.45157 
## 
## Coefficients:
##                    Estimate Std. Error t value Pr(>|t|)    
## (Intercept)       6.8293072  0.0079946 854.245   <2e-16 ***
## WrittenFrequency -0.0368919  0.0007045 -52.366   <2e-16 ***
## LengthInLetters   0.0038897  0.0015428   2.521   0.0117 *  
## AgeSubjectyoung  -0.2217215  0.0025915 -85.556   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.08758 on 4564 degrees of freedom
## Multiple R-squared:  0.6887,	Adjusted R-squared:  0.6885 
## F-statistic:  3366 on 3 and 4564 DF,  p-value: < 2.2e-16
```

Two issues make the coefficients difficult to interpret:

1. The **intercept value** is not interesting or meaningful

    * Predicted response when all $X$ are 0.
    
    * What does `WrittenFrequency`=0, `LengthInLetters`=0 mean?

2. **Coefficient values aren't comparable**, because predictors are on different scales.

    * Change of 1 in `AgeSubject` covers 100\% of the data, while change of 1 in `WrittenFrequency` only covers part of the data.
    
    * We need comparable coefficient values to help assess *effect size*.
    
#### Solutions

We can address these two issues by transforming predictors before fitting the model. There are two common ways to *standardize* predictors, considering for now just continuous predictors and binary factors (no multi-level factors):

1. *z-score*: subtract mean (center) and divide by standard deviation
    
    * This measure is similar to Cohen's $d$ for binary predictors (which we'll discuss soon)
    
    * `scale()` function in R
    
2. Alternative method (@gelman2007data §4.2):

    * Continuous predictors: center and divide by **2** SD.
    
    * Binary predictors: transform to have mean 0 and difference of 1 between values (i.e. -0.5/0.5 for balanced data)
    
    * Interpretation of "unit change of 1" is similar for continuous and binary predictors $\implies$ can compare effect sizes.
    
    * Use `rescale()` in the `arm` package.
    
Method 2, while less standard, has advantages discussed by @gelman2007data, and we'll use it going forward.

New interpretations of coefficients:

* **Intercept**: predicted $Y$ when all predictors are held at mean value
    
* **Main effect** coefficients for continuous predictors: predicted change in $Y$ when predictor changed by 2 SD, with other predictors held at **mean** values.
    
* **Main effect** coefficients for binary predictors: difference between the two levels. (Which is the same as 2 SD.)

**Practical notes**: 

* When you report standardization of variables in a paper, report the information needed to replicate what you did---not the same of particular software package or function used. For example, you'd write "continuous variables were standardized by centering and dividing by two standard deviations", not "continuous variables were standardized using the `rescale()` function in the `arm` package". The latter kind of report is common and unhelpful.

* "Rescaling", "rescaled variables" etc. are **not** standard terminology, and should not be used in any write-up.  Instead, refer to "standardized" variables, defining at some point what "standardized" means.

#### Example {-}

Model of `RTlexdec` as a function of `WrittenFrequency`, `LengthInLetters`, `AgeSubject`, using raw and standardized predictors:

Before standardizing:


```r
summary(m6)
```

```
## 
## Call:
## lm(formula = RTlexdec ~ WrittenFrequency + LengthInLetters + 
##     AgeSubject, data = english)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.34438 -0.06041 -0.00695  0.05241  0.45157 
## 
## Coefficients:
##                    Estimate Std. Error t value Pr(>|t|)    
## (Intercept)       6.8293072  0.0079946 854.245   <2e-16 ***
## WrittenFrequency -0.0368919  0.0007045 -52.366   <2e-16 ***
## LengthInLetters   0.0038897  0.0015428   2.521   0.0117 *  
## AgeSubjectyoung  -0.2217215  0.0025915 -85.556   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.08758 on 4564 degrees of freedom
## Multiple R-squared:  0.6887,	Adjusted R-squared:  0.6885 
## F-statistic:  3366 on 3 and 4564 DF,  p-value: < 2.2e-16
```

After standardizing:


```r
english2 <- mutate(english,
       WrittenFrequency = rescale(WrittenFrequency),
       LengthInLetters  = rescale(LengthInLetters),
       AgeSubject       = rescale(AgeSubject))
m7 <-  lm(RTlexdec ~ WrittenFrequency + LengthInLetters + AgeSubject, english2)
summary(m7)
```

```
## 
## Call:
## lm(formula = RTlexdec ~ WrittenFrequency + LengthInLetters + 
##     AgeSubject, data = english2)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.34438 -0.06041 -0.00695  0.05241  0.45157 
## 
## Coefficients:
##                   Estimate Std. Error  t value Pr(>|t|)    
## (Intercept)       6.550097   0.001296 5055.020   <2e-16 ***
## WrittenFrequency -0.136025   0.002598  -52.366   <2e-16 ***
## LengthInLetters   0.006549   0.002598    2.521   0.0117 *  
## AgeSubject       -0.221721   0.002592  -85.556   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.08758 on 4564 degrees of freedom
## Multiple R-squared:  0.6887,	Adjusted R-squared:  0.6885 
## F-statistic:  3366 on 3 and 4564 DF,  p-value: < 2.2e-16
```

Interpretations of coefficients in the  model using standardized predictors:

* `(Intercept)` row: Mean RT at mean $X_i$, for both old and young participants

* `WrittenFrequency`, `LengthInLetters` rows: Predicted change between mean($X_i$) and lower or upper bound of $X_i$'s range

* `AgeSubject` row: Difference in group means between young and old participants

> **Questions**:
>
> * How do these interpretations differ from the coefficient interpretations for the model using raw predictors (`m6`)?
>
> * What is the relative importance of the three predictors for predicting reaction time?

### Interim recipe: Building a multiple linear regression model

Caveat: there is no single recipe for building a regression model of a dataset to address a research question!   Nonetheless, here is one possible recipe, which we have given as part of linear regression project directions in classes.^[Note that this recipe explicitly includes exploratory data analysis, which may not be appropriate depending on your analytic philosophy or conventions in your subfield.]

1. **Preliminaries**

    * State the problem (what are the goals of this analysis?)
    
    * Select the reponse(s) and relevant predictors

    * Continuous variables: center and scale, possibly transform to normality
    
    * Categorical variables: center / choose coding scheme

2. **Do EDA**:

    * What patterns are there that "should"" come out in a statistical model?
    
    * Identify potential outliers

    * (if applicable) Possible interactions? By-subject, by-item plots.
    
3. **Choose models to fit**, based on steps 1-2

4. **Fit candidate model(s)**

5. **Model criticism**

    * Assess linearity assumption
    
    * Examine distribution of (standardized) residuals
    
    * Examine plots of residuals against fitted values, predictors
    
    * Look for influential points
    
    * Check for collinearity
    
    * (Other steps possible---especially checking model *robustness*---but not covered in this book.)
    
6. **Revise**: Based on step 5, possibly:

    * Exclude some data
    
    * Transform the predictor and/or response
    
    * Then re-fit the model (Step 4)
    
7. **Iterate**: repeat Steps 4-6 until model criticism is satisfactory

    * (0--2 iterations)

## Solutions {#c2solns}

### Multiple linear regression: Solutions

**Q**: What does it mean that this coefficient is positive? 

**A**: The effect of `WrittenFrequency` is closer to zero (= less negative) for younger speakers.

#### Regression equation 1 {- #c2sol1}

\begin{equation*}
  \text{Predicted } \texttt{RTlexdec} = \underbrace{6.552}_{\hat{\beta}_0} + (\underbrace{-0.011}_{\hat{\beta}_1} \cdot 3) + (\underbrace{-0.365}_{\hat{\beta}_2} \cdot \underbrace{0}_{\texttt{AgeSubject=='old'}}) + (\underbrace{-0.005}_{\hat{\beta}_3} \cdot 3 \cdot \underbrace{0}_{\texttt{AgeSubject=='old'}}) = 6.519
\end{equation*}

#### Regression equation 2 {- #c2sol2}

\begin{equation*}
  \text{Predicted } \texttt{RTlexdec} = \underbrace{6.552}_{\hat{\beta}_0} + (\underbrace{-0.011}_{\hat{\beta}_1} \cdot 3) + (\underbrace{-0.365}_{\hat{\beta}_2} \cdot \underbrace{1}_{\texttt{AgeSubject=='young'}}) + (\underbrace{-0.005}_{\hat{\beta}_3} \cdot 3 \cdot \underbrace{1}_{\texttt{AgeSubject=='young'}}) = 6.139
\end{equation*}

### Linear regression assumptions: Solutions

**Q**: Is this the case?

**A**: It looks like `rintensity` and `rpitch` have a roughly linear relationship, while the relationships between `rpitch` and `rduration` as well as between `rduration` and `rpitch` are nonlinear.

---

**Q**: Why do the residuals have this distribution?

**A**: The distribution of $Y$ is highly right-skewed: there is a lot of data with $Y \approx 1$, and a long tail of data with $Y \in [2,5]$ (examine `hist(data$rhymeRating)`).  As a result, the fitted line is near $Y=1$ for most values of $X$, so the residuals also have a right-skewed distribution: points with $Y \approx 1$ correspond to residuals near the mode, and other points correspond to residuals in the right tail.  In short, the residual distribution is highly skewed because the distribution of the response ($Y$) is highly skewed.

---

**Q**: Is the linearity assumption met in each case?

**A**: No (left), Yes (right).  The linearity assumption isn't met when $Y$ is untransformed word frequency --- or at least it's unclear -- whereas the assumption looks safe for log-transformed $Y$.

---

### Model comparison: Solutions

<!-- TODO: future -->
<!-- #### Solutions to [25 potential predictors](#c2tfpp) exercise: {-} -->

<!-- TODO ^^ -->

**Q**: What are the reduced model and full model in the $F$ test reported at the bottom of every linear regression's output?

**A**: The full model is the model you fitted in the linear regression.  The reduced model is the model including only an intercept.  For example, in this model:

```r
m2 <- lm(RTlexdec ~ WrittenFrequency + AgeSubject + LengthInLetters, english) 
summary(m2)
```

```
## 
## Call:
## lm(formula = RTlexdec ~ WrittenFrequency + AgeSubject + LengthInLetters, 
##     data = english)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.34438 -0.06041 -0.00695  0.05241  0.45157 
## 
## Coefficients:
##                    Estimate Std. Error t value Pr(>|t|)    
## (Intercept)       6.8293072  0.0079946 854.245   <2e-16 ***
## WrittenFrequency -0.0368919  0.0007045 -52.366   <2e-16 ***
## AgeSubjectyoung  -0.2217215  0.0025915 -85.556   <2e-16 ***
## LengthInLetters   0.0038897  0.0015428   2.521   0.0117 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.08758 on 4564 degrees of freedom
## Multiple R-squared:  0.6887,	Adjusted R-squared:  0.6885 
## F-statistic:  3366 on 3 and 4564 DF,  p-value: < 2.2e-16
```

The $F$ test is for the comparison with the model `m0 <- lm(RTlexdec ~ 1, english)`.

---

**Q**:  Which model of `m1`, `m2`, `m3` (from [the example above](#c2ex4)) would you choose based on nested model comparison ($F$ test)?

**A**: Model 3 > Model 2 > Model 1

---

**Q**:  How do these interpretations differ from the coefficient interpretations for the model using raw predictors (m6)?  What is the relative importance of the three predictors for predicting reaction time?

**A**:

* Original model: change in RT per change of 1 in word frequency

* New model: change in RT per change of 2 SD in word frequency

The interpretation of the `LengthInLetters` coefficient is:

* Original model: change in RT for each additional letter in the word

* New model: change in RT per change of 2 SD word length (in letters)

The interpretation of the `AgeSubject` coefficient is the same in the two models.  Although `AgeSubject` is a two-level factor in one model and a centered numeric variable in the other model, the regression coefficient captures the difference between the two levels (old and young speakers) in each case.

The relative importance of the three predictors follows from the magnitudes of the coefficients in the model using standardized predictors: `AgeSubject` > `WrittenFrequency` > `LengthInLetters`.

<!-- TODO: future -->

<!-- #### Solutions to [Phrase medial devoicing](#c2ex5) exercise questions: -->

<!-- PART1: -->

<!-- **Q**: Which model ends up being chosen, and what do you conclude about the research question? -->

<!-- **A**: TODO -->

<!-- PART2:  -->

<!-- **Q**: Which model ends up being chosen, and what do you conclude about the research question? -->

<!-- **A**: TODO -->

<!-- **Q**: What would be different about your answer to the research question using stepwise backwards selection versus nested model comparison? Why? -->

<!-- **A**: TODO -->

<!-- PART3: -->

<!-- **Q**: What do you conclude about the research question, based on the final model? -->

<!-- **A**: TODO -->

<!-- **Q**:  What does the signifcant `func`:`c1` interaction mean for the research question?  (Does the effect of `func` have the same direction regardless of `c1`?) -->

<!-- **A**: TODO -->

<!-- [^1]: The studentized and standardized residuals, or "externally studentized" and "internally studentized" residuals (in @chatterjee2012regression), differ slightly in how they estimate the error variance:  a leave-one-out estimate versus an estimate using all observations. This difference shouldn't matter much except when certain observations are highly influential or in small datasets. -->

<!-- [^2]: In technical terms: the *design matrix* $X$ (where each column is the values of predictor $i$, across all observations) is not *full rank*, and the matrix $X^T X$, which is used to find the least-squares regression coefficient estimates, is *singular* (alternatively, there is a *singularity*)  Even if you don't understand the math of linear regression, it is useful to know that when you encounter these terms in R output it usually means some variable can be perfectly predicted from the others, and this is a problem. -->

<!-- [^3]: Note that you can have $R^2<1$ even when one predictor is completely predictable from other predictors---as long as this dependence is not a linear function.  Sometimes it does not make conceptual sense to have two predictors related in this way in a model (e.g. VOT and log(VOT)), while in other settings it does (e.g. linear and quadratic terms for the same variable). -->

<!-- [^4]: However, when standard errors are high, the coefficient estimates themselves are by definition very uncertain, so in this sense collinearity "affects" coefficient values. -->

<!-- [^5]: For example: are there terms that should have been included, based on previous work---regardless of the result of a model comparison?  Are there higher-order interactions with borderline significance, that could be spuriously included due to automated selection? -->
