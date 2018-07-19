
Analysis of Red Wine by Michael Bong
========================================================

```{r echo=FALSE, message=FALSE, warning=FALSE, packages}
# Load all of the packages that you end up using in your analysis in this code
# chunk.

# Notice that the parameter "echo" was set to FALSE for this code chunk. This
# prevents the code from displaying in the knitted HTML output. You should set
# echo=FALSE for all code chunks in your file, unless it makes sense for your
# report to show the code that generated a particular plot.

# The other parameters for "message" and "warning" should also be set to FALSE
# for other code chunks once you have verified that each plot comes out as you
# want it to. This will clean up the flow of your report.

# install.packages("ggplot2") 
# install.packages("knitr")
# install.packages("dplyr")
# install.packages("gridExtra")
# install.packages('GlobalOptions')
# install.packages('GGally')
# install.packages('RColorBrewer')
# install.packages('cowplot')
# install.packages('memisc')

library(ggplot2)
library(knitr)
library(dplyr)
library(gridExtra)
library(GlobalOptions)
library(GGally)
library(RColorBrewer)
library(cowplot)
library(memisc)

#{r global_options, include=FALSE}
knitr::opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE)

```

# The data set
I have chosen to analyse the red wine data set to determine the key variables that determine wine quality. My aim is to use my learnings here to enable me to provide a (slightly) more sophisticated commentary on which red wines would taste good in the future without actually tasting it!

In this data set, each record represents a type of red wine. The characteristics of each wine is provided (such as fixed acidity levels, pH etc.) and finally, each wine is assigned a quality score (ranging from 0 to 10), based on sensory data.  

# Initial look at the data
The red wine data set is loaded and the first 10 rows is printed out below.
```{r echo=FALSE, Load_the_Data}
# Load the Data
rw <- read.csv('wineQualityReds.csv')
head(rw, 10)
```

The variables are as follows.
```{r}
names(rw)
```

Units for each variable as follows:
   1. fixed acidity (tartaric acid - g / dm^3)
   2. volatile acidity (acetic acid - g / dm^3)
   3. citric acid (g / dm^3)
   4. residual sugar (g / dm^3)
   5. chlorides (sodium chloride - g / dm^3
   6. free sulfur dioxide (mg / dm^3)
   7. total sulfur dioxide (mg / dm^3)
   8. density (g / cm^3)
   9. pH
   10. sulphates (potassium sulphate - g / dm3)
   11. alcohol (% by volume)
   12. quality (score between 0 and 10, based on sensory data)


# Univariate Plots Section

This data set has 1599 records across 13 variables.
```{r echo=FALSE, Univariate_Plots}
dim(rw)
```

The structure of the data is as follows.
```{r}
str(rw)
```

A quick summary of the data set below.
```{r}
summary(rw)
```

```{r, Custom_Functions_Stats}
#Create a standard deviation printout
fun_stdev <- function(input) {
  stdev <- sd(input)
  
  return(paste('Standard deviation is: ', stdev))
}

# fun_stdev(rw$fixed.acidity)


#Create a standard deviation printout with log10 scale
fun_stdev_log10 <- function(input) {
  stdev <- sd(log10(input+1))
  
  return(paste('Standard deviation 1og10 scale is: ', stdev))
}

# fun_stdev_log10(rw$fixed.acidity)


#Create a summary printout for chosen variable vs quality
fun_summary_vs_quality <- function(input)
  {
  return (by(input, rw$quality, summary))
  }

# fun_summary_vs_quality(rw$fixed.acidity)

```


```{r, Custom_Functions_Plots}
# Create a function to generate generic histograms, without labels
fun_hist <- function(input, input_name) 
  {
  
    input_min <- min(input)  
    input_q1 <- quantile(input, 0.25)
    input_median <- median(input)
    input_q3 <- quantile(input, 0.75)
    input_max <- max(input)
    input_mean <- mean(input)
    
    result_plot <-
      ggplot(data = rw, 
             aes(x = input)) +
      geom_histogram() +
      labs(x = input_name) + 
      geom_vline(xintercept = input_min, col = 'red') + 
      geom_vline(xintercept = input_q1, col = 'blue') + 
      geom_vline(xintercept = input_median, col = 'orange') + 
      geom_vline(xintercept = input_q3, col = 'blue') + 
      geom_vline(xintercept = input_max, col = 'red') + 
      geom_vline(xintercept = input_mean, col = 'green')  

    return(result_plot)
  }

# fun_hist(rw$fixed.acidity, 'fixed.acidity')


# Create a function to generate generic histograms, with labels
fun_hist_label <- function(input, input_name) 
  {
    annot_y <- 0
  
    input_min <- min(input)  
    input_q1 <- quantile(input, 0.25)
    input_median <- median(input)
    input_q3 <- quantile(input, 0.75)
    input_max <- max(input)
    input_mean <- mean(input)
    
    result_plot <-
      ggplot(data = rw, 
             aes(x = input)) +
      geom_histogram() +
      labs(x = input_name) + 
      geom_vline(xintercept = input_min, col = 'red') + 
        annotate('text', label = 'min', 
                 y = annot_y, 
                 x = input_min, 
                 col = 'red', angle = 90) +
      geom_vline(xintercept = input_q1, col = 'blue') + 
        annotate('text', label = 'q1', 
                 y = annot_y, 
                 x = input_q1, 
                 col = 'blue', angle = 90) +
      geom_vline(xintercept = input_median, col = 'orange') + 
        annotate('text', label = 'median', 
                 y = annot_y, 
                 x = input_median, 
                 col = 'orange', angle = 90) +
      geom_vline(xintercept = input_q3, col = 'blue') + 
        annotate('text', label = 'q3', 
                 y = annot_y, 
                 x = input_q3, 
                 col = 'blue', angle = 90) +
      geom_vline(xintercept = input_max, col = 'red') + 
        annotate('text', label = 'max', 
                 y = annot_y, 
                 x = input_max, 
                 col = 'red', angle = 90) +
      geom_vline(xintercept = input_mean, col = 'green') + 
        annotate('text', label = 'mean', 
                 y = annot_y, 
                 x = input_mean, 
                 col = 'green', angle = 90)
    
    return(result_plot)
  }

# fun_hist_label(rw$fixed.acidity, 'fixed.acidity')


# Create a function to generate generic boxplots, with labels
fun_box_label <- function(input, input_name) 
  {
    result_box <- 
      ggplot(data = rw,
             aes(x = 1, y = input)) +
      geom_boxplot(col = 'red', alpha = 0.2) +
      geom_jitter(alpha = 0.1) +
      labs(y = input_name)
    
    return(result_box)
}
# fun_box_label(rw$fixed.acidity, 'fixed.acidity')


# Create a function to generate generic histograms and boxplots, with labels
fun_hist_box_label <- function(input, input_name) 
  {
    return(grid.arrange(fun_box_label(input, input_name),
                        fun_hist_label(input, input_name), ncol = 2)
           )
}

# fun_hist_box_label(rw$fixed.acidity, 'fixed.acidity')


# Create a function to generate generic histograms, with labels, with log10 scale
fun_hist_label_log10 <- function(input_raw, input_raw_name) 
  {
    annot_y <- 0
    
    input <- log10(input_raw)
    input_name <- paste('log10(',input_raw_name,')')
      
    input_min <- min(input)  
    input_q1 <- quantile(input, 0.25)
    input_median <- median(input)
    input_q3 <- quantile(input, 0.75)
    input_max <- max(input)
    input_mean <- mean(input)
    
    result_plot <-
      ggplot(data = rw, 
             aes(x = input)) +
      geom_histogram() +
      labs(x = input_name) + 
      geom_vline(xintercept = input_min, col = 'red') + 
        annotate('text', label = 'min', 
                 y = annot_y, 
                 x = input_min, 
                 col = 'red', angle = 90) +
      geom_vline(xintercept = input_q1, col = 'blue') + 
        annotate('text', label = 'q1', 
                 y = annot_y, 
                 x = input_q1, 
                 col = 'blue', angle = 90) +
      geom_vline(xintercept = input_median, col = 'orange') + 
        annotate('text', label = 'median', 
                 y = annot_y, 
                 x = input_median, 
                 col = 'orange', angle = 90) +
      geom_vline(xintercept = input_q3, col = 'blue') + 
        annotate('text', label = 'q3', 
                 y = annot_y, 
                 x = input_q3, 
                 col = 'blue', angle = 90) +
      geom_vline(xintercept = input_max, col = 'red') + 
        annotate('text', label = 'max', 
                 y = annot_y, 
                 x = input_max, 
                 col = 'red', angle = 90) +
      geom_vline(xintercept = input_mean, col = 'green') + 
        annotate('text', label = 'mean', 
                 y = annot_y, 
                 x = input_mean, 
                 col = 'green', angle = 90)
    
    return(result_plot)
  }

# fun_hist_label_log10(rw$fixed.acidity, 'fixed.acidity')


# Create a function to generate generic boxplots, with labels, with log10 scale
fun_box_label_log10 <- function(input_raw, input_raw_name) 
  {
    input <- log10(input_raw)
    input_name <- paste('log10(',input_raw_name,')')
  
    result_box <- 
      ggplot(data = rw,
             aes(x = 1, y = input)) +
      geom_boxplot(col = 'red', alpha = 0.2) +
      geom_jitter(alpha = 0.1) +
      labs(y = input_name)
    
    return(result_box)
}
# fun_box_label_log10(rw$fixed.acidity, 'fixed.acidity')


# Create a function to generate generic histograms and boxplots, with labels, with log10 scale
fun_hist_box_label_log10 <- function(input, input_name) 
  {
    return(grid.arrange(fun_box_label_log10(input, input_name),
                        fun_hist_label_log10(input, input_name), ncol = 2)
           )
}

# fun_hist_box_label_log10(rw$fixed.acidity, 'fixed.acidity')


# Create a function to generate generic scatter and boxplots, with labels
fun_box_scatter_label <- function(input, input_name) 
  {
  result_plot <- 
    ggplot(data = rw, 
           aes(x = factor(quality), 
               y = input)) +
      geom_jitter( alpha = 0.3)  +
      geom_boxplot( alpha = 0.5,color = 'red')+
      geom_smooth(aes(quality-2, input, alpha = 0.5), 
                  method = "lm", 
                  se = FALSE, 
                  size = 1.2,
                  alpha = 0.5, 
                  color = 'blue',
                  linetype = 'dashed') +
      stat_summary(aes(y = input, group = 1),
                   fun.y = "median",
                   geom = "line",
                   color = "orange",
                   # shape = 8,
                   size = 1.2,
                   alpha = 0.8) +
      stat_summary(aes(y = input, group = 1),
                   fun.y = "mean",
                   geom = "line",
                   color = "green",
                   # shape = 8,
                   size = 1.2,
                   alpha = 0.8) +
      ylab(input_name)

  return(result_plot)
  
  }

# fun_box_scatter_label(rw$volatile.acidity, 'volatile.acidity')


# Create a function to generate generic scatter and boxplots, with labels, with log10 scale
fun_box_scatter_label_log10 <- function(input_raw, input_raw_name) 
  {
  input <- log10(input_raw)
  input_name <- paste('log10(',input_raw_name,')')
  
  result_plot <- 
    ggplot(data = rw, 
           aes(x = factor(quality), 
               y = input)) +
      geom_jitter( alpha = 0.3)  +
      geom_boxplot( alpha = 0.5,color = 'red')+
      geom_smooth(aes(quality-2, input, alpha = 0.5), 
                  method = "lm", 
                  se = FALSE, 
                  size = 1.2,
                  alpha = 0.5, 
                  color = 'blue',
                  linetype = 'dashed') +
      stat_summary(aes(y = input, group = 1),
                   fun.y = "median",
                   geom = "line",
                   color = "orange",
                   # shape = 8,
                   size = 1.2,
                   alpha = 0.8) +
      stat_summary(aes(y = input, group = 1),
                   fun.y = "mean",
                   geom = "line",
                   color = "green",
                   # shape = 8,
                   size = 1.2,
                   alpha = 0.8) +
      ylab(input_name)

  return(result_plot)
  
  }

# fun_box_scatter_label_log10(rw$volatile.acidity, 'volatile.acidity')


# Create a function to generate generic scatter with diverging quality colors, with labels
fun_scatter_facet_label <- function(input, input_name) 
  {
  result_plot <- 
    ggplot(data = rw, 
           aes(x = 1, 
               y = input, 
               color = factor(quality))) +
      geom_jitter(alpha = 1)  +
      scale_color_brewer(type = 'div',
      guide = guide_legend(title = 'Quality', reverse = T,
      override.aes = list(alpha = 1, size = 2))) + 
      ylab(input_name)

  return(result_plot)
  
}

# fun_scatter_facet_label(rw$volatile.acidity, 'volatile.acidity')


# Create a function to generate generic scatter with diverging quality colors, with labels, with log10 scale
fun_scatter_facet_label_log10 <- function(input_raw, input_raw_name) 
  {
  input <- log10(input_raw)
  input_name <- paste('log10(',input_raw_name,')')
  
  result_plot <- 
    ggplot(data = rw, 
           aes(x = 1, 
               y = input, 
               color = factor(quality))) +
      geom_jitter(alpha = 1)  +
      scale_color_brewer(type = 'div',
      guide = 
        guide_legend(title = 'Quality', 
                      reverse = T,
                      override.aes = list(alpha = 1, size = 2))) + 
      ylab(input_name)

  return(result_plot)
  
}

# fun_scatter_facet_label_log10(rw$volatile.acidity, 'volatile.acidity')


# Create a function to generate generic scatter, box, scatter with diverging quality colors plots, with labels
fun_box_scatter_facet_label <- function(input, input_name) 
  {
  p1 <-  fun_box_scatter_label(input, input_name)
  p2 <-  fun_scatter_facet_label(input, input_name)
  
  result_plot <- plot_grid(p1, p2, 
                           align = 'h',
                           nrow = 1,
                           rel_heights = c(0.6, 0.4)
                           )
  
  return(result_plot)
           
}

# fun_box_scatter_facet_label(rw$volatile.acidity, 'volatile.acidity')


# Create a function to generate generic scatter, box, scatter with diverging quality colors plots, with labels, with log10 scale
fun_box_scatter_facet_label_log10 <- function(input, input_name) 
  {
  p1 <-  fun_box_scatter_label_log10(input, input_name)
  p2 <-  fun_scatter_facet_label_log10(input, input_name)
  
  result_plot <- plot_grid(p1, p2, 
                           align = 'h',
                           nrow = 1,
                           rel_heights = c(0.6, 0.4)
                           )
  
  return(result_plot)
           
}

# fun_box_scatter_facet_label_log10(rw$volatile.acidity, 'volatile.acidity')


# Create a function to generate generic multi variable scatter with diverging quality colors, with labels
fun_multi_scatter_facet_label <- function(input1, input_name1, 
                                          input2, input_name2) 
  {
  result_plot <- 
    ggplot(data = rw, 
           aes(x = input1, 
               y = input2, 
               color = factor(quality))) +
      geom_jitter(alpha = 0.5)  +
      scale_color_brewer(type = 'div',
                         # palette = 'Blues',
                         guide = guide_legend(title = 'Quality', reverse = T,
                                              override.aes = list(alpha = 1, size = 2))) + 
      geom_smooth(method = 'lm', size = 1, se = FALSE) +
      ylab(input_name2) +
      xlab(input_name1) +
      theme(panel.background = 
              element_rect(fill = 'grey69')) +
      theme(panel.grid.minor = 
              element_line(colour = 'white', size=0.5)) +
      theme(panel.grid.major = 
              element_line(colour = 'white', size=0.5))

  return(result_plot)
  
}

# fun_multi_scatter_facet_label(rw$volatile.acidity, 'volatile.acidity', 
#                               rw$alcohol, 'alcohol')
# fun_multi_scatter_facet_label(rw$alcohol, 'alcohol',
#                               rw$volatile.acidity, 'volatile.acidity')

```

A quick look at the histogram for each variable below, with summary statistics 
(
  min = 1st red line, 
  q1 = 1st blue line, 
  median = Orange line, 
  q3 = 2nd blue line, 
  max = 2nd red line, 
  mean = Green line
).
```{r}
h1 <- fun_hist(rw$fixed.acidity, 
               'fixed.acidity') 

h2 <- fun_hist(rw$volatile.acidity, 
               'volatile.acidity') 

h3 <- fun_hist(rw$citric.acid, 
               'citric.acid') 

h4 <- fun_hist(rw$residual.sugar, 
               'residual.sugar') 

h5 <- fun_hist(rw$chlorides, 
               'chlorides') 

h6 <- fun_hist(rw$free.sulfur.dioxide, 
               'free.sulfur.dioxide') 

h7 <- fun_hist(rw$total.sulfur.dioxide, 
               'total.sulfur.dioxide') 

h8 <- fun_hist(rw$density, 
               'density') 

h9 <- fun_hist(rw$pH, 
               'pH') 

h10 <- fun_hist(rw$sulphates, 
                'sulphates') 

h11 <- fun_hist(rw$alcohol, 
                'alcohol')

h12 <- fun_hist(rw$quality, 
                'quality')

grid.arrange(h1, h2, h3, 
             h4, h5, h6, 
             h7, h8, h9, 
             h10, h11, h12, 
             ncol = 3)
```

Let's look at each variable in turn.

##Fixed acidity
Fixed acidity is not normally distributed. It has a long tail as there are some large acidity figures, although with low frequency.

```{r}
fun_hist_box_label(rw$fixed.acidity, 
                   'fixed.acidity')
summary(rw$fixed.acidity)

fun_stdev(rw$fixed.acidity)
```

The 1og10 plot has a more normal distribution, as can be seen by smaller standard deviation.
```{r}
fun_hist_box_label_log10(rw$fixed.acidity, 
                         'fixed.acidity')

fun_stdev_log10(rw$fixed.acidity)
```


##Volatile acidity
Volatile acidity is not normally distributed. It has a long tail as there are some large acidity figures, although with low frequency. It is more normally distributed than fixed acidity.

```{r}
fun_hist_box_label(rw$volatile.acidity, 
                   'volatile.acidity')
summary(rw$volatile.acidity)

fun_stdev(rw$volatile.acidity)
```

The 1og10 plot has a slightly more normal distribution, as can be seen by smaller standard deviation.
```{r}
fun_hist_box_label_log10(rw$volatile.acidity, 
                         'volatile.acidity')

fun_stdev_log10(rw$volatile.acidity)
```


##Citric acid
Citric acid has a long tail distribution as with the previous 2 variables. It is not normally distrbuted and the distribution is stretched by large acid levels for several red wines. 

```{r}
fun_hist_box_label(rw$citric.acid, 
                   'citric.acid')
summary(rw$citric.acid)

fun_stdev(rw$citric.acid)
```

Attempts to normalise the distribution of citric acid resulted in a reversal of the direction of long-tail distribution from left skewed (bulk of observation on the low end) to right skewed (bulk of observation on the high end).

```{r}
fun_hist_box_label_log10(rw$citric.acid, 
                         'citric.acid')

fun_stdev_log10(rw$citric.acid)

```


##Residual sugar
Residual sugar has a bulk of observations in the lower end, with a lot of outliers. This results in a large right skew, long tailed distribution.
We will attempt to normalise this using log10 plot.
```{r}
fun_hist_box_label(rw$residual.sugar, 
                   'residual.sugar')
summary(rw$residual.sugar)

fun_stdev(rw$residual.sugar)
```

The log10 plot is more normally distributed, however it is still quite right skewed.
```{r}
fun_hist_box_label_log10(rw$residual.sugar, 
                         'residual.sugar')

fun_stdev_log10(rw$residual.sugar)
```


##Chlorides
Similar to residual sugar, chlorides has a bulk of observations in the lower end, with a lot of outliers. This results in a large right skew, long tailed distribution.
We will attempt to normalise this using log10 plot.

```{r}
fun_hist_box_label(rw$chlorides, 
                   'chlorides')
summary(rw$chlorides)

fun_stdev(rw$chlorides)
```

The log10 plot is more normally distributed, but still has a rather right skew.

```{r}
fun_hist_box_label_log10(rw$chlorides, 
                         'chlorides')

fun_stdev_log10(rw$chlorides)
```


##Free sulfur dioxide
Free sulfur dioxide is quite similar in distribution to chlorides and residual sugar, although it is less extreme.
It is right skewed, has sizeable outliers and is not normally distributed.

```{r}
fun_hist_box_label(rw$free.sulfur.dioxide, 
                   'free.sulfur.dioxide')
summary(rw$free.sulfur.dioxide)

fun_stdev(rw$free.sulfur.dioxide)
```

Attempts to normalise using log10 plot has worked, with a more even distribution of observations and greatly reduced outlier count.

```{r}
fun_hist_box_label_log10(rw$free.sulfur.dioxide, 
                         'free.sulfur.dioxide')

fun_stdev_log10(rw$free.sulfur.dioxide)
```


##Total sulfur dioxide
The distribution of total sulfur dioxide is very similar to free sulfur dioxide.

```{r}
fun_hist_box_label(rw$total.sulfur.dioxide, 
                   'total.sulfur.dioxide')
summary(rw$total.sulfur.dioxide)

fun_stdev(rw$total.sulfur.dioxide)
```

Using a log10 plot, we were able to normalise the distribution and greatly reduce the number of outliers, like in the previous variable.

```{r}
fun_hist_box_label_log10(rw$total.sulfur.dioxide, 
                         'total.sulfur.dioxide')

fun_stdev_log10(rw$total.sulfur.dioxide)
```


##Density
Density looks normally distributed, with some outliers. Most observations are concentrated between 0.995 and 1. Mean and medain are identical.

```{r}
fun_hist_box_label(rw$density, 
                   'density')
summary(rw$density)

fun_stdev(rw$density)
```

Using a log10 plot has made this variable even more normally distributed.
```{r}
fun_hist_box_label_log10(rw$density, 
                         'density')

fun_stdev_log10(rw$density)
```


##pH
Much like density, pH is very normally distributed. Mean and median are identical and pH of wines are mostly concentrated between 3 to 3.5, with some outliers. 
75% of wines have pH less than 3.4 (quite acidic). 
```{r}
fun_hist_box_label(rw$pH, 
                   'pH')
summary(rw$pH)

fun_stdev(rw$pH)
```

Using a log10 plot has made this variable even more normally distributed.
```{r}
fun_hist_box_label_log10(rw$pH, 
                         'pH')

fun_stdev_log10(rw$pH)
```


##Sulphates
Similar to total and free sulfur dioxide, sulphates is right skewed with a bulk of observations in the lower end of the sulphates scale. It is long tailed and not normally distributed, with substantial amount of large, infrequent outliers.
```{r}
fun_hist_box_label(rw$sulphates, 
                   'sulphates')
summary(rw$sulphates)

fun_stdev(rw$sulphates)
```

Using a log10 scale, the distribution of sulphates is more binomial, with a marked reduction in number of outliers. Mean and median are closer together.
```{r}
fun_hist_box_label_log10(rw$sulphates, 
                         'sulphates')

fun_stdev_log10(rw$sulphates)
```


##Alcohol
Alcohol is also right skewed, with a bulk of observations in the lower end of the alcohol scale.
75% of wines have less than 11% alcohol.
```{r}
fun_hist_box_label(rw$alcohol, 
                   'alcohol')
summary(rw$alcohol)

fun_stdev(rw$alcohol)
```

Using a log10 scale, the distribution of alcohol is more normally distributed, with minimal outliers remaining.
```{r}
fun_hist_box_label_log10(rw$alcohol, 
                         'alcohol')

fun_stdev_log10(rw$alcohol)
```


##Quality
Quality is quite normally distributed, with a slight left skew. 50% of all wines analysed have a score between 5 and 6, the 8 being the highest score. This means that this analysis will be based mostly off of average quality wines.
```{r}
fun_hist_box_label(rw$quality, 
                   'quality')
summary(rw$quality)

fun_stdev(rw$quality)
```

##Additional variable: Total acidity
Total acidity is calculated variable (Fixed acidity + Volatile acidity). As a combination of both fixed and volatile acidity, it has a similar distribution to them.
```{r}
rw$total.acidity <- rw$fixed.acidity + 
                      rw$volatile.acidity

fun_hist_box_label(rw$total.acidity, 
                   'total.acidity')
summary(rw$total.acidity)

fun_stdev(rw$total.acidity)
```

Using a log10 scale, the distribution of total acidity is more normally distributed, with less outliers remaining.
```{r}
fun_hist_box_label_log10(rw$total.acidity, 
                         'total.acidity')

fun_stdev_log10(rw$total.acidity)
```



# Univariate Analysis

### What is the structure of your dataset?
The dataset has 1599 different wines, with characteristics recorded across 13 variables. All the variables are numerical in nature. 

### What is/are the main feature(s) of interest in your dataset?
Quality is of greatest interest, and it forms the anchor for the upcoming analysis. My aim is to determine how the other variables drive the quality of wine up or down. 

### What other features in the dataset do you think will help support your investigation into your feature(s) of interest?
From past experience and through quick research, it seems features such as alcohol content, pH, residual sugar and acid content are factors that result in wines of distinct tastes and textures (which when combined and tasted, either elicits positive or negative responses). 

### Did you create any new variables from existing variables in the dataset?
A new variable called total acidity was created (Fixed acidity + Volatile acidity). This is done so that we have a general acidity variable to analyse. Citric acid is not included in this new variable. 

### Of the features you investigated, were there any unusual distributions? Did you perform any operations on the data to tidy, adjust, or change the form of the data? If so, why did you do this?
Based on the distribution of quality, our analyses will be based mostly on wines with an average quality rating (between 5 and 6). This will make it difficult to determine what combination of levels within each variable makes very good, or very bad scoring wines.

Quite a few of the variables were not normally distributed, contrary to expectations of more normal distributions. There were extreme outliers for some of these variables. In most cases where the distribtuion is long tailed, the log10 scale adjustment is used to make the observations of variables more normally distrbuted.


# Bivariate Plots Section

Firstly, we look at correlation between the variables. Quality has relatively strong correlation with alcohol (positive correlation of 0.476) and volatile acidity (negative correlation of -0.391). 

Quality has a slight but notable correlation with citric acid (positive correlation of 0.226) and sulphates (positive correlation of 0.251).

Initial expectations that pH and residual sugar would impact quality will need to be revisited as both have very low correlation with quality.

```{r echo=FALSE, Bivariate_Plots}
new_var_seq <- c("X", "fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar", "chlorides", "free.sulfur.dioxide", "total.sulfur.dioxide", "density", "pH", "sulphates", "alcohol", "total.acidity", "quality")

rw <- rw[new_var_seq]

ggcorr(rw[, 2:14], 
       nbreaks = 4, 
       label_round = 3, 
       palette = "RdBu", 
       label = TRUE, 
       label_size = 3, 
       label_color = "white", 
       size = 2.5,
       legend.size = 15)
```

We will look at the interaction between quality of wines and each of the potential predictor variables below.

The following applies to the plots below:
 - Median: Orange line
 - Mean: Green line
 - Linear smoothing: Dashed blue line


##Quality & Fixed acidity
Fixed acidity and quality are slightly positive correlated (cor = 0.124). As mean and median of fixed acidity increases, the quality of wines increases.

The log10 plots show the same pattern albeit with less outliers.
```{r}
fun_box_scatter_facet_label(rw$fixed.acidity, 
                            'fixed.acidity')
fun_box_scatter_facet_label_log10(rw$fixed.acidity, 
                                  'fixed.acidity')
cor.test(rw$fixed.acidity, 
         rw$quality)

fun_summary_vs_quality(rw$fixed.acidity)
```


##Quality & Volatile acidity
Volatile acidity and quality are negatively correlated (cor = -0.391). Wines of better quality have a markedly lower mean and median volatile acidity content.

The relationship between volatile acidity and pH is discussed in a later section.

The log10 plots show the same pattern.

```{r}
fun_box_scatter_facet_label(rw$volatile.acidity, 
                            'volatile.acidity')
fun_box_scatter_facet_label_log10(rw$volatile.acidity, 
                                  'volatile.acidity')
cor.test(rw$volatile.acidity, 
         rw$quality)

fun_summary_vs_quality(rw$volatile.acidity)
```


##Quality & Citric acid
Citric acid and quality are positively correlated (cor = 0.226). Wines of better quality have a markedly higher mean and median citric acid content.

The relationship between citric acid and pH is discussed in a later section.

The log10 plots show the same pattern.

```{r}
fun_box_scatter_facet_label(rw$citric.acid, 
                            'citric.acid')

fun_box_scatter_facet_label_log10(rw$citric.acid, 
                                  'citric.acid')

cor.test(rw$citric.acid, 
         rw$quality)

fun_summary_vs_quality(rw$citric.acid)
```


##Quality & Residual sugar
Residual sugar and quality have a very small, negligible positive correlation (cor = 0.0137). Wines generally have residual sugar content of 0 to 3.1. This variable has minimal impact on quality of wine.

However, we should consider the interation between density and residual sugar (covered in Quality & Density section), which suggests it may have an indirect impact on quality of wine.

The log10 plots show the same pattern.

```{r}
fun_box_scatter_facet_label(rw$residual.sugar, 
                            'residual.sugar')
fun_box_scatter_facet_label_log10(rw$residual.sugar, 
                                  'residual.sugar')
cor.test(rw$residual.sugar, 
         rw$quality)

fun_summary_vs_quality(rw$residual.sugar)
```


##Quality & Chlorides
Chlorides and quality are negatively correlated (cor = -0.129). Wines of better quality have a lower mean and median chloride content. A bulk of the wine generally has chloride content between 0 and 0.1.

The log10 plots show the same pattern.

```{r}
fun_box_scatter_facet_label(rw$chlorides, 
                            'chlorides')
fun_box_scatter_facet_label_log10(rw$chlorides, 
                                  'chlorides')
cor.test(rw$chlorides, 
         rw$quality)

fun_summary_vs_quality(rw$chlorides)
```


##Quality & Free sulfur dioxide
Free sulfur dioxide and quality have a very small, negligible negative correlation (cor = -0.0507). This variable has minimal impact on quality of wine. Interestingly, the lower quality wines (with scores of 3, 4) and the higher quality wines (with scores of 7, 8) have low levels of free sulfur dioxide. Free sulfur dioxide prevents microbial growth and wine oxidation and is added to wine to improve its aging potential. 

The log10 plots show the same pattern with less outliers.

```{r}
fun_box_scatter_facet_label(rw$free.sulfur.dioxide, 
                            'free.sulfur.dioxide')
fun_box_scatter_facet_label_log10(rw$free.sulfur.dioxide, 
                                  'free.sulfur.dioxide')
cor.test(rw$free.sulfur.dioxide, 
         rw$quality)

fun_summary_vs_quality(rw$free.sulfur.dioxide)
```


##Quality & Total sulfur dioxide
Similar to free sulfur dioxide, total sulfur dioxide and quality have a small negative correlation (cor = -0.185). This variable has a small impact on quality of wine. Interestingly, the lower quality wines (with scores of 3, 4) and the higher quality wines (with scores of 7, 8) have low levels of total sulfur dioxide. 

The log10 plots show the same pattern with less outliers.

```{r}
fun_box_scatter_facet_label(rw$total.sulfur.dioxide, 
                            'total.sulfur.dioxide')
fun_box_scatter_facet_label_log10(rw$total.sulfur.dioxide, 
                                  'total.sulfur.dioxide')
cor.test(rw$total.sulfur.dioxide, 
         rw$quality)

fun_summary_vs_quality(rw$total.sulfur.dioxide)
```


##Quality & Density
Density and quality are negatively correlated (cor = -0.175). Density has a negative correlation with alcohol content (cor = -0.496) and a positive correlation with residual sugar content (cor = 0.355). 

Wines of better quality have a lower mean and median density level. This implies that higher quality wine has higher alcohol content and lower residual sugar content.

The log10 plots show the same pattern.

```{r}
fun_box_scatter_facet_label(rw$density, 
                            'density')
fun_box_scatter_facet_label_log10(rw$density, 
                                  'density')
cor.test(rw$density, 
         rw$quality)

fun_summary_vs_quality(rw$density)
```


##Quality & pH
pH and quality have a very small negative correlation (cor = -0.0577). Wines generally have pH levels of 3.2 to 3.5. This variable has small direct impact on quality of wine.

However, we should consider the interation between pH and citric acid and volatile acid, which suggests it may have an indirect impact on quality of wine. pH has a negative correlation with citric acid (cor = -0.542) and a positive correlation with volatile acidity (cor = 0.235). As higher quality wine has lower volatile acidity, this implies that it has lower pH. Higher quality wine also has higher citric acid content, implying that it has lower pH.   

The log10 plots show the same pattern.

```{r}
fun_box_scatter_facet_label(rw$pH, 'pH')
fun_box_scatter_facet_label_log10(rw$pH, 'pH')
cor.test(rw$pH, rw$quality)

fun_summary_vs_quality(rw$pH)
```


##Quality & Sulphates
Sulphates and quality are positively correlated (cor = 0.251). 

Sulphates contribute to sulfur dioxide gas that prevents microbial growth and wine oxidation and is added to wine to improve its aging potential. Given this, I expected a high correlation between sulphatesand both total and free sulfur dioxide levels, however the correlation in both cases are negligible.

Wines of better quality have a markedly higher mean and median sulphate content. This enables the wines to age better, and thus taste better.

The log10 plots show the same pattern.

```{r}
fun_box_scatter_facet_label(rw$sulphates, 
                            'sulphates')
fun_box_scatter_facet_label_log10(rw$sulphates, 
                                  'sulphates')
cor.test(rw$sulphates, 
         rw$quality)

fun_summary_vs_quality(rw$sulphates)
```


##Quality & Alcohol
Alcohol content and quality are positively correlated (cor = 0.476). Wines of better quality have a markedly higher mean and median alcohol content. Most of the wines with quality score of 7+ have alcohol content of 10.8% and above.

Higher alcohol content also means lower density (ie. lighter feel) to the wine given both variables are negatively correlated (cor = -0.496).

The log10 plots show the same pattern.

```{r}
fun_box_scatter_facet_label(rw$alcohol, 
                            'alcohol')
fun_box_scatter_facet_label_log10(rw$alcohol, 
                                  'alcohol')
cor.test(rw$alcohol, 
         rw$quality)

fun_summary_vs_quality(rw$alcohol)
```


##Quality & Total acidity
Total acidity and quality have a very small positive correlation (cor = 0.0857). This variable has minimal impact on quality of wine.

The log10 plots show the same pattern, with less outliers.

```{r}
fun_box_scatter_facet_label(rw$total.acidity, 
                            'total.acidity')
fun_box_scatter_facet_label_log10(rw$total.acidity, 
                                  'total.acidity')
cor.test(rw$total.acidity, 
         rw$quality)

fun_summary_vs_quality(rw$total.acidity)
```

## Relevant variables
The following variables are deemed relevant and could be useful in predicting the quality of wines.
```{r}
new_var_seq_relevant <- c("X", "residual.sugar", 
                          "pH", "chlorides", 
                          "density", "citric.acid",  
                          "sulphates", "volatile.acidity", 
                          "alcohol", "quality")

rw <- rw[new_var_seq_relevant]

ggcorr(rw[, 2:10], 
       nbreaks = 4, 
       label_round = 3, 
       palette = "RdBu", 
       label = TRUE, 
       label_size = 3, 
       label_color = "white", 
       size = 2.5,
       legend.size = 15)
```


# Bivariate Analysis

### Talk about some of the relationships you observed in this part of the investigation. How did the feature(s) of interest vary with other features in the dataset?
I found that alcohol, volatile acidity have sufficient relationship with quality. On a slightly lower extent, sulphates, citric acid, density and chlorides are also related to quality. All the listed variables will be useful as predictors of wine quality in a predictive model. 

These features were chosen as they had a sufficient correlation with wine quality.

I am keen to explore the relationship between residual sugar and density, and also the relationship between pH and citric acid, and how it could impact quality too.

### Did you observe any interesting relationships between the other features (not the main feature(s) of interest)?
Interestingly, although volatile acidity and citric acid are flagged as useful predictors, fixed and total acidity do not share the same relationships with wine quality.

Sulphates is useful as a wine quality predictor, but the same is not true for free and total sulfur dioxide.


### What was the strongest relationship you found?
The strongest relationship was between alcohol levels and wine quality with a a correlation of 0.476.



# Multivariate Plots Section
The approach I will take here is to firstly to focus on variables with highest absolute correlation to quality.

For each of the focus variables, I will then pair it with the variables (non-quality) with an absolute correlation of 0.20

- Pair 1: Alcohol - Volatile acidity, Density, Chlorides, pH 
- Pair 2: Volatile acidity - Sulphates, Citric acid, pH
- Pair 3: Sulphates - Citric acid, Chlorides
- Pair 4: Citric acid - Density, Chlorides, pH
- Pair 5: Density - Chlorides, pH, Residual sugar
- Pair 6: Chlorides - pH


## Pair 1: Alcohol - Volatile acidity, Density, Chlorides, pH
Alcohol has the strongest correlation with wine quality (cor = 0.476). 

Alcohol has a correlation of -0.202 with volatile acidity. Higher wine quality has higher alcohol content and lower volatile acidity.

Differing wine qualities have a relatively clear distinction in volatile acidity levels. 

Action: Add volatile acidity as predictor.

```{r echo=FALSE, Multivariate_Plots1}
fun_multi_scatter_facet_label(rw$alcohol, 
                              'alcohol', 
                              rw$volatile.acidity, 
                              'volatile.acidity')
```

Alcohol has a correlation of -0.496 with density. Higher wine quality has higher alcohol content and lower density. 

Differing wine qualities have a relatively clear distinction in density levels. 

Action: Add density as predictor.

```{r echo=FALSE, Multivariate_Plots2}
fun_multi_scatter_facet_label(rw$alcohol, 
                              'alcohol', 
                              rw$density, 
                              'density')
```

Alcohol has a correlation of -0.221 with chlorides. Higher wine quality has higher alcohol content and lower chlorides. 

Differing wine qualities have a relatively clear distinction in chlorides levels. 

Action: Add chlorides as predictor.

```{r echo=FALSE, Multivariate_Plots3}
fun_multi_scatter_facet_label(rw$alcohol, 
                              'alcohol', 
                              rw$chlorides, 
                              'chlorides')
```

The plot is recreated with chlorides under a log10 scale. The patterns are more pronounced.

```{r echo=FALSE, Multivariate_Plots3.1}
fun_multi_scatter_facet_label(rw$alcohol, 
                              'alcohol', 
                              log10(rw$chlorides), 
                              'log10(chlorides)')
```

Alcohol has a correlation of 0.206 with pH. Higher wine quality has higher alcohol content with relatively lower in pH. pH levels tend to increase as alcohol levels increase though.

Differing wine qualities have a relatively clear distinction in pH levels. 

Action: Add pH as predictor.

```{r echo=FALSE, Multivariate_Plots4}
fun_multi_scatter_facet_label(rw$alcohol, 
                              'alcohol', 
                              rw$pH, 
                              'pH')
```


## Pair 2: Volatile acidity - Sulphates, Citric acid, pH
Volatile acidity has a sufficient correlation with wine quality (cor = -0.391). 

Volatile acidity has a correlation of -0.261 with sulphates. Higher wine quality has lower volatile acidity and higher sulphates.

Differing wine qualities have a relatively clear distinction in sulphates levels. 

Action: Add sulphates as predictor.

```{r echo=FALSE, Multivariate_Plots5}
fun_multi_scatter_facet_label(rw$volatile.acidity, 
                              'volatile.acidity', 
                              rw$sulphates, 
                              'sulphates')
```

The plot is recreated with sulphates under a log10 scale. The patterns are more pronounced.

```{r echo=FALSE, Multivariate_Plots5.1}
fun_multi_scatter_facet_label(rw$volatile.acidity, 
                              'volatile.acidity', 
                              log10(rw$sulphates), 
                              'log10(sulphates)')
```

Volatile acidity has a correlation of -0.552 with citric acid. Higher wine quality has lower volatile acidity and slightly higher citric acid.

Differing wine qualities have a relatively clear distinction in citric acid levels. 

Action: Add citric acid as predictor.

```{r echo=FALSE, Multivariate_Plots6}
fun_multi_scatter_facet_label(rw$volatile.acidity, 
                              'volatile.acidity', 
                              rw$citric.acid, 
                              'citric.acid')
```

Volatile acidity has a correlation of 0.235 with pH. Higher wine quality has lower volatile acidity and sligtly lower pH levels.

Differing wine qualities have a relatively clear distinction in pH levels. 

Action: Add pH as predictor.

```{r echo=FALSE, Multivariate_Plots7}
fun_multi_scatter_facet_label(rw$volatile.acidity, 
                              'volatile.acidity', 
                              rw$pH, 
                              'pH')
```



## Pair 3: Sulphates - Citric acid, Chlorides
Sulphates has a sufficient correlation with wine quality (cor = 0.251). 

Sulphates has a correlation of 0.313 with citric acid. Higher wine quality has higher sulphates and any slightly higher citric acid levels.

Differing wine qualities have a relatively clear distinction in citric acid levels. 

Action: Add citric acid as predictor.


```{r echo=FALSE, Multivariate_Plots8}
fun_multi_scatter_facet_label(rw$sulphates, 
                              'sulphates', 
                              rw$citric.acid, 
                              'citric.acid')
```

Sulphates has a correlation of 0.371 with chlorides. Higher wine quality has higher sulphates and lower chlorides levels.

Differing wine qualities have a relatively clear distinction in chlorides levels. 

Action: Add chlorides as predictor.

```{r echo=FALSE, Multivariate_Plots9}
fun_multi_scatter_facet_label(rw$sulphates, 
                              'sulphates', 
                              rw$chlorides, 
                              'chlorides')
```


## Pair 4: Citric acid - Density, Chlorides, pH
Citric acid has a sufficient correlation with wine quality (cor = 0.226). 

Citric acid has a correlation of 0.365 with density. Higher wine quality has slightly higher citric acid levels and have lower density.

Differing wine qualities have a relatively clear distinction in density levels. 

Action: Add density as predictor.


```{r echo=FALSE, Multivariate_Plots10}
fun_multi_scatter_facet_label(rw$citric.acid, 
                              'citric.acid', 
                              rw$density, 
                              'density')
```

Citric acid has a correlation of 0.204 with chlorides. Higher wine quality is not really impacted by citric acid levels and have lower chlorides.

Differing wine qualities have a relatively clear distinction in chlorides levels. 

Action: Add chlorides as predictor.

```{r echo=FALSE, Multivariate_Plots11}
fun_multi_scatter_facet_label(rw$citric.acid, 
                              'citric.acid', 
                              rw$chlorides, 
                              'chlorides')
```

Citric acid has a correlation of -0.542 with pH. Higher wine quality is not really impacted by citric acid levels and has slightly lower pH levels.

Differing wine qualities have a relatively clear distinction in pH levels. 

Action: Add pH as predictor.

```{r echo=FALSE, Multivariate_Plots12}
fun_multi_scatter_facet_label(rw$citric.acid, 
                              'citric.acid', 
                              rw$pH, 
                              'pH')
```



## Pair 5: Density - Chlorides, pH, Residual sugar
Density has a sufficient correlation with wine quality (cor = -0.175).

Density has a correlation of 0.201 with chlorides. Higher wine quality has lower density and lower chlorides.

Differing wine qualities have a relatively clear distinction in chlorides levels. 

Action: Add chlorides as predictor.


```{r echo=FALSE, Multivariate_Plots13}
fun_multi_scatter_facet_label(rw$density, 
                              'density', 
                              rw$chlorides, 
                              'chlorides')
```

Density has a correlation of -0.342 with pH. Higher wine quality has lower density and slightly lower pH levels.

Differing wine qualities have a relatively clear distinction in pH levels. 

Action: Add pH as predictor.

```{r echo=FALSE, Multivariate_Plots14}
fun_multi_scatter_facet_label(rw$density, 
                              'density', 
                              rw$pH, 
                              'pH')
```

Density has a correlation of 0.355 with residual sugar. Higher wine quality has lower density and seems to have slightly higher residual sugar content.

Differing wine qualities do not have a relatively clear distinction in residual sugar levels. 

Action: Remove residual sugar as predictor.

```{r echo=FALSE, Multivariate_Plots15}
fun_multi_scatter_facet_label(rw$density, 
                              'density', 
                              rw$residual.sugar, 
                              'residual.sugar')
```



## Pair 6: Chlorides - pH
Chlorides has a sufficient correlation with wine quality (cor = -0.129).

Chlorides has a correlation of -0.265 with pH. Higher wine quality has lower chlorides and has slightly lower pH levels.

Differing wine qualities have a relatively clear distinction in pH levels. 

Action: Add pH as predictor.


```{r echo=FALSE, Multivariate_Plots16}
fun_multi_scatter_facet_label(rw$chlorides, 
                              'chlorides', 
                              rw$pH, 
                              'pH')

```


##Prediction model
Based on the previous analyses, I have decided to use the following variables are predictors for wine quality:
 - Alcohol, Volatile acidity, Sulphates, Density, Chlorides, pH, Citric acid

The following variable have been excluded as predictors:
 - Residual sugar

```{r}
m1 <- lm(data = rw, formula = I(quality) ~ I(alcohol))
m2 <- update(m1, ~ . + volatile.acidity)
m3 <- update(m2, ~ . + sulphates)
m4 <- update(m3, ~ . + density)
m5 <- update(m4, ~ . + chlorides)

m6 <- update(m5, ~ . + pH)

m7 <- update(m6, ~ . + citric.acid)
# ?mtable

mtable(m1,m2,m3,m4,m5,m6,m7,  sdigits = 3)
```

The original model has an R-squared value of 0.35, meaning that it can explain 35% of variability in wine quality.

We can increase this as follows:
- From the original model, it seems that Density does not add to the predictive power of the model. Hence, it is removed from this revised model.
- Additionally, we will use log10 scales for sulphates and chlorides as it will improve the normality of the distribution of the observations, reduce outliers and improve its predictive power.

The revised model has an improved R-squared value of 0.36.
```{r}
m1 <- lm(data = rw, formula = I(quality) ~ I(alcohol))
m2 <- update(m1, ~ . + volatile.acidity)
m3 <- update(m2, ~ . + log10(sulphates))
m4 <- update(m3, ~ . + citric.acid)
m5 <- update(m4, ~ . + log10(chlorides))

m6 <- update(m5, ~ . + pH)

mtable(m1,m2,m3,m4,m5,m6,  sdigits = 3)
```



# Multivariate Analysis

### Talk about some of the relationships you observed in this part of the investigation. Were there features that strengthened each other in terms of looking at your feature(s) of interest?
The strongest relationships were between wine quality and the 2 strongest correlated variables, alcohol and volatile acidity. It clearly shows that quality of wine increases with higher alcohol levels and lower volatile acidity.

Other variable combinations that strengthened each other were as follows: (alcohol and density), (alcohol and chlorides), (sulphates and volatile acidity), (density and citric acid). This is judged by the level of distinction between the different quality scores each combination of variables can predict.


### Were there any interesting or surprising interactions between features?
Alcohol and density was an interesting interaction. I was surprised that density decreased when alcohol levels increased. Incidentally, higher alcohol levels and lower density leads to better scoring wines.

This roughly proves that drinkers like a light textured, and stronger drink!

However, based on our linear model, it seems that density does not have any predictive qualities when alcohol is added a predictor. 


### OPTIONAL: Did you create any models with your dataset? Discuss the strengths and limitations of your model.
A linear model was created to predict wine quality. The revised model uses the following variables as predictors: Alcohol, Volatile acidity, log10(Sulphates), log10(Chlorides), pH, Citric acid. 

It is able to explain 36% of variability in wine quality, which is not very high. This is likely caused by the relatively low correlations between predictors and wine quality.


------

# Final Plots and Summary

### Plot One
```{r echo=FALSE, Plot_One}
fun_hist_label_clean <- function(input, input_name) 
  {
    
    result_plot <-
      ggplot(data = rw, 
             aes(x = input)) +
      geom_bar() +
      labs(title = 'Histogram of Wine quality',
           x = input_name,
           y = 'Wine count') +
      scale_x_continuous(breaks = seq(3, 8, 1))
    
    return(result_plot)
}

p1.1 <- fun_hist_label_clean(rw$quality, 'Wine quality')


fun_box_label_clean <- function(input, input_name) 
  {
    result_box <- 
      ggplot(data = rw,
             aes(x = 1, y = input)) +
      geom_boxplot(col = 'red', alpha = 0.2) +
      geom_jitter(alpha = 0.1) +
      labs(title = 'Box, scatter plot of Wine quality',
           y = input_name,
           x = '') +
      theme(axis.text.y = element_blank()) +
      scale_y_continuous(breaks = seq(3, 8, 1)) +
      coord_flip()
    
    return(result_box)
}

p1.2 <- fun_box_label_clean(rw$quality, 'Wine quality')

grid.arrange(p1.1, p1.2,
             nrow = 2)

```

### Description One
This first plot illustrates that a majority of the of the wines in the data sets have a quality score of 5 and 6 (ie. average quality wine). The first and thrid quartiles of data fall within the scores of 5 and 6, meaning at least 50% of wines in the data are from this quality groups. 

This improves our understanding of what levels of each variables make  average quality wine. 

If the data set is more even and has a much higher representation of very low (scores 3 to 4) and very high (scores 7 to 8) quality wines, we will be able to learn more about what levels of each variables makes very good or very bad wines.


### Plot Two
```{r echo=FALSE, Plot_Two}

fun_box_scatter_label_clean <- function(input, input_name) 
  {
  result_plot <- 
    ggplot(data = rw, 
           aes(x = factor(quality), 
               y = input)) +
      geom_jitter( alpha = 0.3)  +
      geom_boxplot( alpha = 0.8,color = 'orange')+
      geom_smooth(aes(quality-2, input, 
                      alpha = 0.5, group = 1, 
                      col = 'Linear fit'), 
                  method = "lm", 
                  se = FALSE, 
                  size = 1.2,
                  alpha = 0.5, 
                  # color = 'blue',
                  linetype = 'dashed') +
      stat_summary(aes(y = input, group = 1, 
                       col = 'Median'),
                   fun.y = "median",
                   geom = "line",
                   # color = "orange",
                   # shape = 8,
                   size = 1.2,
                   alpha = 0.8) +
      stat_summary(aes(y = input, group = 1, 
                       col = 'Mean'),
                   fun.y = "mean",
                   geom = "line",
                   # color = "green",
                   # shape = 8,
                   size = 1.2,
                   alpha = 0.8) +
      labs(title = 'Quality by Alcohol levels',
           x = 'Wine quality',
           y = input_name)+
      guides(col = guide_legend(title = 'Summary type', 
                                title.theme = element_text(
                                  size = 12,
                                  face = 'bold'
                                )))

  return(result_plot)
  
  }
p2.1 <- fun_box_scatter_label_clean(rw$alcohol, 
                                    'Alcohol (% by volume)')

p2.1
```

### Description Two
The second plot shows the relationship between wine quality and its strongest correlated variables, alcohol level. 

There is a positive correlation where the higher the alcohol levels, the higher the quality of the wine. There is a clear increase in mean and median of alcohol levels as the quality of wine increases.


### Plot Three
```{r echo=FALSE, Plot_Three}

fun_multi_scatter_facet_label_clean <- function(input1, input_name1, 
                                                input2, input_name2) 
  {
  result_plot <- 
    ggplot(data = rw, 
           aes(x = input1, 
               y = input2, 
               color = factor(quality))) +
      geom_jitter(alpha = 0.6)  +
      scale_color_brewer(type = 'div',
                         # palette = 'Blues',
                         guide = guide_legend(title = 'Quality', 
                                              reverse = T,
                                              override.aes = 
                                                list(alpha = 1, size = 2))) + 
      geom_smooth(method = 'lm', 
                  size = 1, se = FALSE) +
      labs(title = 'Quality by Alcohol and Sodium chloride levels',
           y = input_name2,
           x = input_name1) +
      theme(
        panel.background = element_rect(fill = 'grey69')) +
      theme(
        panel.grid.minor = element_line(colour = 'white', size=0.5)) +
      theme(
        panel.grid.major = element_line(colour = 'white', size=0.5)) +
      scale_y_continuous(limits = c(0, 
                                    quantile(rw$chlorides, 0.85)))

  return(result_plot)
  
}

p3.1 <- fun_multi_scatter_facet_label_clean(rw$alcohol, 
                                            'Alcohol (% by volume)', 
                                            rw$chlorides, 
                                            'Sodium chloride (g/dm^3)')
p3.1

```

### Description Three
The third plot shows the combined relationship between alcohol and chloride levels and quality. The combination of these variables are important as it has a clear relationship with wine quality. The lower the chloride levels and the higher the alcohol content, the higher the quality of wine.

This also implies that drinkers tend to prefer stronger and less salty wines!

The combination of both these variables have good predictive qualities for determining wine quality.



------

# Reflection
I ran into difficulty trying to decide the best way to visualise the data. I explored the many different plot options and struggled to choose a single plot as I thought each of the plots helped improve understanding of the data set. I finally decided to use multiple plots to visualise a single variable. Box, scatter and line plots used in combination has been really helpful in this analysis.

I also ran into difficulty with the distribution of some of the variables (such as sulphates and chlorides). They had really long tails and were skewed, and these tend to hide patterns/relationships of these variables. I eventually overcame this by using a log10 scale on both these variables.

I found success and was happy to find some key predictors of wine quality such as alcohol levels, volatile acidity, sulphates, chlorides and citric acid. They have a good level of correlation with quality and when used as predictors in a linear prediction model, it can explain 36% of variability in wine quality. The flipside of this is that an expanded dataset (as elaborated further below), could potentially uncover some stronger predictors an improve the prediction model.

Given the nature of the data set, where a majority of the wines analysed were of average quality, it reduces my confidence in the predictive power of any models built from this data set. It would be good to be able to expand this data set to include both very good and very bad wines.

It would be good to be able to expand on the data set from a quality rating perspective. Currently, we have a single quality score, which may be based on multiple criteria such as taste, texture, price to name a few. It would be good to brign those additional variables (and potentially useful predictors) into the data set. This will open the door for us to analyse things like what drives wine prices and also what combination of chemical properties result in different wine textures.

Overall, I feel that I know more about red wine and the different variables to pick up on when trying to determine what would be a good quality red wine.

------
# References

https://en.wikipedia.org/wiki/Red_wine

https://www.vinodiversity.com/sulphur-preservatives-in-wine.html

http://www.aromadictionary.com/articles/salt_article.html
