# cmonCorr

Create correlations of a specified size by reshuffling a vector to (not) match another one. Can be used for visualisation and comparison of correlations and exploration of their properties. Based on linear correlations (pearson).

# Installation

```R
devtools::install_github("einGlasRotwein/cmonCorr")

library("cmonCorr")
```

# Examples

## sim_cor_vec

Reshuffles a vector to create a correlation of specified size with another one.

Takes one or two vectors as input. If only one is provided, the second vector is a duplicate of the first one.

```R
vec1 <- 1:100
vec2 <- 51:150

example1 <- sim_cor_vec(vector1 = vec1, vector2 = vec2, r = .2, shuffles = 100)

library(tidyverse)
example1$data %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(colour = "#0c2c76", alpha = .6) +
  geom_smooth(method = "lm", colour = "#ff4600") +
  labs(title = paste("desired correlation", example1$desired_correlation, " - ",
                     "actual correlation", round(example1$actual_correlation, 2))) +
  theme(plot.title = element_text(hjust = .5))
```

![](https://raw.githubusercontent.com/einGlasRotwein/cmonCorr/master/examples/example1.png)

```R
vec1 <- 1:200

example2 <- sim_cor_vec(vector1 = vec1, r = .42, shuffles = 1000)
```

![](https://raw.githubusercontent.com/einGlasRotwein/cmonCorr/master/examples/example2.png)

## sim_cor_vec

Reshuffles a vector to create a correlation of specified size with another one.

Takes parameters for one or two vectors as input. If only parameters for one are provided, the second vector is created from the parameters of the first one.

**WARNING:** Experimental danger zone, as there will not always be error messages when parameters in arglist are not provided correctly. When in doubt, generate vectors to be correlated outside the function and then use `sim_cor_vec`.

```R
example3 <- sim_cor_param(100, "normal", list(mean = 10, sd = 1), r = .52, shuffles = 1000)
```

![](https://raw.githubusercontent.com/einGlasRotwein/cmonCorr/master/examples/example3.png)

```R
example4 <- sim_cor_param(100, "normal", list(mean = 10, sd = 1), r = -.8, shuffles = 100,
                          "unif", list(min = 1, max = 2))
```

![](https://raw.githubusercontent.com/einGlasRotwein/cmonCorr/master/examples/example4.png)

## Simpson's Paradox

Uses the sim_cor functions to create a [Simpson's Paradox](https://en.wikipedia.org/wiki/Simpson%27s_paradox). There are two versions of functions: simpsons_means creates an overall correlation by targetting the means of subgroups. simpsons_paradox shifts the subgroups along the y-axis and takes a scaling parameter specifying the degree of the shift as well as a parameter determining the smalles y-coordinate of the data. If the initial correlation provided (`r_tot` in `simpsons_paradox`) or the correlation of subgroup means (`r_means` in `simpsons_means`) have a different sign than the correlation within the subgroups, a "full" Simpson's Paradox is created.

**NOTE:** This is a bit experimental and not every parameter combination will yield a satisfying result. The correlation between subgroup means is easy to specify, but overall correlation between x and y may substantially differ from the desired outcome. There are limits to the overall correlation when the correlation within subgroups is strong. Nevertheless, a result where the overall correlation differs from the one within subgroups should always be achievable.

### simpsons_mean
```R
example5 <- simpsons_paradox(r_tot = 1, r_sub = -.6, ngroups = 4, nsubgroups = 30)
```

The overall correlation looks like this ...

![](https://raw.githubusercontent.com/einGlasRotwein/cmonCorr/master/examples/examplesimps1.png)

But when you look at the subgroups, it's a different story.

![](https://raw.githubusercontent.com/einGlasRotwein/cmonCorr/master/examples/examplesimps2.png)

Note that the correlation between group means is still 0.995, while the overall correlation between x and y is only 0.325.

### simpsons_paradox

```R
example6 <- simpsons_paradox(r_tot = .4, r_sub = -.7, means_subgroups = c(0, 1, -1, 1),
                             nsubgroups = 100, scaling = 4, ymin = 10)
```
Overall correlation:

![](https://raw.githubusercontent.com/einGlasRotwein/cmonCorr/master/examples/examplesimps3.png)

Subgroup correlations

![](https://raw.githubusercontent.com/einGlasRotwein/cmonCorr/master/examples/examplesimps4.png)

Note that the correlation between group means is 0.728, while the overall correlation between x and y is only 0.257. Also note that, as specified via `ymin`, the smallest y-coordinate is 10.

---

Credit: Internal functions for input validation adapted from [prmisc](https://github.com/m-Py/prmisc).
