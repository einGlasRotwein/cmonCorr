# cmonCorr

Create correlations of a specified size by reshuffling a vector to (not) match another one. Can be used for visualisation and comparison of correlations and exploration of their properties.

# Installation

```R
devtools::install_github("einGlasRotwein/cmonCorr")

# load package
library("cmonCorr")
```

# Examples

## sim_cor

Reshuffle a vector to create a correlation of specified size with another one.

### vector mode

Takes one or two vectors as input. If only one is provided, the second vector is a duplicate of the first one.

```R
vec1 <- 1:100
vec2 <- 51:150

example1 <- sim_cor(vector1 = vec1, vector2 = vec2, r = .2, shuffles = 100)

library(tidyverse)
example1$data %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = paste("desired correlation", example1$desired_correlation, " - ",
                     "actual correlation", round(example1$actual_correlation, 2))) +
  theme(plot.title = element_text(hjust = .5))
```

![](https://raw.githubusercontent.com/einGlasRotwein/cmonCorr/master/examples/example1.png)

```R
vec1 <- 1:200

example2 <- sim_cor(vector1 = vec1, r = .42, shuffles = 1000)

library(tidyverse)
example2$data %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = paste("desired correlation", example2$desired_correlation, " - ",
                     "actual correlation", round(example2$actual_correlation, 2))) +
  theme(plot.title = element_text(hjust = .5))
```

![](https://raw.githubusercontent.com/einGlasRotwein/cmonCorr/master/examples/example2.png)

### parameter mode

Takes parameters for one or two vectors as input. If only parameters for one are provided, the second vector is created from the parameters of the first one.

```R
example3 <- sim_cor(N = 50, MW1 = 40, SD1 = 4, MW2 = 40, SD2 = 10, r = .81, shuffles = 1000)

library(tidyverse)
example3$data %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = paste("desired correlation", example3$desired_correlation, " - ",
                     "actual correlation", round(example3$actual_correlation, 2))) +
  theme(plot.title = element_text(hjust = .5))
```

![](https://raw.githubusercontent.com/einGlasRotwein/cmonCorr/master/examples/example3.png)

```R
example4 <- sim_cor(N = 20, MW1 = 40, SD1 = 4, r = -1, shuffles = 1000)

library(tidyverse)
example4$data %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = paste("desired correlation", example4$desired_correlation, " - ",
                     "actual correlation", round(example4$actual_correlation, 2))) +
  theme(plot.title = element_text(hjust = .5))
```

![](https://raw.githubusercontent.com/einGlasRotwein/cmonCorr/master/examples/example4.png)
