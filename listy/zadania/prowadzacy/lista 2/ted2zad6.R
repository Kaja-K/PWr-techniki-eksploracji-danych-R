library(ggplot2)
hist(mpg$hwy,
     xlab = "hwy",
     main = "Histogram of hwy",
     breaks = 5
) # set number of bins
boxplot(mpg$hwy,
        ylab = "hwy"
)
hist(mpg$hwy,
      xlab = "hwy",
      main = "Histogram of hwy",
      nclass=4
) # s

