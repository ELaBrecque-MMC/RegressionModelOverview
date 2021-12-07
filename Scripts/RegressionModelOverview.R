#Script RegressionModelOverview.r...Megan C. Ferguson...6 December 2021

  library(ggplot2)

  #What is the normal distribution?

    #Plot the probability density functions for two normal distributions:
    #dens1 has a mean=0 and standard deviation=1
    #dens2 has mean=3, sd=0.6
      x <- seq(from=-4, to=6, by=0.01)
      dens1 <- dnorm(x,
                     mean=0,
                     sd = 1)
      dens2 <- dnorm(x,
                     mean=3,
                     sd = 0.6)
      df.dens <- cbind.data.frame(x, dens1, dens2)
      ggplot(df.dens, aes(x)) +
        geom_line(aes(y = dens1, colour = "dens1")) + 
        geom_line(aes(y = dens2, colour = "dens2")) +
        xlab("Y") +
        ylab("PDF") + 
        theme(legend.position = "none")
      ggsave(filename="Figures\\2_Normal_Distributions.png",
             device="png", width=7.5, height=6.5, units="in")