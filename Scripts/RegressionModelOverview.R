#Script RegressionModelOverview.r...Megan C. Ferguson...7 December 2021

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
        scale_color_hue(labels=c("mean=0, sd=1", 
                                 "mean=3, sd=0.6")) +
        theme(legend.title = element_blank(),
              legend.text = element_text(size=14),
              legend.position = "top")        
      ggsave(filename="Figures\\2_Normal_Distributions.png",
             device="png", width=7.5, height=6.5, units="in")
      
  #What is the binomial distribution?

    #Plot the probability density functions for three Bin distributions:
    #dens1: p=0.5, n=20
    #dens2: p=0.7, n=20
    #dens3: p=0.5, n=40
      
      x <- seq(from=0, to=40, by=1)
      
      dens1 <- dbinom(x,
                     size=20,
                     prob = 0.5)
      
      dens2 <- dbinom(x,
                     size=20,
                     prob = 0.7)
      
      dens3 <- dbinom(x,
                     size=40,
                     prob = 0.5)
      
      df.dens <- cbind.data.frame(x, dens1, dens2, dens3)
      
      ggplot(df.dens, aes(x)) +
        geom_point(aes(y = dens1, colour = "dens1")) + 
        geom_point(aes(y = dens2, colour = "dens2")) +
        geom_point(aes(y = dens3, colour = "dens3")) +
        xlab("Y") +
        ylab("p(Y = y)") + 
        scale_color_hue(labels=c("p=0.5, n=20", 
                                 "p=0.7, n=20",
                                 "p=0.5, n=40")) +
        theme(legend.title = element_blank(),
              legend.text = element_text(size=14),
              legend.position = "top")
      ggsave(filename="Figures\\3_Binomial_Distributions.png",
             device="png", width=7.5, height=6.5, units="in")
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      