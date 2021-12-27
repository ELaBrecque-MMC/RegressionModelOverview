#Script RegressionModelOverview_MCF_PostWorkshop.r...Megan C. Ferguson...27 December 2021

  library(ggplot2)

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
      
  #A binomial, link="logit" (i.e., logistic) GAM from the mgcv package manual,
  #mgcv.pdf, page 200
      
    library(mgcv)
      
    ## simulate binomial data...
      set.seed(0) #Set seed for random number generator to get the same
                  #random numbers every time
      
      n.samp <- 400 #Set sample size
      
      dat <- gamSim(1,n=n.samp,dist="binary",scale=.33) #Simulate data
      
      p <- binomial()$linkinv(dat$f) ## binomial p
        #Break down the command above: Type ?stats::family in the R console to 
        #view the R helpfile that explains this further.
          B <- binomial() #Create an object called B that is class "family",
                          #in this case, the binomial family
          
          #Investgate B, which is a list with multiple named elements
            class(B) #The class of B.
            names(B) #The names assigned to B.
            B$family #The family name, as a character.
            B$link   #The name of the link function, as a character.
            B$linkfun#The link function, as a function.
                     #The logit link function is defined as log(p/(1-p))
            B$linkinv#The inverse of the link function, as a function.
                     #The inverse logit is exp(x)/(1 + exp(x))
            
          #So, binomial()$linkinv(dat$f) applies the inverse logit function
          #to the values stored in dat$f:
            
            test <- exp(dat$f)/(1 + exp(dat$f)) #Compute "p" manually
            summary(test - p) #These are identical

      n <- sample(c(1,3),n.samp,replace=TRUE) ## binomial n
        #Break down the command above: sample() is a function that 
        #randomly samples from the elements in c(1,3), n.samp times, with
        #replacement. n.samp is defined as 400 above. So, n is just a 
        #vector with 400 elements that is composed of a random assignment
        #of 1s and 3s, representing total # of trials for use by rbinom
        #below.
      
      dat$y <- rbinom(n,n,p)
        #Break down the command above: rbinom() creates length(n) random 
        #numbers from the binomial distribution. The binomial distribution
        #is defined by two parameters: 1) the number of trials (called n
        #here); and 2) the probability of success on each trial (p). The 
        #result is a vector y, the randomly-generated number of successes. 
        #The ith element of y, y[i], is the result
        #of randomly drawing from the binomial distribution with number of
        #trials equal to n[i] and probability of success on a single trial
        #equal to p[i]. y[i] corresponds to the number of successes out of 
        #n[i] trials.
      
      dat$n <- n #Save the n created above to dataframe dat
      
      lr.fit <- gam(y/n~s(x0)+s(x1)+s(x2)+s(x3),family=binomial,
                    data=dat,weights=n,method="REML")
        #Here, y/n is the proportion of successes, computed as the number
        #of successful trials divided by the total number of trials.
      
        #Also, note that the default link function for the binomial family 
        #is the logit. (See ?stats::family). The call above is identical to the
        #following:
          test <- gam(y/n~s(x0)+s(x1)+s(x2)+s(x3),family=binomial(link="logit"),
                      data=dat,weights=n,method="REML")
          summary(lr.fit)
          summary(test) #Identical to summary(lr.fit)

    par(mfrow=c(2,2))
    
    #The following code shows how to evaluate the fit of the model to the 
    #simulated data
    
      ## normal QQ-plot of deviance residuals
      qqnorm(residuals(lr.fit),pch=19,cex=.3)
      
      ## Quick QQ-plot of deviance residuals
      qq.gam(lr.fit,pch=19,cex=.3)
      
      ## Simulation based QQ-plot with reference bands
      qq.gam(lr.fit,rep=100,level=.9)
      
      ## Simulation based QQ-plot, Pearson resids, all
      ## simulated reference plots shown...
      qq.gam(lr.fit,rep=100,level=1,type="pearson",pch=19,cex=.2)
    
    ## Now fit the wrong model and check....
      #This shows what the model evaluation plots look like when the wrong
      #model is used to fit the data.
      pif <- gam(y~s(x0)+s(x1)+s(x2)+s(x3),family=poisson,data=dat,method="REML")
      par(mfrow=c(2,2))
      qqnorm(residuals(pif),pch=19,cex=.3)
      qq.gam(pif,pch=19,cex=.3)
      qq.gam(pif,rep=100,level=.9)
      qq.gam(pif,rep=100,level=1,type="pearson",pch=19,cex=.2)      
      dev.off()
          
  #Input Crance's bearded seal data and create preliminary models
      
    #Input data
      seal.dat <- read.csv("Data\\Bearded overwinter data - R figuring_forMF_byPng.csv")
      summary(seal.dat)
      
      #Rename some fields, to simplify
        idx <- which(names(seal.dat)=="X.yes")
        names(seal.dat)[idx] <- "Yes"
        
        idx <- which(names(seal.dat)=="X.Pngs")
        names(seal.dat)[idx] <- "Pngs"
        
      #Make sure Mooring is a factor variable
        seal.dat$Mooring <- as.factor(seal.dat$Mooring)
        summary(seal.dat)
                
    #CalJulian gam; logit link
    
      CalJulian.logit <- gam(Yes/Pngs ~ s(CalJulian, bs="cc"), #cyclic cubic regression spline
                                          data = seal.dat,
                                          weights = Pngs,
                                          method="REML",  #Restricted maximum likelihood to fit model
                                          family=binomial(link="logit"))
      plot(CalJulian.logit)
      
    #ActualJulianDate (scaled) gam; logit link
      
      #Scale ActualJulianDate to minimize chances of running into parameter estimation
      #issues during model fitting
      
        seal.dat$AJD <- seal.dat$ActualJulianDate/10000
        head(seal.dat$AJD)
        summary(seal.dat$ActualJulianDate)
        summary(seal.dat$AJD)

        AJD.logit <- gam(Yes/Pngs ~ s(AJD, bs="ts"), #thin plate regression spline w/ shrinkage
                                      data = seal.dat,
                                      weights = Pngs,
                                      method="REML",  
                                      family=binomial(link="logit"))
        plot(AJD.logit)
                                            
    #Ice gam; logit link
    
      Ice.logit <- gam(Yes/Pngs ~ s(Ice, bs="ts"), 
                                          data = seal.dat,
                                          weights = Pngs,
                                          method="REML",  
                                          family=binomial(link="logit"))
      plot(Ice.logit)

    #Distance gam; logit link.
    #Set k=7 in s(Distance) because there are only 7 unique distances and the 
    #default basis dimension for a ts spline is larger than that. See note about
    #k and choose.k in the Year gam below for more information. The default k
    #is 10, and if you try building the gam with the default setting you'll
    #get an error.
    
      Distance.logit.gam <- gam(Yes/Pngs ~ s(Distance, bs="ts", k=7), #specify k
                                          data = seal.dat,
                                          weights = Pngs,
                                          method="REML",  
                                          family=binomial(link="logit"))
      plot(Distance.logit.gam)

    #Year gam; logit link. Because there are only 5 years, need to set the argument
    #k (basis dimension) in the smooth term to a value <= 5 or else the following
    #error occurs: 
    #  "Error in smooth.construct.tp.smooth.spec(object, data, knots) : 
    #  A term has fewer unique covariate combinations than specified maximum 
    #  degrees of freedom"
    #See the choose.k helpfile for more information, including the following:
    #  "In practice k-1 (or k) sets the upper limit on the degrees of freedom 
    #   associated with an s smooth (1 degree of freedom is usually lost to the 
    #   identifiability constraint on the smooth)." 
    
      Year.logit.ts <- gam(Yes/Pngs ~ s(Year, bs="ts", k=5), #thin plate regression spline w/ shrinkage
                                          data = seal.dat,
                                          weights = Pngs,
                                          method="REML",  
                                          family=binomial(link="logit"))
      plot(Year.logit.ts)
      
      #Build model with cs smooth, just out of curiosity, to compare with ts model
        Year.logit.cs <- gam(Yes/Pngs ~ s(Year, bs="cs", k=5), #cubic regression spline w/ shrinkage
                                            data = seal.dat,
                                            weights = Pngs,
                                            method="REML",  
                                            family=binomial(link="logit"))
        plot(Year.logit.cs)

    #Global gam. Mooring as a fixed factor affecting only intercept (this is 
    #likely a poor choice). logit link.
    
      global.logit.int <- gam(Yes/Pngs ~ s(CalJulian, bs="cc") + 
                                       Mooring + #Mooring as a fixed effect (factor) that
                                                 #  can only affect the intercept
                                       s(Ice, bs="ts") +       
                                       s(Year, bs="ts", k=5),
                            data = seal.dat,
                            weights = Pngs,
                            method="REML",                     
                            family=binomial(link="logit"))
      plot(global.logit.int) #Note that the plots are all scaled to have the
      #same range on the y-axis. Can change that using the scale argument
      #to plot.gam, as follows:
        plot(global.logit.int, scale=0)
                              
    #CalJulian hgam. "GS" model structure from Pedersen et al. (2019), with mooring  
    #as a factor in the smooth for CalJulian, creating a shared global trend with
    #individual smooths for each mooring, and each factor-level smooth shares
    #the same smoothing parameter. 
    #logit link. For more information on bs="fs", see  
    #?mgcv::factor.smooth.interaction.
    
      #I don't think this is the best way to model this. See CalJulian.logit.GS.xt
      #and associated notes below for a better approach and rationale.   
        CalJulian.logit.GS <- gam(Yes/Pngs ~ s(CalJulian, bs="cc", m=2) +
                                           s(CalJulian, Mooring, bs="fs", m=2), #not sure if I need "cc" anywhere here
                            data = seal.dat,
                            weights = Pngs,
                            method="REML",                     
                            family=binomial(link="logit"))
        plot(CalJulian.logit.GS)
      
      #I think the next model is the correct model because it specifies cc splines  
      #for the global smooth and also for each factor level smooth. The following
      #is from ?mgcv::factor.smooth.interaction :
      #  "Any singly penalized basis can be used to smooth at each factor level. 
      #  The default is "tp", but alternatives can be supplied in the xt argument 
      #  of s (e.g. s(x,fac,bs="fs",xt="cr") or s(x,fac,bs="fs",xt=list(bs="cr")). 
      #  The k argument to s(...,bs="fs") refers to the basis dimension to use for 
      #  each level of the factor variable."      
      
        CalJulian.logit.GS.xt <- gam(Yes/Pngs ~ s(CalJulian, bs="cc", m=2) + 
                                                s(CalJulian, Mooring, bs="fs", 
                                                  xt=list(bs="cc"), m=2), #cc
                            data = seal.dat,
                            weights = Pngs,
                            method="REML",                     
                            family=binomial(link="logit"))
        plot(CalJulian.logit.GS.xt)
        
    #AJD hgam. "GS" model structure from Pedersen et al. (2019), with mooring  
    #as a factor in the smooth for AJD, creating a shared global trend with
    #individual smooths for each mooring that share the smoothing parameter. 
    #logit link. For more information on bs="fs", see  
    #?mgcv::factor.smooth.interaction. Use "tx" splines for the global and 
    #all factor-level smooths.
    
      #This model didn't run with bs="ts" and I don't know why. Maybe it's not
      #possible to use shrinkage smoothers with bs="fs"?
        not.run <- TRUE
        if(!not.run){ #Do not run this code if not.run is TRUE
          AJD.logit.GS <- gam(Yes/Pngs ~ s(AJD, bs="ts", m=2) +
                                       s(AJD, Mooring, bs="fs", 
                                         xt=list(bs="ts"), m=2),
                              data = seal.dat,
                              weights = Pngs,
                              method="REML",                     
                              family=binomial(link="logit"))
          plot(AJD.logit.GS)
        }
      
      #Shrinkage smoother for global; regular tp smoother for factor levels    
        AJD.logit.GS.ts <- gam(Yes/Pngs ~ s(AJD, bs="ts", m=2) +
                                     s(AJD, Mooring, bs="fs", m=2),
                            data = seal.dat,
                            weights = Pngs,
                            method="REML",                     
                            family=binomial(link="logit"))
        plot(AJD.logit.GS.ts)
      
      #Regular tp smoothers for all smooths  
        AJD.logit.GS.tp <- gam(Yes/Pngs ~ s(AJD, bs="tp", m=2) +
                                     s(AJD, Mooring, bs="fs", 
                                         xt=list(bs="tp"), m=2),
                            data = seal.dat,
                            weights = Pngs,
                            method="REML",                     
                            family=binomial(link="logit"))
        plot(AJD.logit.GS.tp)

    #Ice hgam. "GS" model structure from Pedersen et al. (2019), with mooring  
    #as a factor in the smooth for Ice, creating a shared global trend with
    #individual smooths for each mooring that share the smoothing parameter. 
    #logit link.
    
      Ice.logit.GS <- gam(Yes/Pngs ~ s(Ice, bs="ts", m=2) + 
                                     s(Ice, Mooring, bs="fs", m=2), 
                            data = seal.dat,
                            weights = Pngs,
                            method="REML",                     
                            family=binomial(link="logit"))
      plot(Ice.logit.GS)
      
    #Create a fake latitude variable, numbered 1:9 from south to north based on 
    #mooring location. Based on the mooring map in Jess's manuscript, I think
    #the correct order is as follows:
    # 1: NM1
    # 2: KZ1
    # 3: PH1
    # 4: CL1
    # 5: IC1
    # 6: WT1
    # 7: IC2
    # 8: BF2
    # 9: IC3
      
      Fake.Lat <- rep(NA, length=nrow(seal.dat))
      Fake.Lat[which(seal.dat$Mooring == "NM1")] <- 1
      Fake.Lat[which(seal.dat$Mooring == "KZ1")] <- 2
      Fake.Lat[which(seal.dat$Mooring == "PH1")] <- 3
      Fake.Lat[which(seal.dat$Mooring == "CL1")] <- 4
      Fake.Lat[which(seal.dat$Mooring == "IC1")] <- 5
      Fake.Lat[which(seal.dat$Mooring == "WT1")] <- 6
      Fake.Lat[which(seal.dat$Mooring == "IC2")] <- 7
      Fake.Lat[which(seal.dat$Mooring == "BF2")] <- 8
      Fake.Lat[which(seal.dat$Mooring == "IC3")] <- 9
      
      seal.dat$Fake.Lat <- Fake.Lat
      
      summary(seal.dat$Fake.Lat)
      summary(as.factor(seal.dat$Fake.Lat))
      summary(seal.dat$Mooring)
      
    #Bivariate smooth of AJD and Fake.Lat. logit link.
      
        AJD.Fake.Lat.logit <- gam(Yes/Pngs ~ te(AJD, Fake.Lat, bs="ts"), #tensor product spline
                                      data = seal.dat,
                                      weights = Pngs,
                                      method="REML",  
                                      family=binomial(link="logit"))
        plot(AJD.Fake.Lat.logit)
      
    #Trivariate smooth of AJD, Fake.Lat, and Ice. logit link.
      
        AJD.Fake.Lat.Ice.logit <- gam(Yes/Pngs ~ te(AJD, Fake.Lat, Ice, bs="ts"),
                                      data = seal.dat,
                                      weights = Pngs,
                                      method="REML",  
                                      family=binomial(link="logit"))
        plot(AJD.Fake.Lat.Ice.logit) #These default figures are awful!!
          #I think each plot is showing Fake.Lat on the x-axis and AJD on the 
          #y-axis. Then, each plot sets Ice to be a constant value, and the
          #colors and contours show the value of the spline. I think the 
          #supplementary files for Pedersen et al. 2019 likely have good
          #examples of how to build better plots for these types of models.
          #Files available at https://github.com/eric-pedersen/mixed-effect-gams;
          #open supplemental_code.r and search for bird_modGS, which is Figure
          #12 in the manuscript.
          
    #AJD and Ice hgam. "GS" model structure from Pedersen et al. (2019), with mooring  
    #as a factor in the smooth for AJD and Ice, creating a shared global trend with
    #individual smooths for each mooring that share the smoothing parameter. 
    #logit link. See p. 15 of Pedersen et al. 2019 for coding details and his
    #supplemental_code.r for plotting suggestions. 
    
      #This model takes a while to build.  
        AJD.Ice.logit.GS <- gam(Yes/Pngs ~ te(AJD, Ice, bs="tp", m=2) + 
                                         t2(AJD, Ice, Mooring, 
                                            bs=c("tp", "tp", "re"),
                                            m=2, full=TRUE), 
                                        data = seal.dat,
                                        weights = Pngs,
                                        method="REML",                     
                                        family=binomial(link="logit"))
        plot(AJD.Ice.logit.GS) #This plot is not very helpful
          
    #Other types of variables and models to consider:
    # i. Consider examining latitude of each mooring instead of 
    #    mooring as a factor variable.
    # ii. Regular gam with a bivariate tensor product smooth 
    #      te(date, latitude). From the ?te helpfile: 
    #      "Tensor product smooths are especially useful for representing 
    #       functions of covariates measured in different units, although they 
    #       are typically not quite as nicely behaved as t.p.r.s. smooths for 
    #       well scaled covariates." In other words, te smooths can be anisotropic,
    #       whereas tprs smooths are only isotropic.
    # iii. Regular gam with a trivariate smooth: te(date, latitude, ice)
    # iv. Evaluate whether the logit link is appropriate. Another potential
    #    link fcn to use is the cloglog link.
    # v. Evaluate whether the binomial distribution is appropriate. If there
    #   are an overwhelmingly large number of zeros in the data, a zero-inflated
    #   binomial model might be better.

        
        
        
        
                
        
    #Models that MCF added on 12.27.
        
      #Explicitly omit NA data so that we know what dataset we're working with
        
        idx <- which(is.na(seal.dat$Yes) == TRUE)
        seal.dat.noNA <- seal.dat[-idx,]
        summary(seal.dat.noNA)
        
      #Make Month a factor 
        seal.dat.noNA <- cbind.data.frame(seal.dat.noNA, "mo.fact"=as.factor(seal.dat.noNA$Month))

      #Build CalJulian.Ice.mo.fact model  
        CalJulian.Ice.mo.fact.logit.GS <- gam(Yes/Pngs ~ te(CalJulian, Ice, bs=c("cc","tp"), m=2) + 
                                                       t2(CalJulian, Ice, mo.fact, 
                                                          bs=c("cc", "tp", "re"),
                                                          m=2, full=TRUE), 
                                                      data = seal.dat.noNA,
                                                      weights = Pngs,
                                                      method="REML",                     
                                                      family=binomial(link="logit"))
        gam.check(CalJulian.Ice.mo.fact.logit.GS) 
        
        
        
        
        
          #This says that the term te(CalJulian, Ice) has k' = 19.0. I'm not sure why it was 19. 
          #The helpfile for te() says the default k is 5^d, and I think d is the number of dimensions
          #in the smooth, which would be 2 in this case. The te() helpfile also says that if
          #k is supplied as a single number, that basis dimension is used for each basis. If k
          #is supplied as an array, then the elements are the dimensions of the component
          #marginal bases of the tensor product. The choose.k helpfile says that, for
          #te smooths, the upper limit of the degrees of freedom is given by the product of the 
          #k values provided for each marginal smooth, less one for the constraint. It seems like k' 
          #provided in the output from gam.check represents the upper limit on the degrees of freedom
          #for that term. Because CalJulian.Ice.logit.GS used the default k value, I 
          #expected the upper limit on the degrees of freedom to be 5^2 - 1 = 25 - 1 = 24, not 19. 
        #Examine residuals further to see if k is an issue 
          resids <- residuals.gam(CalJulian.Ice.mo.fact.logit.GS)
          dat <- cbind.data.frame(seal.dat.noNA, resids)
          CalJulian.Ice.mo.fact.logit.GS.resids <- gam(resids ~ te(CalJulian, Ice, bs=c("cc","tp"), m=2, k=20), 
                                                                            data = dat,
                                                                            method="REML",                     
                                                                            family=normal)
      
        
        
        
        
        
        
        
        
        
      
      #Try increasing k. See mgcv helpfiles for choose.k, te, and gam.check  
        CalJulian.Ice.logit.GS.k20 <- gam(Yes/Pngs ~ te(CalJulian, Ice, bs=c("cc","tp"), m=2, k=20) + 
                                         t2(CalJulian, Ice, Mooring, 
                                            bs=c("cc", "tp", "re"),
                                            m=2, full=TRUE), 
                                        data = seal.dat,
                                        weights = Pngs,
                                        method="REML",                     
                                        family=binomial(link="logit"))
        gam.check(CalJulian.Ice.logit.GS.k20)
        
        #Examine residuals further to see if k is an issue 
          resids.k20 <- residuals.gam(CalJulian.Ice.logit.GS.k20)
          CalJulian.Ice.logit.GS.k20.resids <- gam(Yes/Pngs ~ te(CalJulian, Ice, bs=c("cc","tp"), m=2, k=25), 
                                                              data = seal.dat,
                                                              weights = Pngs,
                                                              method="REML",                     
                                                              family=binomial(link="logit"))
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
      