####################################################
## estimate the conditional efficiency estimates by the integral
####################################################

  # Packages
  
  require(np)
  require(xtable)
  
  # Parameters
  
  mm = 20
  
  t <- length(x[,1])
  p <- length(x[1,])
  
  dat <- data.frame(z)
  bw <- np::npudensbw(dat=dat, 
                      # bws=c(1.590294), # Objective Function Value: 0.03153068
                      bws=c(1.590695), # Objective Function Value: 0.03141853
                      bwmethod= 'cv.ls', ckertype="epanechnikov", 
                      nmulti=1000, bandwidth.compute=F) # least squares cross-validation method to define bandwidth
  summary(bw)
  
  f <- function(lambda,x,y,i,m) # define a function, depending on the efficiency score lambda
  {
    
    tdata <- dat[i,]
    ker <- npudens(bws=bw,cykertype="epanechnikov",cxkertype="epanechnikov",oxkertype="liracine",tdat=tdata,edat=dat)
    
    K <- ker$dens
    nsum <- 0; dsum <- 0
    
    for (j in (1:t))
    {
      
      n = (as.numeric((x[j,1] <= x[i,1]) & (x[j,2] <= x[i,2]) & (y[j] >= (y[i] * lambda)))) * K[j]
      d = (as.numeric((x[j,1] <= x[i,1]) & (x[j,2] <= x[i,2]))) * K[j]
      
      nsum <- n+nsum
      dsum <- d+dsum
    }
    
    if(dsum==0)
    {
      dsum <- 1
    }
    
    return(1-(1-(nsum/dsum))^m)
  }
  
  eff.int <- matrix(nrow=t,ncol=1) # define a matrix where you put your results
  
  for (i in (1:t))
  {
    print(i)
    eff <- integrate(f,0,Inf,x=x,y=y,i=i,m=mm, stop.on.error=FALSE) # integrate from O to infinity
    eff.int[i] <- eff$value
  }
  
####################################################
#### managerial efficiency
####################################################
  
  reg.dat = as.data.frame(cbind(log(eff.int),dat))
  colnames(reg.dat) <- c("eff.int", "z1")
  
  rbw <- npregbw(eff.int ~ z1, data = reg.dat,
                  # bws=c(1.431084), # Objective Function Value: 0.1898811 
                  bws=c(1.431084), # Objective Function Value: 0.1890746 
                  bwmethod = "cv.ls", regtype="lc", ckertype="epanechnikov",oxkertype="liracine", 
                  nmulti=1000, bandwidth.compute=F)
  summary(rbw)
  
  model <- npreg(bws=rbw, gradients=TRUE, residuals=TRUE)
  signif <- npsigtest(bws=rbw, boot.num=1000)
  
  npplot(bws=rbw,xq=0.5, common.scale=FALSE, perspective=FALSE)
  
  print(signif)
  
  # Table
  
    mu = model$mean
    sigma = model$merr
    eps = model$resid/model$merr
    
    eff.output.cond_r = as.tibble(cbind(data$iso_code, y, x, z, log(eff.int), mu, sigma, eps))
    colnames(eff.output.cond_r) <- c("Country", "y", "x_1", "x_2", "z_1", "Lambda", "Mu", "Sigma", "Epsilon")
  
    eff.output.cond_r = transform( eff.output.cond_r,
                                   y = as.numeric(y),
                                   x_1 = as.numeric(x_1),
                                   x_2 = as.numeric(x_2),
                                   z_1 = as.numeric(z_1),
                                   Lambda = as.numeric(Lambda),
                                   Mu = as.numeric(Mu),
                                   Sigma = as.numeric(Sigma),
                                   Epsilon = as.numeric(Epsilon) )
    eff.output.cond_r <- eff.output.cond_r[order(eff.output.cond_r[,9],decreasing = T),]
     
    final_table <- inner_join(eff.output.uncond, eff.output.cond_r, 
                              by=c("Country"="Country"), suffix = c("", ".cond")) %>% 
                   select(Country, y, x_1, x_2, z_1, Lambda, Lambda.cond, Mu, Sigma, Epsilon)
    
    final_table[,6] <- log(final_table[,6])
    final_table <- final_table[order(final_table[,10],decreasing=F),]
    
    xtable(final_table)
  
  # Figures 

      plot(z, log(eff.int), ylab = "Log of conditional efficiencies", xlab = "Median age")
      points(z, sigma, col="red")
      points(z, mu, col="blue")

      legend("topleft", inset=.05, legend=c(expression(paste(mu, "(z)")),
                                 expression(paste(sigma, "(z)")), "data points"),
             col=c("red", "blue", "black"), pch=1, cex=1, bty="n")



      plot(z,eps, ylab = "Estimated managerial efficiencies", xlab = "Median age", col="purple")
      cor(z,eps)

      hist(eps, xlab = expression(paste("Values of ", epsilon)), main = "")
      
  # Other
      
      plot(final_table$y,final_table$Epsilon)
      plot(final_table$x_1,final_table$Epsilon)
      plot(final_table$x_2,final_table$Epsilon)
