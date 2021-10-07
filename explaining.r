####################################################
## partial frontier, order-m
####################################################

  # Packages

  require(np)

  # Parameters
  
  mm = 20
  
  t <- length(x[,1])
  p <- length(x[1,])
  
  f <- function(lambda,x,y,i,m) # define a function, depending on the efficiency score lambda
  {
    nsum <- 0; dsum <- 0
    
    for (j in (1:t))
    {
      
      n = as.numeric((x[j,1] <= x[i,1]) & (x[j,2] <= x[i,2]) & (y[j] >= (y[i] * lambda))) # indicator function to test whether it is lower or higher than a specific value
      d = as.numeric((x[j,1] <= x[i,1]) & (x[j,2] <= x[i,2])) # in theory we have y[j] <= 0, or simply the empirical CDF on X
      
      nsum <- n+nsum
      dsum <- d+dsum
    }
    
    if(dsum==0)
    {
      dsum <- 1
    }
    
    return(1-(1-(nsum/dsum))^m)
  }
  
  eff.uncond <- matrix(nrow=t,ncol=1) # define a matrix where you put your results 
  
  for (i in (1:t)){
    print(i)
    eff <- integrate(f,0,Inf,x=x,y=y,i=i,m=mm, stop.on.error = F) # integrate from O to infinity
    eff.uncond[i] <- eff$value
  }
  
  eff.output.uncond = as.tibble(cbind(data$iso_code, y, x, z, eff.uncond))
  colnames(eff.output.uncond) <- c("Country", "y", "x_1", "x_2", "z_1", "z_2", "z_3", "Lambda")

  eff.output.uncond = transform( eff.output.uncond,
                                 y = as.numeric(y),
                                 x_1 = as.numeric(x_1),
                                 x_2 = as.numeric(x_2),
                                 z_1 = as.numeric(z_1),
                                 z_2 = as.numeric(z_2),
                                 z_3 = as.numeric(z_3),
                                 Lambda = as.numeric(Lambda) )
  eff.output.uncond <- eff.output.uncond[order(eff.output.uncond[,8],decreasing = T),]

####################################################
## estimate the conditional efficiency estimates by the integral
####################################################
  
  dat <- data.frame(z)
  bw <- npudensbw(dat=dat, 
                      # bws=c(2.166798, 5.529434, 3.697385), # Objective Function Value: 3.118069e-05
                      bws=c(2.170529 , 5.552987 , 3.698998 ), # Objective Function Value: 3.128868e-05
                      bwmethod= 'cv.ls', ckertype="epanechnikov", 
                      nmulti=100, bandwidth.compute=F) # least squares cross-validation method to define bandwidth
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

  eff.output.cond = as.tibble(cbind(data$iso_code, y, x, z, eff.int))
  colnames(eff.output.cond) <- c("Country", "y", "x_1", "x_2", "z_1", "z_2", "z_3", "Lambda")

  eff.output.cond = transform( eff.output.cond,
                               y = as.numeric(y),
                               x_1 = as.numeric(x_1),
                               x_2 = as.numeric(x_2),
                               z_1 = as.numeric(z_1),
                               z_2 = as.numeric(z_2),
                               z_3 = as.numeric(z_3),
                               Lambda = as.numeric(Lambda) )
  eff.output.cond <- eff.output.cond[order(eff.output.cond[,8],decreasing = T),]

####################################################
#### explaining efficiency
####################################################

  effratio <-  eff.int / eff.uncond
  reg.dat = as.data.frame(cbind(effratio,dat))
  # colnames(reg.dat) <- c("effratio", "z1 (Median Age)", "z2 (Stringency Index)", "z3 (Retail and Recreation Visitors Change)")
  colnames(reg.dat) <- c("effratio", "z1", "z2", "z3")
  
  rbw <- npregbw(effratio ~ z1 + z2 + z3, data = reg.dat,
                 # bws=c(4.561579, 8.145833, 3.204445), # Objective Function Value: 0.0201205
                 bws=c(4.740464 , 8.145832 , 3.195359 ), # Objective Function Value: 0.02099532  
                 bwmethod = "cv.ls", regtype="lc", ckertype="epanechnikov",oxkertype="liracine", 
                 nmulti=100, bandwidth.compute=F)
  summary(rbw)
  
  model <- npreg(bws=rbw, gradients=TRUE)
  signif <- npsigtest(bws=rbw, boot.num=1000)
  
  npplot(bws=rbw,xq=0.5, zq=0.5, common.scale=F, perspective=F, 
          ylab = "Efficiency Ratio" ,type = "p", col = "purple", 
         plot.errors.method = "asymptotic", plot.errors.style = "bar")
  
  print(signif)
  
  plot(z[,1], effratio, ylab = "Ratio", xlab = "Median age")
  plot(z[,2], effratio, ylab = "Ratio", xlab = "Stringency Index")
  plot(z[,3], effratio, ylab = "Ratio", xlab = "Mobility")
  