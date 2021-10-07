####################################################
## the FDH model
####################################################

# Loop for m selection

t <- length(x[,1])

temp <- matrix(nrow=t,ncol=1) # define a matrix where you put the results of each loop
m.select <- matrix(nrow=t,ncol=2) # define the matrix to salve results

for (m in (1:t)){
  
  for (i in (1:t)){
    print(i)
    eff <- integrate(f,0,Inf,x=x,y=y,i=i,m=m, stop.on.error = FALSE) # integrate from O to infinity
    temp[i] <- eff$value
  }
  
  m.select[m,1] = m
  m.select[m,2] = sum(temp < 1)
  
}

plot(m.select, type = 's', xlab = 'm', ylab = 'super-efficient countries')


