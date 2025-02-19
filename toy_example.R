## toy example described in the KRCP review paper
##
## Ke Xu and Hakmook Kang
## Dept. of Biostatistics
## Vanderbilt University Medical Center
##

# use ggplot2 package to generate a plot
library(ggplot2)

# to make the results reproducible, we set the seed for random number
# generation at 1234
set.seed(1234)

# investigate 8 different sample sizes
n_lag = c(30,40, 50, 60, 70, 80, 90, 100)


### write a function to compute the bias
## bias = abs(beta_3_without_noise - beta_3_with_noise)
bias_fun = function(N){
  # N = number of iterations, in the manuscript we used N=300
  Bias_out = NULL
  for (k in 1:N){
    x1=rnorm(100) # random sample (size = 100) from Normal(0,1) for x1
    
    x2 = ifelse(rnorm(100)>0, 1, 0) # x2 = binary, P(x2=0) = P(x2=1) = 0.5
    x3 = rnorm(100)+2   # x3 = random sample from Normal(2, 1)
    
    y1 = 1+2*x1 + x2 + x3 + rnorm(100)  # beta = c(1, 2, 1, 1)
    fit0 = lm(y1 ~ x1+x2+x3)    # fitting a linear regression model
    test0 = summary(fit0)       # store summary of 'fit0' as test0
    dat_test = data.frame(y1, x1, x2, x3)
    
    # with differnt n, reducing sample size from 100 to 90,80,..., 30
    Out = NULL
    for (m in 1:length(n_lag)){
    
    # randomly choose rows from data_test without replacement
    # sample size = n_lag[m], when m = 2, then the sample size = 40
    dat_samp = dat_test[sample(nrow(dat_test), n_lag[m], replace=FALSE),]
    
    dat_samp$x3n = dat_samp$x3
    # double the first 10 observations in x3
    dat_samp$x3n[1:10] = dat_samp$x3n[1:10]*2 
    
    # fit the same model with updated data set
    fit1 = lm(y1 ~ x1+x2+x3n, data = dat_samp)
    test = summary(fit1)
    
    # compute the bias
    out_t = abs(abs(test0$coefficients[4,1]) - abs(test$coefficients[4,1]))
    Out = cbind(Out, out_t)
    }
    Bias_out = rbind(Bias_out, Out)
  }
  
  return(Bias_out)
}


test_out = bias_fun(300)
bias_result = colMeans(test_out) # compute the average bias for each sample size

# generate a plot

###################################
# To SAVE the PLOT as Bias_plot.tiff file
# you need to set your own path 
# and uncomment (delete '#' in front of setwd, tiff, and dev.off() commands)
##################################

#setwd('your_own_path')
#tiff("bias_plot.tiff", units="in", width=5, height=5, res=300)
qplot(n_lag,bias_result)+geom_line(col="red")+theme_bw() +
  ggtitle("Bias vs. Sample Size")+ theme(plot.title = element_text(size=15,hjust = .5)) +
  xlab("Sample size") + ylab("Bias")
#dev.off()

