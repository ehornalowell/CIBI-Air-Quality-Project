model{
  ### PRIORS
  ## Hyperpriors
  mu.omega ~ dnorm(0, 0.1)
  tau.omega ~ dgamma(1,1)
  sig.omega <- 1 / sqrt(tau.omega)
  # for abundance
  mu.mu.beta0 ~ dnorm(0, 0.1)
  mu.tau.beta0 ~ dgamma(1, 1)
  mu.sig.beta0 <- 1 / sqrt(mu.tau.beta0)
  mu.mu.beta1 ~ dnorm(0, 0.1)
  mu.tau.beta1 ~ dgamma(1, 1)
  mu.sig.beta1 <- 1 / sqrt(mu.tau.beta1)
  mu.mu.beta2 ~ dnorm(0, 0.1)
  mu.tau.beta2 ~ dgamma(1, 1)
  mu.sig.beta2 <- 1 / sqrt(mu.tau.beta2)
  mu.mu.beta3 ~ dnorm(0, 0.1)
  mu.tau.beta3 ~ dgamma(1, 1)
  mu.sig.beta3 <- 1 / sqrt(mu.tau.beta3)
  mu.phi ~ dnorm(0, 0.1)
  tau.phi ~ dgamma(2,1)
  sig.phi <- 1 / sqrt(tau.phi)

  # for captures
  mu.mu.alpha0 ~ dnorm(0, 0.1)
  mu.tau.alpha0 ~ dgamma(1, 1)
  mu.sig.alpha0 <- 1 / sqrt(mu.tau.alpha0)
  mu.mu.alpha2 ~ dnorm(0, 0.1)
  mu.tau.alpha2 ~ dgamma(1,1)
  mu.sig.alpha2 <- 1 / sqrt(mu.tau.alpha2)
  mu.mu.alpha3 ~ dnorm(0, 0.1)
  mu.tau.alpha3 ~ dgamma(1,1)
  mu.sig.alpha3 <- 1 / sqrt(mu.tau.alpha3)

  ## Order-Specific Priors
  for (q in 1:nOrder){
    mu.beta0[q] ~ dnorm(mu.mu.beta0, mu.tau.beta0)
    mu.beta1[q] ~ dnorm(mu.mu.beta1, mu.tau.beta1)
    mu.beta2[q] ~ dnorm(mu.mu.beta2, mu.tau.beta2)
    mu.beta3[q] ~ dnorm(mu.mu.beta3, mu.tau.beta3)
    mu.alpha0[q] ~ dnorm(mu.mu.alpha0, mu.tau.alpha0)
    mu.alpha2[q] ~ dnorm(mu.mu.alpha2, mu.tau.alpha2)
    mu.alpha3[q] ~ dnorm(mu.mu.alpha3, mu.tau.alpha3)
  }
  tau.shape.alpha0 ~ dunif(0.001, 5)
  tau.rate.alpha0 ~ dunif(0.001, 5)
  tau.shape.alpha ~ dunif(0.001, 5)
  tau.rate.alpha ~ dunif(0.001, 5)
  tau.shape.beta ~ dunif(0.001, 5)
  tau.rate.beta ~ dunif(0.001, 5)
  for(i in 1:nTaxa){
    tau.alpha0[i] ~ dgamma(tau.shape.alpha0, tau.rate.alpha0)
    sd.alpha0[i] <- 1 / sqrt(tau.alpha0[i])
    tau.alpha2[i] ~ dgamma(tau.shape.alpha, tau.rate.alpha)
    sd.alpha2[i] <- 1 / sqrt(tau.alpha2[i])
    tau.alpha3[i] ~ dgamma(tau.shape.alpha, tau.rate.alpha)
    sd.alpha3[i] <- 1 / sqrt(tau.alpha3[i])
    tau.beta0[i] ~ dgamma(tau.shape.beta, tau.rate.beta)
    sd.beta0[i] <- 1 / sqrt(tau.beta0[i])
    tau.beta1[i] ~ dgamma(tau.shape.beta, tau.rate.beta)
    sd.beta1[i] <- 1 / sqrt(tau.beta1[i])
    tau.beta2[i] ~ dgamma(tau.shape.beta, tau.rate.beta)
    sd.beta2[i] <- 1 / sqrt(tau.beta2[i])
    tau.beta3[i] ~ dgamma(tau.shape.beta, tau.rate.beta)
    sd.beta3[i] <- 1 / sqrt(tau.beta3[i])
  }
  
  ## Species-Specific Priors
  for (i in 1:nTaxa) {
    alpha0[i] ~ dnorm(mu.alpha0[order[i]], tau.alpha0[i])
    alpha2[i] ~ dnorm(mu.alpha2[order[i]], tau.alpha2[i])
    alpha3[i] ~ dnorm(mu.alpha3[order[i]], tau.alpha3[i])
    phi[i] ~ dnorm(mu.phi, tau.phi)
    for (j in 1:nSite){
      beta0[i,j] ~ dnorm(mu.beta0[order[i]], tau.beta0[i])
      beta1[i,j] ~ dnorm(mu.beta1[order[i]], tau.beta1[i])
      beta2[i,j] ~ dnorm(mu.beta2[order[i]], tau.beta2[i])
      beta3[i,j] ~ dnorm(mu.beta3[order[i]], tau.beta3[i])
      omega[i,j] ~ dnorm(mu.omega, tau.omega)
    }
  }
  
  ### MODEL
  # First sampling month
  # State Process
  for (i in 1:nTaxa) {
    for (j in 1:nSite) {
      w[i,j] ~ dbern(1-omega[i,j])
      N[i,j,1] ~ dpois(lambda[i,j,1]*w[i,j])
      log(lambda[i,j,1]) <- beta0[i,j] + beta1[i,j]*PM[j,1] + 
        beta2[i,j]*smoke[j,1] + beta3[i,j]*PC1[j,1]
      # Observation Process
      y[i,j,1] ~ dbinom(p[i,j,1], N[i,j,1])
      logit(p[i,j,1]) <- alpha0[i] + alpha2[i]*PC1[j,1] + 
        alpha3[i]*PC2[j,1]
  # Subsequent Sampling Months
  # State Process
      for (t in 2:nMonth){
        N[i,j,t] ~ dpois(lambda[i,j,t]*w[i,j])
        log(lambda[i,j,t]) <- beta0[i,j] + beta1[i,j]*PM[j,t] + 
          beta2[i,j]*smoke[j,t] + beta3[i,j]*PC1[j,t] + phi[i]*log(N[i,j,t-1]+1)
        # Observation Process
        y[i,j,t] ~ dbinom(p[i,j,t], N[i,j,t])
        logit(p[i,j,t]) <- alpha0[i] + alpha2[i]*PC1[j,t] + 
          alpha3[i]*PC2[j,t]
      }
    }
  }
  # Derived Parameters
  for(j in 1:nSite){
    for(t in 1:nMonth){
      Nsite[j,t] <- sum(N[,j,t]) # abundance of all insects at each site each month
    }
  }
  for(i in 1:nTaxa){
    for(j in 1:nSite){
      for(t in 1:nMonth){
        P[i,j,t] <- N[i,j,t]/(sum(N[,j,t])) # proportion of individuals of each species
      }
    }
  }
}