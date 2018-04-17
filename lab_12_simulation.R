generate.data = function(n,p) {
  covariates = matrix(rnorm(n*p),nrow = n,ncol = p)
  responses = rnorm(n)
  return(list(covariates = covariates, responses = responses))
}

model_select = function(covariances,responses,cutoff){
  cov.lm = summary(lm(responses ~ covariates))
  p.values = cov.lm$coefficients[,"Pr(>|t|)"]
  p.values = p.values <= cutoff
  ret.lm = summary(lm(responses ~ covariates[,p.values]))
  ret.lm.p = ret.lm$coefficients[,"Pr(>|t|)"]
  return(ret.lm.p)
}

