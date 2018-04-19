generate.data = function(n,p) {
  covariates = matrix(rnorm(n*p),nrow = n,ncol = p)
  responses = rnorm(n)
  return(list(covariates = covariates, responses = responses))
}

model_select = function(covariances,responses,cutoff){
  cov.lm = summary(lm(responses ~ covariances))
  p.values = cov.lm$coefficients[,"Pr(>|t|)"]
  p.values = p.values <= cutoff
  p.values = p.values[2:length(p.values)]
  if (length(covariances[,p.values]) == 0) { return(c())}
  ret.lm = summary(lm(responses ~ covariances[,p.values]))
  ret.lm.p = ret.lm$coefficients[,"Pr(>|t|)"]
  return(ret.lm.p)
}

run_simulation = function(n_trials, n, p, cutoff) {
  p.vals = c()
  for (i in 1:n_trials) {
    data = generate.data(n,p)
    result = model_select(data$covariates,data$responses,cutoff)
    p.vals = c(p.vals,result)
  }
  write.csv(p.vals, file = "pvals.csv",quote = FALSE, row.names = FALSE)
}

make_plot = function(datapath) {
  p.vals = read.csv(file = datapath)
  hist(as.numeric(p.vals$x))
}

for (n in c(100,1000,10000)) {
  for (p in c(10,20,50)) {
    run_simulation(30,n,p,0.05)
    make_plot("pvals.csv")
  }
}
