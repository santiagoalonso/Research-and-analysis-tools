within_sem <-function(data){ 
  #Computes means and std. error of mean for within subject designs.
  #Useful to plot error bars. 
  
  #Reference: Cousineau, D (2005). Confidence intervals in within-subject designs:
  #           A simpler solution to Loftus and Masson's method.
  
  #INPUT
  #   data: subj x condition (dataframe or matrix). 
  #     That is, rows subjects, cols within subject conditions. 
  #     e.g. col 1 RTs for fast SOA, col 2 RTs for medium SOA, 
  #     col 3 RTs for fast SOA (SOA: stimulus onset asynchrony)
  
  #OUTPUT
  #   out: list with 2 elements: 1st means by conditions, 2nd sem by conditions.
  
  sa = apply(data, 1, mean, na.rm = T)  #subject average
  ga = mean(sa) #grand average
  
  nv = matrix(NA, nrow = dim(data)[1], ncol = dim(data)[2]) #normalized values (i.e. no between subject noise)
  colnames(nv) = sprintf('condition %s', 1:dim(data)[2])
  nsubj = dim(data)[1]
  conds = dim(data)[2]
  for (cond in 1:conds){
    nv[,cond] =  data[,cond] - sa + rep(ga, nsubj)
  }
  avg = apply(data, 2, mean, na.rm = T) 
  sem = (apply(nv, 2, sd)/sqrt(nsubj))
  
  out = list(mean = avg, sem = sem)
  out
}
