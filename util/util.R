get_estimates <- function(fit, setting){
  theta <- summary(fit, par="theta")$summary[,"mean"]
  beta_rt <- convert_beta_rt(summary(fit, par="beta_rt")$summary[,"mean"] ,setting$n_time, setting$n_rater)
  category_prm <- convert_category_estimates(summary(fit, par="beta_rk")$summary[,"mean"],setting$n_rater, setting$K)
  sigma_beta_rt <- summary(fit, par="sigma_beta_rt")$summary[,"mean"]
  rater_parameters <- t(rbind(beta_rt, category_prm, sigma_beta_rt))
  rownames(rater_parameters) <- paste("rater", c(1:length(sigma_beta_rt)), sep="")
  names(theta) <- NULL
  return(list(examinee_ability = theta, rater_parameters = rater_parameters))
}

# Get rater-specific step parameters from Stan outputs 
convert_category_estimates <- function(category_prm, N, K){
  if (N == 1){
    prm = category_prm[1:(K-2)]
    prm = append(prm, -1*sum(prm))
    mat = t(data.frame(prm))
    return(mat)    
  } else {
    for(n in 1:N){
      prm = category_prm[((n-1)*(K-2)+1):((n-1)*(K-2)+(K-2))]
      prm = append(prm, -1*sum(prm))
      if(n == 1){
        mat = t(data.frame(prm))
      } else {
        mat = rbind(mat, t(data.frame(prm)))
      }
    }  
    mat = t(mat)
    rownames(mat) <- paste("d_r", c(1:(K-1)), sep="")
    return(mat)
  }
}

# Get time-specific rater severity parameters from Stan outputs 
convert_beta_rt <- function(beta_rt, T, R){
  for(r in 1:R){
    alrt = beta_rt[((r-1)*T+1):((r-1)*T+T)]
    if(r == 1){
      mat = t(data.frame(alrt))
    }else{
      mat = rbind(mat,t(data.frame(alrt)))
    }
  }
  mat = t(mat)
  rownames(mat) <- paste("beta_r", c(1:T), sep="")
  return(mat)
}

# Read actual data
read_data <- function(setting, filename){
  data <- read.table(filename, header=TRUE,sep=",")
  colnames(data) <- c("ExamineeID","RaterID","TimeID","Score")
  data_irt=list(
    K=setting$K, 
    J=setting$n_person, 
    R=setting$n_rater, 
    T=setting$n_time, 
    N=nrow(data), 
    ExamineeID=data$ExamineeID, 
    RaterID=data$RaterID, 
    TimeID=data$TimeID, 
    X=data$Score,
    hyperprior_beta_rt = setting$hyperprior_beta_rt
    )
  return(data_irt)
}
