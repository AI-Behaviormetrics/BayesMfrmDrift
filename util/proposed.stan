data{
  int <lower=1> J; //n_examinee
  int <lower=1> R; //n_rater
  int <lower=1> T; //n_time
  int <lower=2> K; //n_score
  int <lower=1> N; //n_samples
  int <lower=1, upper=J> ExamineeID [N];
  int <lower=1, upper=R> RaterID [N];
  int <lower=1, upper=T> TimeID[N];
  int <lower=1, upper=K> X [N];
  real hyperprior_beta_rt[2];
}
transformed data{
  vector[K] c = cumulative_sum(rep_vector(1, K)) - 1;
}
parameters {
  vector[J] theta;
  matrix[R,T] beta_rt;
  vector[K-2] beta_rk[R];
  real<lower=0> sigma_beta_rt[R];
}
transformed parameters{
  vector[K-1] category_est[R];
  vector[K] category_prm[R];
  for(r in 1:R) {
    category_est[r,1:(K-2)] = beta_rk[r];
    category_est[r,K-1] = -1 * sum(beta_rk[r]);
    category_prm[r] = cumulative_sum(append_row(0, category_est[r]));
  }
}
model{
  theta ~ normal(0, 1);
  sigma_beta_rt ~ lognormal(hyperprior_beta_rt[1], hyperprior_beta_rt[2]);
  for (r in 1:R){
    beta_rt[r,1] ~ normal(0, 1);
    for (t in 2:T){
      beta_rt[r,t] ~ normal(beta_rt[r,t-1], sigma_beta_rt[r]);
    }
  }
  for (r in 1:R) category_est [r,] ~ normal(0, 1);
  for (n in 1:N){
    X[n] ~ categorical_logit(1.7*(c*(theta[ExamineeID[n]]-beta_rt[RaterID[n],TimeID[n]])-category_prm[RaterID[n]]));      
  }
}
generated quantities {
  vector[N] log_lik;
  for (n in 1:N){
    log_lik[n] = categorical_logit_log(X[n], 1.7*(c*(theta[ExamineeID[n]]-beta_rt[RaterID[n],TimeID[n]])-category_prm[RaterID[n]]));    
  }
}
