data {
  int<lower = 1> T;       // Number of time periods
  int<lower=0> N;         // number of observations
  int<lower=1> K;         //number of teams
  int<lower=1> day[N];
  int<lower=1> player[N];
  //row_vector[K] x;              // Independent variable for prior (players)
  //row_vector[T] z;              // Independent variable for prior (days)
  int<lower = 1> y[N];              // solve time
}
parameters {
  matrix[T,K] mu; //skill
  vector[T] p; //puzzle difficulty
  real<lower=0> sigma_proc; // process noise (skill drift)
  real<lower=0> sigma_obs;  // observation noise
  real alpha;
  
  //real alpha;                    // standard home field advantage
  //real beta_x;                   // coefficient on prior
  //real<lower = 0> sigma_state;   // week to week variation
  //real<lower = 0> sigma_ability; // team ability variation
  //real<lower = 0> sigma;         // residual standard error
  //matrix[T,K] eta_mu; //Matrix of team abilities
  //vector[T] p; //vector of puzzle difficulty
}


model {
  // Priors
  
  mu[1] ~ normal(0, 60); // prior for initial skill
  p ~ normal(0, 60);
  sigma_proc ~ normal(0, 5);
  sigma_obs ~ normal(0, 60);
  alpha ~ normal(mean(y), 10);

  // State evolution
  for (t in 2:T) {
    mu[t] ~ normal(mu[t - 1], sigma_proc);  // skill evolves as random walk
  }

  // Observation model
  for (n in 1:N){
    y[n] ~ normal(alpha + mu[day[n], player[n]] + p[day[n]], sigma_obs);
  } 
}