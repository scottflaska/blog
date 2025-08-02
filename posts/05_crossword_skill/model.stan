data {
  int<lower = 1> T;         // number of days
  int<lower=0> N;           // number of observations
  int<lower=1> K;           // number of players
  int<lower=1> day[N];      // observation day 
  int<lower=1> player[N];   // observation player
  int<lower = 1> y[N];      // observation solution time
}

parameters {
  matrix[T,K] mu;           // estimated player skill by day
  vector[T] rho;            // estimated puzzle difficulty for day
  real<lower=0> sigma_proc; // process noise (change in skill)
  real<lower=0> sigma_obs;  // observation noise 
  real alpha;               // global intercept
}

model {
  // Priors
  mu[1] ~ normal(0, 60);
  rho ~ normal(0, 60);
  sigma_proc ~ normal(0, 5);
  sigma_obs ~ normal(0, 60);
  alpha ~ normal(mean(y), 20);

  // State evolution
  for (t in 2:T) {
    mu[t] ~ normal(mu[t - 1], sigma_proc);
  }

  // Observation model
  for (n in 1:N){
    y[n] ~ normal(alpha + mu[day[n], player[n]] + rho[day[n]], sigma_obs);
  } 
}