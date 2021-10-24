data{
  int<lower=0> l; //Number of cities (locations)
  vector[l] alphaZ; // From seroprevalence estimates
  vector[l] gammaZ; // From seroprevalence estimates
  int N[l];       //Population size in each city
  int S[l];   //Reported suspect Zika cases in each city
  int NC[l]; //Reported suspect neurological complications in each city
  int Sall; //Reported suspect Zika cases across all cities
  int NCall; //Reported suspect neurological complications across all cities
  int Nall; //total population across all cities
}

parameters{
  real<lower=0,upper=1> pNC_min;
  real<lower=pNC_min,upper=1> pNC_max;
  real<lower=0,upper=1> pS_min;
  real<lower=pS_min,upper=1> pS_max;
  real<lower=0,upper=1> pZ[l]; //Probability of Zika infection
  real<lower=pS_min,upper=pS_max> pS[l]; //Probability that a ZIKV infection is reported as a case to the surveillance system
  real<lower=pNC_min,upper=pNC_max> pNC[l]; //Probability that a ZIKV infection becomes reported as a case with neurological complications
  real<lower=pS_min,upper=pS_max> pSall; //overall risk that a ZIKV infection is reported as a case to the surveillance system
  real<lower=pNC_min,upper=pNC_max> pNCall; //overall risk that a ZIKV infection becomes reported as a case with neurological complications
  real<lower=0,upper=1> pZall;
}

model{
  
  //Hyper-priors
  pS_min ~ uniform(0,1);
  pS_max ~ uniform(pS_min, 1);
  pNC_min ~ uniform(0,1);
  pNC_max ~ uniform(pNC_min, 1);
  
  //Priors
  for(i in 1:l){
  pS[i] ~ uniform(pS_min, pS_max); // allows reporting rate of ZVD to vary by location
  pNC[i] ~ uniform(pNC_min, pNC_max); // allows reporting rate of neurological complications to vary by location
  pZ[i] ~ beta(alphaZ[i], gammaZ[i]); // representing possible ranges of attack rates from seroprevalence study
  }
  
  pSall ~ uniform(pS_min, pS_max); //overall risk
  pNCall ~ uniform(pNC_min, pNC_max); //overall risk
  pZall ~ beta(1, 1);
  
  //Model
  for (i in 1:l){
    S[i] ~ binomial(N[i], pZ[i]*pS[i]); //Zika infections that give rise to suspected reported cases
    NC[i] ~ binomial(N[i], pNC[i]*pZ[i]); //Neurological complications
  }
  
  Sall ~ binomial(Nall, pSall*pZall);
  NCall ~ binomial(Nall, pNCall*pZall);
 
}
