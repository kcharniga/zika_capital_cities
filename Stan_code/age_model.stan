data{
  int<lower=0> l; //Number of cities
  vector[l] alphaZ; // From seroprevalence estimates
  vector[l] gammaZ; // From seroprevalence estimates

  int Nyoung[l];     //Population size in each city for youngs
  int Syoung[l];   //Reported suspect Zika cases in each city for youngs
  int NCyoung[l]; //Reported suspect neurological complications in each city for youngs

  int Nold[l];     //Population size in each city for olds
  int Sold[l];   //Reported suspect Zika cases in each city for olds
  int NCold[l]; //Reported suspect neurological complications in each city for youngs

  int Syoung_all; //Reported suspect Zika cases across all cities in youngs
  int NCyoung_all; //Reported suspect neurological complications across all cities in youngs
  int Nyoung_all; //total pop across all cities in youngs
  
  int Sold_all; //Reported suspect Zika cases across all cities in olds
  int NCold_all; //Reported suspect neurological complications across all cities in olds
  int Nold_all; //total pop across all cities in olds
}

parameters{
  real<lower=0,upper=1> pNC_young_min;
  real<lower=pNC_young_min,upper=1> pNC_young_max;
  real<lower=0,upper=1> pS_young_min;
  real<lower=pS_young_min,upper=1> pS_young_max;
  
  real<lower=0,upper=1> pNC_old_min;
  real<lower=pNC_old_min,upper=1> pNC_old_max;
  real<lower=0,upper=1> pS_old_min;
  real<lower=pS_old_min,upper=1> pS_old_max;
  
  real<lower=pS_young_min,upper=pS_young_max> pSyoung[l]; //Probability that a ZIKV infection is reported as a case to the surveillance system
  real<lower=pNC_young_min,upper=pNC_young_max> pNCyoung[l]; //Probability that a ZIKV infection becomes reported as a case with neurological complications
  
  real<lower=pS_old_min,upper=pS_old_max> pSold[l]; //Probability that a ZIKV infection is reported as a case to the surveillance system
  real<lower=pNC_old_min,upper=pNC_old_max> pNCold[l]; //Probability that a ZIKV infection becomes reported as a case with neurological complications
  
  real<lower=pS_young_min,upper=pS_young_max> pSyoung_all; //overall risk that a ZIKV infection is reported as a case to the surveillance system (youngs)
  real<lower=pNC_young_min,upper=pNC_young_max> pNCyoung_all; //overall risk that a ZIKV infection becomes reported as a case with neurological complications (youngs)
  
  real<lower=pS_old_min,upper=pS_old_max> pSold_all; //overall risk that a ZIKV infection is reported as a case to the surveillance system (olds)
  real<lower=pNC_old_min,upper=pNC_old_max> pNCold_all; //overall risk that a ZIKV infection becomes reported as a case with neurological complications (olds)
  
  real<lower=0,upper=1> pZ[l]; //Probability of Zika infection by city (youngs and olds)
  real<lower=0,upper=1> pZall; // Overall probability of Zika infection (youngs and olds)
}

model{
  
  //Hyper-priors (youngs)
  pS_young_min ~ uniform(0,1);
  pS_young_max ~ uniform(pS_young_min, 1);
  pNC_young_min ~ uniform(0,1);
  pNC_young_max ~ uniform(pNC_young_min, 1);
  
  //Hyper-priors (olds)
  pS_old_min ~ uniform(0,1);
  pS_old_max ~ uniform(pS_old_min, 1);
  pNC_old_min ~ uniform(0,1);
  pNC_old_max ~ uniform(pNC_old_min, 1);
  
  //Priors 
  for(i in 1:l){
  //youngs
  pSyoung[i] ~ uniform(pS_young_min, pS_young_max); // allows reporting rate of symptomatic Zika to vary by location
  pNCyoung[i] ~ uniform(pNC_young_min, pNC_young_max); // allows reporting rate of neurological complications to vary by location
  //olds
  pSold[i] ~ uniform(pS_old_min, pS_old_max); // allows reporting rate of symptomatic Zika to vary by location
  pNCold[i] ~ uniform(pNC_old_min, pNC_old_max); // allows reporting rate of neurological complications to vary by location
  //both
  pZ[i] ~ beta(alphaZ[i], gammaZ[i]); // representing possible ranges of attack rates from seroprevalence study
  }
  
  pSyoung_all ~ uniform(pS_young_min, pS_young_max); //overall risk
  pNCyoung_all ~ uniform(pNC_young_min, pNC_young_max); //overall risk
  
  pSold_all ~ uniform(pS_old_min, pS_old_max); //overall risk
  pNCold_all ~ uniform(pNC_old_min, pNC_old_max); //overall risk
  
  pZall ~ beta(1, 1); //assume same infection attack rate for both youngs and olds
  
  //Model
  for (i in 1:l){
    //youngs
    Syoung[i] ~ binomial(Nyoung[i], pZ[i]*pSyoung[i]); //Zika infections that give rise to suspected reported cases
    NCyoung[i] ~ binomial(Nyoung[i], pNCyoung[i]*pZ[i]); //Neurological complications
    //olds
    Sold[i] ~ binomial(Nold[i], pZ[i]*pSold[i]); //Zika infections that give rise to suspected reported cases
    NCold[i] ~ binomial(Nold[i], pNCold[i]*pZ[i]); //Neurological complications
  }
  
  Syoung_all ~ binomial(Nyoung_all, pSyoung_all*pZall);
  NCyoung_all ~ binomial(Nyoung_all, pNCyoung_all*pZall);
  
  Sold_all ~ binomial(Nold_all, pSold_all*pZall);
  NCold_all ~ binomial(Nold_all, pNCold_all*pZall);
 
}
