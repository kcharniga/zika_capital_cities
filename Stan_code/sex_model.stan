data{
  int<lower=0> l; //Number of cities
  vector[l] alphaZ; // From seroprevalence estimates
  vector[l] gammaZ; // From seroprevalence estimates

  int Nmale[l];     //Population size in each city for males
  int Smale[l];   //Reported suspect Zika cases in each city for males
  int NCmale[l]; //Reported suspect neurological complications in each city for males

  int Nfemale[l];     //Population size in each city for females
  int Sfemale[l];   //Reported suspect Zika cases in each city for females
  int NCfemale[l]; //Reported suspect neurological complications in each city for males

  int Smale_all; //Reported suspect Zika cases across all cities in males
  int NCmale_all; //Reported suspect neurological complications across all cities in males
  int Nmale_all; //total pop across all cities in males
  
  int Sfemale_all; //Reported suspect Zika cases across all cities in females
  int NCfemale_all; //Reported suspect neurological complications across all cities in females
  int Nfemale_all; //total pop across all cities in females
}

parameters{
  real<lower=0,upper=1> pNC_male_min;
  real<lower=pNC_male_min,upper=1> pNC_male_max;
  real<lower=0,upper=1> pS_male_min;
  real<lower=pS_male_min,upper=1> pS_male_max;
  
  real<lower=0,upper=1> pNC_female_min;
  real<lower=pNC_female_min,upper=1> pNC_female_max;
  real<lower=0,upper=1> pS_female_min;
  real<lower=pS_female_min,upper=1> pS_female_max;
  
  real<lower=pS_male_min,upper=pS_male_max> pSmale[l]; //Probability that a ZIKV infection is reported as a case to the surveillance system
  real<lower=pNC_male_min,upper=pNC_male_max> pNCmale[l]; //Probability that a ZIKV infection becomes reported as a case with neurological complications
  
  real<lower=pS_female_min,upper=pS_female_max> pSfemale[l]; //Probability that a ZIKV infection is reported as a case to the surveillance system
  real<lower=pNC_female_min,upper=pNC_female_max> pNCfemale[l]; //Probability that a ZIKV infection becomes reported as a case with neurological complications
  
  real<lower=pS_male_min,upper=pS_male_max> pSmale_all; //overall risk that a ZIKV infection is reported as a case to the surveillance system (males)
  real<lower=pNC_male_min,upper=pNC_male_max> pNCmale_all; //overall risk that a ZIKV infection becomes reported as a case with neurological complications (males)
  
  real<lower=pS_female_min,upper=pS_female_max> pSfemale_all; //overall risk that a ZIKV infection is reported as a case to the surveillance system (females)
  real<lower=pNC_female_min,upper=pNC_female_max> pNCfemale_all; //overall risk that a ZIKV infection becomes reported as a case with neurological complications (females)
  
  real<lower=0,upper=1> pZ[l]; //Probability of Zika infection by city (males and females)
  real<lower=0,upper=1> pZall; // Overall probability of Zika infection (males and females)
}

model{
  
  //Hyper-priors (males)
  pS_male_min ~ uniform(0,1);
  pS_male_max ~ uniform(pS_male_min, 1);
  pNC_male_min ~ uniform(0,1);
  pNC_male_max ~ uniform(pNC_male_min, 1);
  
  //Hyper-priors (females)
  pS_female_min ~ uniform(0,1);
  pS_female_max ~ uniform(pS_female_min, 1);
  pNC_female_min ~ uniform(0,1);
  pNC_female_max ~ uniform(pNC_female_min, 1);
  
  //Priors 
  for(i in 1:l){
  //males
  pSmale[i] ~ uniform(pS_male_min, pS_male_max); // allows reporting rate of symptomatic Zika to vary by location
  pNCmale[i] ~ uniform(pNC_male_min, pNC_male_max); // allows reporting rate of neurological complications to vary by location
  //females
  pSfemale[i] ~ uniform(pS_female_min, pS_female_max); // allows reporting rate of symptomatic Zika to vary by location
  pNCfemale[i] ~ uniform(pNC_female_min, pNC_female_max); // allows reporting rate of neurological complications to vary by location
  //both
  pZ[i] ~ beta(alphaZ[i], gammaZ[i]); // representing possible ranges of attack rates from seroprevalence study
  }
  
  pSmale_all ~ uniform(pS_male_min, pS_male_max); //overall risk
  pNCmale_all ~ uniform(pNC_male_min, pNC_male_max); //overall risk
  
  pSfemale_all ~ uniform(pS_female_min, pS_female_max); //overall risk
  pNCfemale_all ~ uniform(pNC_female_min, pNC_female_max); //overall risk
  
  pZall ~ beta(1, 1); //assume same infection attack rate for both males and females
  
  //Model
  for (i in 1:l){
    //males
    Smale[i] ~ binomial(Nmale[i], pZ[i]*pSmale[i]); //Zika infections that give rise to suspected reported cases
    NCmale[i] ~ binomial(Nmale[i], pNCmale[i]*pZ[i]); //Neurological complications
    //females
    Sfemale[i] ~ binomial(Nfemale[i], pZ[i]*pSfemale[i]); //Zika infections that give rise to suspected reported cases
    NCfemale[i] ~ binomial(Nfemale[i], pNCfemale[i]*pZ[i]); //Neurological complications
  }
  
  Smale_all ~ binomial(Nmale_all, pSmale_all*pZall);
  NCmale_all ~ binomial(Nmale_all, pNCmale_all*pZall);
  
  Sfemale_all ~ binomial(Nfemale_all, pSfemale_all*pZall);
  NCfemale_all ~ binomial(Nfemale_all, pNCfemale_all*pZall);
 
}
