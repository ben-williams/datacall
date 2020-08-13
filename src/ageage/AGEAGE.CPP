#include <admodel.h>

  extern "C"  {
    void ad_boundf(int i);
  }
#include <ageage.htp>

model_data::model_data(int argc,char * argv[]) : ad_comm(argc,argv)
{
  nobs.allocate("nobs");
  age.allocate(1,nobs,"age");
  ape.allocate(1,nobs,"ape");
  n.allocate(1,nobs,"n");
}

void model_parameters::initializationfunction(void)
{
  sigma1.set_initial_value(0.5);
  sigma2.set_initial_value(5);
}

model_parameters::model_parameters(int sz,int argc,char * argv[]) : 
 model_data(argc,argv) , function_minimizer(sz)
{
  initializationfunction();
  sigma1.allocate(1,"sigma1");
  sigma2.allocate(2,"sigma2");
  Perc_Corr.allocate(1,nobs,"Perc_Corr");
  #ifndef NO_AD_INITIALIZE
    Perc_Corr.initialize();
  #endif
  Perc_Corr1.allocate(1,nobs,"Perc_Corr1");
  #ifndef NO_AD_INITIALIZE
    Perc_Corr1.initialize();
  #endif
  Perc_Corr2.allocate(1,nobs,"Perc_Corr2");
  #ifndef NO_AD_INITIALIZE
    Perc_Corr2.initialize();
  #endif
  Phat.allocate(1,nobs,"Phat");
  #ifndef NO_AD_INITIALIZE
    Phat.initialize();
  #endif
  sigma_a.allocate(1,nobs,"sigma_a");
  sigma_inc.allocate("sigma_inc");
  #ifndef NO_AD_INITIALIZE
  sigma_inc.initialize();
  #endif
  RSS.allocate(1,nobs,"RSS");
  #ifndef NO_AD_INITIALIZE
    RSS.initialize();
  #endif
  f.allocate("f");
}

void model_parameters::userfunction(void)
{
  get_A_SD_est();
  evaluate_the_objective_function();
}

void model_parameters::get_A_SD_est(void)
{
  sigma_inc = (sigma2-sigma1)/(age(nobs)-age(1));
  sigma_a(1) = sigma1;
  for (int i=2;i<=nobs;i++){
   sigma_a(i) = sigma1+(age(i)-age(1))*sigma_inc;}
  for (int i=1;i<=nobs;i++){
  Perc_Corr(i) = cumd_norm(0.5/sigma_a(i))-cumd_norm((-0.5)/sigma_a(i));
  Perc_Corr1(i) = cumd_norm((-0.5)/sigma_a(i))-cumd_norm((-1.5)/sigma_a(i));
  Perc_Corr2(i) = cumd_norm((-1.5)/sigma_a(i))-cumd_norm((-2.5)/sigma_a(i));}
  Phat = square(Perc_Corr)+2*square(Perc_Corr1)+2*square(Perc_Corr2);
}

void model_parameters::set_runtime(void)
{
  dvector temp("{1e-7,1e-7}");
  convergence_criteria.allocate(temp.indexmin(),temp.indexmax());
  convergence_criteria=temp;
}

void model_parameters::evaluate_the_objective_function(void)
{
  RSS.initialize();
  for (int i=1;i<=nobs;i++){
   if(n(i)>0){
   RSS(i) = pow(n(i),0.5)*square(Phat(i)-ape(i));}}
  f = sum(RSS);
}

void model_parameters::preliminary_calculations(void){
  admaster_slave_variable_interface(*this);
}

model_data::~model_data()
{}

model_parameters::~model_parameters()
{}

void model_parameters::report(void){}

void model_parameters::final_calcs(void){}

#ifdef _BORLANDC_
  extern unsigned _stklen=10000U;
#endif


#ifdef __ZTC__
  extern unsigned int _stack=10000U;
#endif

  long int arrmblsize=0;

int main(int argc,char * argv[])
{
    ad_set_new_handler();
  ad_exit=&ad_boundf;
    gradient_structure::set_NUM_DEPENDENT_VARIABLES(5000);
    gradient_structure::set_NO_DERIVATIVES();
    gradient_structure::set_YES_SAVE_VARIABLES_VALUES();
  #if defined(__GNUDOS__) || defined(DOS386) || defined(__DPMI32__)  || \
     defined(__MSVC32__)
      if (!arrmblsize) arrmblsize=150000;
  #else
      if (!arrmblsize) arrmblsize=25000;
  #endif
    model_parameters mp(arrmblsize,argc,argv);
    mp.iprint=10;
    mp.preliminary_calculations();
    mp.computations(argc,argv);
    return 0;
}

extern "C"  {
  void ad_boundf(int i)
  {
    // so we can stop here
    exit(i);
  }
}
