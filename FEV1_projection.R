
fev1_projection <- function(fev1_0, int_effect, tio="No"){


  x<-c(0:11)

  beta_0<-2.7594
  beta_t<--0.04314
  beta_t2<--0.00093
  v_0<-0.3988
  cov1<--0.00048
  v_t<-0.000941
  v_e<-0.01724

  tioBefore <- 0.015
  tioAfter <- 0.022


  fev1_avg <- c()
  vari <- c()
  obs <- fev1_0

  for (i in 1:11){

    t1 <- i
    beta_t_x <- 0;
    beta_x_p <- 0;
    beta_x <- int_effect


    unconditional_mu <- c(
      mu_f=beta_0 + beta_x + beta_t_x + beta_t*t1 + beta_t2*t1*t1,
      mu_0=beta_0 + beta_x_p
    )

    v_t_f<- v_0 + t1^2*v_t + 2*t1*cov1
    v_t_0<- v_0 + v_e
    cov_f_0<- v_0 + t1*cov1
    cov_mat<- rbind(c(v_t_f,cov_f_0),c(cov_f_0,v_t_0))

    sigma_11<-as.matrix(cov_mat[1,1])
    sigma_12<-as.matrix(t(cov_mat[1,-1]))
    sigma_21<-as.matrix(cov_mat[-1,1])
    sigma_22<-as.matrix(cov_mat[-1,-1])

    fev1_avg[i]<-unconditional_mu[1] + sigma_12%*%solve(sigma_22)%*%(obs-unconditional_mu[-1])
    vari[i]<-sigma_11 - sigma_12%*%solve(sigma_22)%*%sigma_21
    if(tio=="Yes"){
      fev1_avg[i] <- fev1_avg[i] + tioBefore*i
    }
  }


  fev1_avg<-c(fev1_0, fev1_avg)

  vari<-c(0,vari)

  fev1_up<-fev1_avg+1.96*sqrt(vari)
  fev1_low<-fev1_avg-1.96*sqrt(vari)

  df<-data.frame(x, y=fev1_avg, vari, fev1_low, fev1_up)
  names(df) <- c("Time", "FEV1", "vari", "FEV1_lower", "FEV1_upper")

  cv1<-sqrt(vari[2:12])/(fev1_avg[2:12]-fev1_0)
  aa1<-rbind(fev1_avg[2:12], fev1_up[2:12], fev1_low[2:12], round(abs(cv1)*100,0))

  n_mean1<-(fev1_avg[12]-fev1_0)/11*1000
  n_sd1<-((fev1_avg[12]-fev1_0)/11-(fev1_low[12]-fev1_0)/11)/1.96*1000
  bb1<-data.frame(round(pnorm(-40, n_mean1, n_sd1)*100,0))

  df_aa1 <- list("df"=df, "aa1"=aa1, "bb1"=bb1, "options"=1)
  print(df_aa1)
  return(df_aa1)


}

fev1_projection2 <- function(fev1_0, int_effect, sex, smoking, age, weight, height, oco, tio="No"){

  x<-c(0:11)

  if (sex=="male"){
    gender<-1
  } else if (sex=="female"){
    gender<-0
  }

  if (smoking=="Smoker"){
    smo<-1
    int<-0
  } else if (smoking=="Sustained quitter"){
    smo<-0
    int<-0
  }

  tioBefore <- 0.015
  tioAfter <- 0.022

  beta_0<-1.4212
  beta_t<--0.1779
  beta_t2<--0.00044
  v_0<-0.09728
  cov1<-0.000597
  v_t<-0.000749
  v_e<-0.01703

  fev1_avg <- c()
  vari <- c()

  obs<-fev1_0


  for (i in 1:11) {

    t1 <- i

    beta_x <- -0.00519*age + 0.4625*gender + -0.00011*weight + -1.7603*height + 1.8931*height*height +
      -0.07722*smo + -0.04131*int + 0.002613*oco + -0.00820*age*height*height +
      0.02735*(1-smo) + int_effect;

    beta_t_x <- 0.002313*age*t1 + -0.00886*gender*t1 + 0.000149*weight*t1 +
      0.07413*height*t1 + 0.01139*height*height*t1 + -0.02579*smo*t1 + -0.01002*int*t1 + 0.000195*oco*t1 +
      -0.00092*age*height*height*t1;


    beta_x_p <- -0.00519*age + 0.4625*gender + -0.00011*weight + -1.7603*height + 1.8931*height*height +
      -0.07722*(1) + -0.04131*(0) + 0.002613*oco + -0.00820*age*height*height;

    beta_t_x_p <- 0.002313*age*(-1) + -0.00886*gender*(-1) + 0.000149*weight*(-1) +
      0.07413*height*(-1) + 0.01139*height*height*(-1) + -0.02579*(1)*(-1) + -0.01002*(0)*(-1) + 0.000195*oco*(-1) +
      -0.00092*age*height*height*(-1);

    unconditional_mu <- c(
      mu_f=beta_0 + beta_x + beta_t_x + beta_t*t1 + beta_t2*t1*t1,
      mu_0=beta_0 + beta_x_p
    )

    v_t_f<- v_0 + t1^2*v_t + 2*t1*cov1
    v_t_0<- v_0 + v_e
    cov_f_0<- v_0 + t1*cov1
    cov_mat<- rbind(c(v_t_f,cov_f_0),c(cov_f_0,v_t_0))

    sigma_11<-as.matrix(cov_mat[1,1])
    sigma_12<-as.matrix(t(cov_mat[1,-1]))
    sigma_21<-as.matrix(cov_mat[-1,1])
    sigma_22<-as.matrix(cov_mat[-1,-1])

    fev1_avg[i]<-unconditional_mu[1] + sigma_12%*%solve(sigma_22)%*%(obs-unconditional_mu[-1])
    vari[i]<-sigma_11 - sigma_12%*%solve(sigma_22)%*%sigma_21
    if(tio=="Yes"){
      fev1_avg[i] <- fev1_avg[i] + tioBefore*i
    }
  }


  fev1_avg<-c(fev1_0, fev1_avg)
  vari<-c(0,vari)

  fev1_up<-fev1_avg+1.96*sqrt(vari)
  fev1_low<-fev1_avg-1.96*sqrt(vari)

  df<-data.frame(x, y=fev1_avg, vari, fev1_low, fev1_up)
  names(df) <- c("Time", "FEV1", "vari", "FEV1_lower", "FEV1_upper")

  cv2<-sqrt(vari[2:12])/(fev1_avg[2:12]-fev1_0)
  aa2<-rbind(fev1_avg[2:12], fev1_up[2:12], fev1_low[2:12], round(abs(cv2)*100,0))

  n_mean2<-(fev1_avg[12]-fev1_0)/11*1000
  n_sd2<-((fev1_avg[12]-fev1_0)/11-(fev1_low[12]-fev1_0)/11)/1.96*1000

  bb2<-data.frame(round(pnorm(-40, n_mean2, n_sd2)*100,0))

  df_aa2 <- list("df"=df, "aa1"=aa2, "bb1"=bb2, "options"=2)
  print(df_aa2)
  return(df_aa2)

}

fev1_projection3 <- function(fev1_0, int_effect, sex, smoking, age, weight, height, tio="No"){

  print("kinda workings")
  x<-c(0:11)

  if (sex=="male"){
    gender<-1
  } else if (sex=="female"){
    gender<-0
  }

  if (smoking=="Smoker"){
    smo<-1
    int<-0
  } else if (smoking=="Sustained quitter"){
    smo<-0
    int<-0
  }

  tioBefore <- 0.015
  tioAfter <- 0.022

  beta_0<-1.4258
  beta_t<--0.1795
  beta_t2<--0.00044
  v_0<-0.1008
  cov1<-0.000873
  v_t<-0.000769
  v_e<-0.01703

  fev1_avg <- c()
  vari <- c()

  obs<-fev1_0

  for (i in 1:11) {

    t1 <- i

    beta_x <- -0.00482*age + 0.4828*gender + -0.00041*weight + -1.8759*height + 1.9527*height*height +
      -0.07634*smo + -0.04159*int + -0.00837*age*height*height +
      0.02830*(1-smo) + int_effect;

    beta_t_x <- 0.002358*age*t1 + -0.00739*gender*t1 + 0.000127*weight*t1 +
      0.06680*height*t1 + 0.01565*height*height*t1 + -0.02552*smo*t1 + -0.01023*int*t1 +
      -0.00094*age*height*height*t1;


    beta_x_p <- -0.00482*age + 0.4828*gender + -0.00041*weight + -1.8759*height + 1.9527*height*height +
      -0.07634*(1) + -0.04159*(0) + -0.00837*age*height*height;

    beta_t_x_p <- 0.002358*age*(-1) + -0.00739*gender*(-1) + 0.000127*weight*(-1) +
      0.06680*height*(-1) + 0.01565*height*height*(-1) + -0.02552*(1)*(-1) + -0.01023*(0)*(-1) +
      -0.00094*age*height*height*(-1);


    unconditional_mu <- c(
      mu_f=beta_0 + beta_x + beta_t_x + beta_t*t1 + beta_t2*t1*t1,
      mu_0=beta_0 + beta_x_p
    )

    v_t_f<- v_0 + t1^2*v_t + 2*t1*cov1
    v_t_0<- v_0 + v_e
    cov_f_0<- v_0 + t1*cov1
    cov_mat<- rbind(c(v_t_f,cov_f_0),c(cov_f_0,v_t_0))

    sigma_11<-as.matrix(cov_mat[1,1])
    sigma_12<-as.matrix(t(cov_mat[1,-1]))
    sigma_21<-as.matrix(cov_mat[-1,1])
    sigma_22<-as.matrix(cov_mat[-1,-1])

    fev1_avg[i]<-unconditional_mu[1] + sigma_12%*%solve(sigma_22)%*%(obs-unconditional_mu[-1])
    vari[i]<-sigma_11 - sigma_12%*%solve(sigma_22)%*%sigma_21
    if(tio=="Yes"){
      fev1_avg[i] <- fev1_avg[i] + tioBefore*i
    }
  }


  fev1_avg<-c(fev1_0, fev1_avg)
  vari<-c(0,vari)

  fev1_up<-fev1_avg+1.96*sqrt(vari)
  fev1_low<-fev1_avg-1.96*sqrt(vari)

  df <-data.frame(x, y=fev1_avg, vari, fev1_low, fev1_up)
  names(df) <- c("Time", "FEV1", "vari", "FEV1_lower", "FEV1_upper")

  cv3 <-sqrt(vari[2:12])/(fev1_avg[2:12]-fev1_0)
  aa3 <-rbind(fev1_avg[2:12], fev1_up[2:12], fev1_low[2:12], round(abs(cv3)*100,0))

  n_mean3 <-(fev1_avg[12]-fev1_0)/11*1000
  n_sd3 <-((fev1_avg[12]-fev1_0)/11-(fev1_low[12]-fev1_0)/11)/1.96*1000
  bb3 <-data.frame(round(pnorm(-40, n_mean3, n_sd3)*100,0))

  df_aa3 <- list("df"=df, "aa1"=aa3, "bb1"=bb3, "options"=3)
  print(df_aa3)
  return(df_aa3)

}

fev1_projection4 <- function(fev1_0, fev1_prev, int_effect, sex, smoking, age, weight, height, oco, tio="No"){

  x<-c(-1:11)
  print(x)
  print("Test")

  if (sex=="male"){
    gender<-1
  } else if (sex=="female"){
    gender<-0
  }

  if (smoking=="Smoker"){
    smo<-1
    int<-0
  } else if (smoking=="Sustained quitter"){
    smo<-0
    int<-0
  }

  tioBefore <- 0.015
  tioAfter <- 0.022

  beta_0<-1.4212
  beta_t<--0.1779
  beta_t2<--0.00044
  v_0<-0.09728
  cov1<-0.000597
  v_t<-0.000749
  v_e<-0.01703

  fev1_avg <- c()
  vari <- c()

  obs<-c(fev1_prev,fev1_0)

  for (i in 1:11)
  {
    t1 <- i

    beta_x <- -0.00519*age + 0.4625*gender + -0.00011*weight + -1.7603*height + 1.8931*height*height +
      -0.07722*smo + -0.04131*int + 0.002613*oco + -0.00820*age*height*height +
      0.02735*(1-smo) + int_effect;

    beta_t_x <- 0.002313*age*t1 + -0.00886*gender*t1 + 0.000149*weight*t1 +
      0.07413*height*t1 + 0.01139*height*height*t1 + -0.02579*smo*t1 + -0.01002*int*t1 + 0.000195*oco*t1 +
      -0.00092*age*height*height*t1;


    beta_x_p <- -0.00519*age + 0.4625*gender + -0.00011*weight + -1.7603*height + 1.8931*height*height +
      -0.07722*(1) + -0.04131*(0) + 0.002613*oco + -0.00820*age*height*height;

    beta_t_x_p <- 0.002313*age*(-1) + -0.00886*gender*(-1) + 0.000149*weight*(-1) +
      0.07413*height*(-1) + 0.01139*height*height*(-1) + -0.02579*(1)*(-1) + -0.01002*(0)*(-1) + 0.000195*oco*(-1) +
      -0.00092*age*height*height*(-1);


    unconditional_mu <- c(
      mu_f=beta_0 + beta_x + beta_t_x + beta_t*t1 + beta_t2*t1*t1,
      mu_0=beta_0 + beta_x_p
    )

    v_t_f<- v_0 + t1^2*v_t + 2*t1*cov1
    v_t_0<- v_0 + v_e
    cov_f_0<- v_0 + t1*cov1
    cov_mat<- rbind(c(v_t_f,cov_f_0),c(cov_f_0,v_t_0))

    unconditional_mu<-c(unconditional_mu, mu_p=beta_0 + beta_x_p + beta_t_x_p + beta_t*(-1) + beta_t2*(-1)*(-1))
    v_t_p<-v_0 + v_t + 2*(-1)*cov1 + v_e
    cov_0_p<-v_0 + (-1)*cov1
    cov_f_p<-v_0 + t1*cov1 + (-1)*cov1 + (-1)*t1*v_t
    cov_mat<-rbind(cbind(cov_mat,c(cov_f_p,cov_0_p)),c(cov_f_p,cov_0_p,v_t_p))

    sigma_11<-as.matrix(cov_mat[1,1])
    sigma_12<-as.matrix(t(cov_mat[1,-1]))
    sigma_21<-as.matrix(cov_mat[-1,1])
    sigma_22<-as.matrix(cov_mat[-1,-1])

    fev1_avg[i]<-unconditional_mu[1] + sigma_12%*%solve(sigma_22)%*%(obs-unconditional_mu[-1])
    vari[i]<-sigma_11 - sigma_12%*%solve(sigma_22)%*%sigma_21
    if(tio=="Yes"){
      fev1_avg[i] <- fev1_avg[i] + tioBefore*i
    }
  }


  fev1_avg<-c(fev1_prev, fev1_0, fev1_avg)
  vari<-c(0,0,vari)

  fev1_up<-fev1_avg+1.96*sqrt(vari)
  fev1_low<-fev1_avg-1.96*sqrt(vari)

  df<-data.frame(x, y=fev1_avg, fev1_low, fev1_up)
  names(df) <- c("Time", "FEV1", "FEV1_lower", "FEV1_upper")

  cv4<-sqrt(vari[3:13])/(fev1_avg[3:13]-fev1_prev)
  aa4<-rbind(fev1_avg[3:13], fev1_up[3:13], fev1_low[3:13], round(abs(cv4)*100,0))

  n_mean4<-(fev1_avg[13]-fev1_prev)/12*1000
  n_sd4<-((fev1_avg[13]-fev1_prev)/12-(fev1_low[13]-fev1_prev)/12)/1.96*1000
  bb4<-data.frame(round(pnorm(-40, n_mean4, n_sd4)*100,0))

  df_aa4 <- list("df"=df, "aa1"=aa4, "bb1"=bb4, "options"=4)
  print(df_aa4)
  return(df_aa4)
}
