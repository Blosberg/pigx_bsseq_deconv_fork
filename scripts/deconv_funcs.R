##== deconv_funcs.R -functions specific to deconvolution ======
# ---last updated Nov. 8 0:54
# Brendan Osberg, MDC, Berlin 2017
#=======================================================================
#--- get cell fractions using the lm method: (from published papers) ----
get_cell_fracs_RLR <- function(target_profile, 
                               Sigmat, 
                               deconv_params )
{
  lm.out <- lm( target_profile ~ -1+Sigmat_sighit[,1] + Sigmat_sighit[,2] + Sigmat_sighit[,3] + Sigmat_sighit[,4] )
  #---- only valid for NCT=4
  
  raw_pars=lm.out$coefficients
  
  lm.out$coefficients[lm.out$coefficients < 0]=0  #==== any negative coef's are set to zero
  
  value=norm(target_profile - Sigmat %*% lm.out$coefficients, "F")
  normconst = sum( lm.out$coefficients )
  
  if( (deconv_params$maxbound && deconv_params$minbound) || (deconv_params$maxbound && normconst>1 ) || (deconv_params$minbound && normconst<1 ) )
  {
    par=(1.0/normconst)*lm.out$coefficients
  }
  else
  {
    par=lm.out$coefficients
  }
  
  return( list( "par" = par, "value"=value, "raw_pars" = raw_pars ) )
}

#=======================================================================
#--- get cell fractions using the constrained Nelder Mead approach: ----
get_cell_fracs_NMconstr <- function( target_sample     = stop("This must be defined"),  
                                     refdat_in         = stop("This must be defined"),
                                     deconv_params     = stop("This must be defined"),
                                     PARTOL = 0.0001, #==== tolerance on the change in parameter elements
                                     VALTOL = 0.0001  #==== tolerance on the change in value of the function
                                    )
{
  NCT= length(refdat_in$CT_list )
  
  conditions <- deconv_params$conditions 
  ui_COND = conditions$ui_COND; 
  ci_COND = conditions$ci_COND; 

  
  w       = deconv_params$init_guess 
  temp_old = constrOptim(w, function(w){ calc_costfunc( w, 
                                                        target_sample_in    = target_sample, 
                                                        refdat        = refdat_in, 
                                                        deconv_params = deconv_params) 
                                        },
                          ui=ui_COND, 
                          ci=ci_COND, 
                          mu = deconv_params$mu, 
                          control = list(), 
                          method = "Nelder-Mead"
                         ) 
  
  w        = temp_old$par 
  temp_new = constrOptim(w, function(w){ calc_costfunc( w, 
                                                        target_sample_in = target_sample, 
                                                        refdat = refdat_in, 
                                                        deconv_params = deconv_params) },
                         ui=ui_COND, ci=ci_COND, mu = deconv_params$mu, control = list(), method = "Nelder-Mead") 
  temp_new$value = rbind(temp_new$value, temp_old$value)
  w              = temp_new$par
  
  if( temp_new$value[1] > temp_old$value ) 
    { stop("Function value increasing on first iteration. Something's gone wrong.")}
  
  num_iter=1
  
  while( max( abs( temp_new$par -temp_old$par) ) > PARTOL ||  ( (temp_new$value[2,1] - temp_new$value[1,1])/temp_new$value[1] ) > VALTOL )
    {
    temp_old  = temp_new
    
    temp_new  = constrOptim(w, function(w){ calc_costfunc(  w, 
                                                            target_sample_in = target_sample, 
                                                            refdat           = refdat_in, 
                                                            deconv_params    = deconv_params) },
                                 ui=ui_COND, ci=ci_COND, mu = deconv_params$mu, control = list(), method = "Nelder-Mead") 
    
    temp_new$value = rbind(temp_new$value, temp_old$value)
    
    if(temp_new$value[1,1] > temp_new$value[2,1] )
      {stop("Function value increasing between iterations. Something's gone wrong.")}
    
    num_iter = num_iter+1
  }
  
  temp_new$num_iter = num_iter
  
  return(temp_new)
}

#==================================================================
#--- arrays to impose constraints on functional optimization ----
get_conditions <- function( NCT          = 4, 
                            impose_le_1  = TRUE,
                            impose_ge_1  = TRUE, 
                            epsilon      = 0.01 ) # DEFINITION: return ui_COND, ci_COND, w, s.t. ui_COND * w >= ci_COND
{  
  
  if( !impose_le_1 && !impose_ge_1 ) #--- impose no constraints on the sum of w --only that each element is positive.
  {print("getting conditions without any imposition on sum")
    
    ui_COND         <-  matrix(0,NCT,NCT)
    ci_COND         <-  matrix(0,NCT,1)
    w <- matrix(1/NCT-(0.1*epsilon/NCT),NCT,1) #--- INITIAL CONDITION OF CELL TYPE FRACTIONS ------
    
    diag(ui_COND)   <-  1
  }
  else if( impose_le_1 && !impose_ge_1 ) #--- impose constraint that sum(w)<=1 does not exceed one.
  { print("getting conditions with upper bound sum <=1+epsilon, but no lower bound")
    
    ui_COND         <-  matrix(0,NCT+1,NCT)
    ci_COND         <-  matrix(0,NCT+1,1)   #--- CONDITION MATRIX AND VECTOR.
    w <- matrix(1/NCT-(0.1*epsilon/NCT),NCT,1)     #--- INITIAL CONDITION OF CELL TYPE FRACTIONS ------
    
    diag(ui_COND)   <-  1
    ui_COND[NCT+1,] <- -1
    
    #--- NOW WE IMPOSE THAT ALL FRACTIONS ARE POSITIVE AND SUM TO <= UNITY.
    ci_COND[NCT+1] <- -1-epsilon
  }
  else if( impose_le_1 && impose_ge_1 ) #--- impose BOTH constraints that 1-eps <= sum(w) <= 1+eps --that sum is within eps of one
  { print("getting conditions with upper and lower bounds 1-epsilon <= sum(w) <= 1+epsilon")
    
    ui_COND         <-  matrix(0,NCT+2,NCT)
    ci_COND         <-  matrix(0,NCT+2,1)   #--- CONDITION MATRIX AND VECTOR.
    w <- matrix(1/NCT-(0.1*epsilon/NCT),NCT,1)     #--- INITIAL CONDITION OF CELL TYPE FRACTIONS ------
    
    diag(ui_COND)   <-  1
    ui_COND[NCT+1,] <- -1
    ui_COND[NCT+2,] <-  1
    
    #--- NOW WE IMPOSE THAT ALL FRACTIONS ARE POSITIVE AND SUM TO <= UNITY.
    ci_COND[NCT+1] <- -1-epsilon
    ci_COND[NCT+2] <-  1-epsilon
  }
  else if( !impose_le_1 && impose_ge_1 ) #--- impose only  1-eps <= sum(w) ; no reason to ever do this. 
  {
    stop("prompted for conditions with lower bound on sum, but no upper bound. This doesn't make sense. Exiting.")
  }
  
  #---------------------------------------------------------------
  
  result <- list("ui_COND" = ui_COND, "ci_COND" = ci_COND, "w" = w)
  
  return( result )
}

#==================================================================
#--- DEFINE RESIDUAL FUNCTION TO SEEK A MINIMUM IN ----

calc_costfunc <- function( w                = stop("This must be defined"), 
                           target_sample_in = stop("This must be defined"), 
                           refdat           = stop("This must be defined"),
                           deconv_params    = stop("This must be defined")
                           ) 
{   
  
  #---------------------------------------------------------------------------------------------------------------
  if ( deconv_params$costfunc == "LSQ" ) #---------------  LEAST SQUARES DIFFERENCE IN AVG. METH -----------------
  {
    y_calc   = refdat$Sigmat  %*% w   
    Res      = ( y_calc - target_sample_in ) # target_sample_in$meth should already be filtered to the RsOI that passed; 
                                             # if dimensions don't agree we'll get a "non-conformable arrays" error.
  
    result = sqrt( sum( Res*Res ) )
    #-------------------------------------------------------------------------------------------------------------
  } else if ( deconv_params$costfunc == "minz")#-----------   LEAST SUM OF Z VALUES
  {
    p1 = refdat$mu_ref[ deconv_params$refROI_selecttion, ] %*% w
    n1 = p1*(1-p1) / ( refdat$G_ref[ deconv_params$refROI_selecttion, ] %*%  (w^2) )
    # n1 = 1/( (1/refdat$covmat[ deconv_params$refROI_selecttion, ]) %*% w )
    
    p2 = target_sample_in$meth
    n2 = target_sample_in$cov 

    phat=( (n1*p1+n2*p2)/(n1+n2))
    
    z = (p1-p2)^2/( phat*(1-phat)*(1/n1 + 1/n2) )
     
    result = sum(z^2)
  } else { stop( paste( "ERROR, invalid method: ", deconv_params$costfunc, " passed to calc_costfunc. Terminating." ) ) }
  #---------------------------------------------------------------------------------------------------------------
  
  if( result <= 0)
    { print(" WARNING: in calc_costfunc, result is <= 0 ") }
  
  return(result)
}
