# --------------------------------------------------
# Carbon Cycle Exercises.
# Computer Lab.
# Farquhar photosynthesis model.
# Author: Benjamin Stocker <beni@climate.unibe.ch>
# Responsible: Fortunat Joos <joos@climate.unibe.ch>
# March 2012
# --------------------------------------------------

# GENERAL STRUCTURE OF PROGRAM
#A. Define functions
#	1. ftemp
#	2. ftemp of J_max
#       3. ftemp of lambda
#	4. Assimulation of RuP2-saturated case
#	5. Assimulation of RuP2-limited case
#	6. actual assimilation as the minimum of 3. and 4.
#	
#B. plotting of functions for a certain domain of independent variables (temperature, pCO2, and PAR)	


#----+
# A  |
#----+

# //////////////////////////////////////////////////
# 0. PARAMETERS
# --------------------------------------------------
# Parameters at 25 degC are defined as global variables
# Global variables are defined inside all functions.
# Note that variables inside a function definition are
# not defined outside and vice-versa.

# Universal gas constant [j/mol k]
assign("R", 8.314, envir = .GlobalEnv)

# Define parameter values at 25 degC
assign("Kc25", 460, envir = .GlobalEnv)
assign("Ko25", 330, envir = .GlobalEnv)
assign("Rd25", 1, envir = .GlobalEnv)
assign("lambda25", 31, envir = .GlobalEnv)

# Define activation energy
assign("Eactiv_Kc", 59356, envir = .GlobalEnv)
assign("Eactiv_Ko", 35948, envir = .GlobalEnv)
assign("Eactiv_Rd", 66405, envir = .GlobalEnv)


# //////////////////////////////////////////////////
# 1. FTEMP
# --------------------------------------------------
# Define function for temperature modification of parameters
ftemp <- function(temp, E_activ){
		
	# Eq. 3.61 in lecture notes
	ftemp <- exp((temp-298)*E_activ/(298*R*temp))
	
	return(ftemp)
	
	}
	
# //////////////////////////////////////////////////
# 2. FTEMP OF J_MAX
# --------------------------------------------------
# Define ftemp for J_max
ftemp_jmax <- function(temp){
	
	E <- 37000
	S <- 710
	H <- 220000
	
	# Eq. 3.62 in lecture notes
	ftemp_jmax <- (exp(-E/(R*temp)) / ( 1 + exp((S*temp-H)/(R*temp)))) / (exp(-E/(R*298)) / ( 1 + exp((S*298-H)/(R*298))))
	
	return(ftemp_jmax)
	
	}

# //////////////////////////////////////////////////
# 3. FTEMP OF LAMBDA-STAR
# --------------------------------------------------
# Define temperature dependence of lambda-star.
# Compare with Eq.3.50 in lecture notes where only
# Kc and Ko are temperature-dependent.
ftemp_lambda <- function(temp){

  # Calculate "parameter" values for actual temperature
  Kc <- Kc25 * ftemp(temp,Eactiv_Kc)
  Ko <- Ko25 * ftemp(temp,Eactiv_Ko)

  ftemp_lambda <- Kc/Ko

  return(ftemp_lambda)
}

	
# //////////////////////////////////////////////////
# 4. A_SAT FUNCTION
# --------------------------------------------------
# Define function for RuP2-saturated assimilation
Asat <- function(pCO2, temp, pO2){

	# Define parameter values at 25 degC
	Vc_max25 <- 98

	# Define activation energy
	Eactiv_Vcmax <- 58520

	# Calculate "parameter" values for actual temperature
	Kc       <- Kc25    *ftemp(temp,Eactiv_Kc)
	Ko       <- Ko25    *ftemp(temp,Eactiv_Ko)
	Vc_max   <- Vc_max25*ftemp(temp,Eactiv_Vcmax)
	Rd       <- Rd25    *ftemp(temp,Eactiv_Rd)
	lambda   <- lambda25*ftemp_lambda(temp)
	
	Asat <- Vc_max * (pCO2-lambda)/(pCO2+Kc*(1+pO2/Ko)) - Rd
	
	return(Asat)
	
	}


# //////////////////////////////////////////////////
# 5. A_LIM FUNCTION
# --------------------------------------------------
# Define function for RuP2-limited assimilation	
Alim <- function(pCO2, temp, PAR){
	
	# Define parameter values at 25 degC
	Jmax25   <- 210

	# Calculate "parameter" values for actual temperature
	Kc       <- Kc25 * ftemp(temp,Eactiv_Kc)
	Ko       <- Ko25 * ftemp(temp,Eactiv_Ko)
	Rd       <- Rd25 * ftemp(temp,Eactiv_Rd)
	lambda   <- lambda25*ftemp_lambda(temp)
	
	Jmax <- Jmax25*ftemp_jmax(temp)

	Alim     <- Jmax*PAR/(PAR+2.1*Jmax) * 
				(pCO2-lambda)/(4.5*pCO2+10.5*lambda) - Rd

	return(Alim)	
	
	}
	
# //////////////////////////////////////////////////
# 6. ASSIM FUNCTION
# --------------------------------------------------
# Define function for actual assimilation.
Assim <- function(pCO2, temp, pO2, PAR){
        Assim <- array(NA,c(length(pCO2)))
        for (i in seq(length(pCO2))){
          Assim[i] <- min(
                          Alim(pCO2, temp, PAR)[i],
                          Asat(pCO2, temp, pO2)[i]
                          )
        }
	return(Assim)
	}


#----+
# B  |
#----+
	
# //////////////////////////////////////////////////
# PLOT
# --------------------------------------------------
## Visualize temperature dependence of Vcmax and Jmax.
## Compare with Fig. 3.26 in lecture notes.
# --------------------------------------------------
Eactiv_Vcmax <- 58520
Vc_max25 <- 98
Jmax25   <- 210
temp <- seq(from=273,to=323,by=0.1)

plot(
     temp-273,
     Vc_max25*ftemp(temp,Eactiv_Vcmax),
     type='l',
     xlim=c(0,50),
     ylim=c(0,600)
     )
par(new=TRUE)
plot(
     temp-273,
     Jmax25*ftemp_jmax(temp),
     type='l',
     xlim=c(0,50),
     ylim=c(0,600)
     )

## Visualize CO2-dependence of Assimilation.
## Compare with Fig. 3.24 in lecture notes.
# --------------------------------------------------
pCO2 <- seq(1000)
temp <- 290
pO2 <- 210
PAR  <- 1000 

plot(
     pCO2,
     Asat(pCO2, temp, pO2),
     type='l',
     xlim=c(0,1000),
     ylim=c(0,35)
     )
par(new=TRUE)
plot(
     pCO2,
     Alim(pCO2, temp, PAR),
     type='l',
     xlim=c(0,1000),
     ylim=c(0,35)
     )
par(new=TRUE)
plot(
     pCO2,
     Assim(pCO2, temp, pO2, PAR),
     type='l',
     lty=1,
     col="red",
     xlim=c(0,1000),
     ylim=c(0,35)
     )
par(new=TRUE)
plot(
     pCO2,
     Assim(pCO2, temp, pO2, 2000),
     type='l',
     lty=2,
     col="red",
     xlim=c(0,1000),
     ylim=c(0,35)
     )


