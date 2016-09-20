# --------------------------------------------------
# Carbon Cycle Exercises.
# Computer Lab.
# Author: Benjamin Stocker <beni@climate.unibe.ch>
# Responsible: Fortunat Joos <joos@climate.unibe.ch>
# Feb. 2012
# --------------------------------------------------


# 2-Box Model Atmosphere-Ocean

# Set parameters
lsim <- 1000 # length of simulation
#A_oc <- 3.6e+14
k_ex <- 60/270

buffer <- 10
gt2ppm <- 2.123   # conversion factor from GtC to ppmv

# 1000 GtC CO2 are emitted in year 100 into the atmosphere. 
# Plot the evolution of the atmospheric perturbation
# (in units of ppm) over the course of 1000 years.

eC <- array(0,c(lsim))

## # 2.a) Pulse emission in year 100
## eC[100] <- 1000
# ----------------------------------------------------------------

# 2.b) Step change in emissions (from 0 to 10 GtC/yr after yr 100)
# eC[101:lsim] <- 10 
# ----------------------------------------------------------------

# 2.c) Linearly increasing emissions (from 0.02 in year 1 
# to 20 GtC in year 1000)
# eC <- seq(0.02,20,0.02)
# ----------------------------------------------------------------

# It is convenient to treat variables that are to be written
# into output separately, while the variable that changes
# its value from time step to time step should not have a 
# time dimension, but changes its value with each time step.
pC_a_out <- array(NA,c(lsim)) # This is the output variable for atmospheric CO2
pC_o_out <- array(NA,c(lsim)) # This is the output variable for atmospheric CO2

# Initialize variables
pC_a <- 270
pC_o_init <- 270
C_o_init<- 5000 # initial ocean C-pool, in GtC
C_a  <- 270*gt2ppm
C_o <- C_o_init
pC_o <- pC_o_init

for (yr in seq(lsim)) {
	
	# Add emissions to atmosphere
	C_a <- C_a + eC[yr]
	
	# Update atmospheric CO2 partial pressure
	pC_a <- C_a/gt2ppm
	
	# Flux atmosphere -> ocean (lecture notes Eq.5.46)
	f_ao <- k_ex*(pC_a - pC_o)
	
	# Update inventories
	C_a <- C_a - f_ao
	C_o <- C_o + f_ao
	
	# Update oceanic CO2 partial pressure. The partial 
	# pressure increase is scaled by the buffer factor
	# (lecture notes Eq.5.37)
	pC_o <- pC_o_init + buffer*(C_o - C_o_init)/C_o_init*pC_o_init
	
	# Copy atmospheric C inventory of this year to output
	pC_a_out[yr] <- pC_a
	pC_o_out[yr] <- pC_o
	
      }



