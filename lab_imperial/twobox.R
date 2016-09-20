## --------------------------------------------------
## Introduction to the (terrestrial) Carbon Cycle
## Computer Lab.
## Author: Benjamin Stocker <b.stocker@imperial.ac.uk>
## Responsible: Colin Prentice <c.prentice@imperial.ac.uk>
## Oct. 2015
## --------------------------------------------------

## ///////////////////////////////////////////////////
## 2-Box Model Atmosphere-Ocean
## --------------------------------------------------
twobox <- function( pC_a_init, pC_o_init, C_o_init, co2_emissions ) {

  ## ------------------------------------------------
  ## pC_a_init: initial atmospheric CO2 (ppm)
  ## pC_o_init: initial oceanic CO2 (ppm)
  ## co2_emissions: CO2 emissions (GtC a-1)
  ## ------------------------------------------------

  ## --------------------------------------------------
  ## Set parameters
  ## --------------------------------------------------
  gt2ppm <- 2.123   # conversion factor from GtC to ppmv

  ## determine integration length (number of time steps) from length of 'co2_emissions'
  len <- length( co2_emissions )

  ## Define an exchange coefficient accounting implicitly
  ## also for the ocean surface area. Gross exchange flux
  ## is defined as 
  ## f_gross = k_ex * pCO2 
  ## and should amount to 60 GtC at 270 ppm. Therefore, ...
  k_ex <- 6/270

  ## Buffer factor defined as Eq.2
  buffer <- 10

  ## copy initial pool size to first element in output time series
  cpool <- cpool0


  ## --------------------------------------------------
  ## Variable initialisations (independent of problem number)
  ## --------------------------------------------------
  ## It is convenient to treat variables that are to be written
  ## into output separately, while the variable that changes
  ## its value from time step to time step should not have a 
  ## time dimension, but changes its value with each time step.
  pC_a_out <- rep( NA, len )  # This is the output variable for atmospheric CO2
  pC_o_out <- rep( NA, len )  # This is the output variable for oceanic CO2
  C_o_out <- rep( NA, len )   # This is the output variable for oceanic CO2

  ## Additionally initialise the continuously updated 
  ## oceanic CO2 values. 
  C_o       <- C_o_init
  pC_o      <- pC_o_init
  pC_a      <- pC_a_init
  C_a       <- pC_a_init*gt2ppm  # initial atmos. CO2 [GtC=PgC]


  ## --------------------------------------------------
  ## Main part: integration of 2 box model
  ## --------------------------------------------------
  for (yr in seq(len)) {
    
    ## Copy atmospheric C inventory of this year to output
    pC_a_out[yr] <- pC_a
    pC_o_out[yr] <- pC_o
    C_o_out[yr]  <- C_o

    ## Add emissions to atmosphere
    C_a <- C_a + co2_emissions[yr]
    
    ## Update atmospheric CO2 partial pressure
    pC_a <- C_a/gt2ppm
    
    ## Flux atmosphere -> ocean (lecture notes Eq.5.46)
    f_ao <- k_ex * (pC_a - pC_o)
    
    ## Update inventories
    C_a <- C_a - f_ao
    C_o <- C_o + f_ao
    
    ## Update oceanic CO2 partial pressure. The partial 
    ## pressure increase is scaled by the buffer factor
    ## (lecture notes Eq.5.37)
    pC_o <- pC_o_init + buffer * (C_o - C_o_init) / C_o_init * pC_o_init
    
  }

  out <- list( pC_a_out=pC_a_out, pC_o_out=pC_o_out, C_o_out=C_o_out )
  return( out )

}

## --------------------------------------------------
## Declare which case you want
## (emission scenario for which problem)
## You could do the same by "commenting" and "un-commenting"
## respective code parts. This way is more illustrative,
## cleaner and more nerdy. Simply copied from 1box model code.
## --------------------------------------------------
problem <- 4
## --------------------------------------------------

## Some general variables used in all problems:
## The reason why we have to declare initial values
## of oceanic pCO2 and DIC (in addition to the variable
## that is updated at every time step) is because Eq. 2 
## requires so. 
pC_a_init <- 270               # initial value (preindustrial) atmos. CO2 [ppm]
pC_o_init <- 270               # initial value (preindustrial) oceanic CO2 [ppm]
C_o_init  <- 20000              # initial ocean C-pool (DIC) [GtC=PgC]


if (problem==1) {
  ## --------------------------------------------------
  ## Problem 1:
  ## 1000 GtC CO2 are emitted in year 100 into the atmosphere. 
  ## Plot the evolution of the atmospheric perturbation
  ## (in units of ppm) over the course of 1000 years.
  ## --------------------------------------------------
  ## Initialize vector representing time series of emissions
  len <- 1000
  co2_emissions <- rep( 0, len)

  ## Put the value '1000' into the 100th element of vector 'co2_emissions'
  co2_emissions[100] <- 1000


} else if (problem==2) {
  ## --------------------------------------------------
  ## Problem 2:
  ## ... same for step change in emissions
  ## (year 1-99: 0; year 100-1000: 10)
  ## --------------------------------------------------
  co2_emissions <- rep( 0, len)
  co2_emissions[100:1000] <- 10


} else if (problem==3) {
  ## --------------------------------------------------
  ## Problem 3:
  ## ... same for linearly increasing emissions
  ## (from 0 in year 1 to 20 GtC in year 1000)
  ## --------------------------------------------------
  ## Use the 'approx' function in R to linearly interpolate
  co2_emissions <- approx( c(1,1000), c(0,20), xout=1:1000 , method="linear" )$y

} else if (problem==4) {
  ## --------------------------------------------------
  ## Problem 4:
  ## stabilization of atmospheric CO2 from current 
  ## emission levels
  ## --------------------------------------------------
  ## Use the 'approx' function in R to linearly interpolate
  yr <- 1:len
  co2_emissions <- sapply( yr, FUN = function(x) 10 * exp( -0.01 * x ) )

} else {
  
  ## Print some error message
  print("select a valid option for emission scenario (1,2,3)")

} 


## --------------------------------------------------
## run 2-box model for this emission scenario
## --------------------------------------------------
out <- twobox( pC_a_init, pC_o_init, C_o_init, co2_emissions  )


## --------------------------------------------------
## PLOT (visible in RStudio)
## --------------------------------------------------
## modify plotting parameters
ylim <- c( 0, max( out$pC_a_out) )
par( xaxs="i", # plotting domain starts at first and ends at last value (by default, R adds ~5% space on either side)
    las=1 # horizontal axis tick labels 
    )
## label x- and y-axis, plot a thick red line:
plot( seq( len ), out$pC_a_out,
     type='l',
     xlab="simulation year",
     ylab=expression(paste("atmospheric CO"[2], " [ppm]")),
     col="red", lwd=2,
     ylim=ylim
     )

## add ticks on top side (by over-laying axis)
axis(3,labels=FALSE)

## add ticks on right side (by over-laying axis)
axis(4,labels=FALSE)


## --------------------------------------------------
## PANEL PLOT (creates a PDF)
## --------------------------------------------------
magn <- 3.5
ncols <- 1
nrows <- 3
widths <- rep(1.6*magn,ncols)
heights <- rep(magn,nrows)
order <- matrix(c(1:(nrows*ncols)),nrows,ncols,byrow=TRUE)

pdf( "twobox.pdf", width=sum(widths), height=sum(heights) )

panel <- layout(
                order,
                widths=widths,
                heights=heights,
                TRUE
                )
                # layout.show(panel)

par( xaxs="i", # plotting domain starts at first and ends at last value (by default, R adds ~5% space on either side)
    las=1      # horizontal axis tick labels 
    )

## 1st plot: CO2 emissions 
plot( seq( len ), co2_emissions,
     type='l',
     xlab="simulation year",
     ylab=expression(paste("CO"[2], " emissions [GtC a"^{-1},"]")),
     col="red", lwd=2
     )

## add a title (normal font, magnification parameter=1.5 )
title( expression(paste("CO"[2], " emissions")),  font.main=1, cex.main=1.5 )

## add ticks on top side (by over-laying axis)
axis(3,labels=FALSE)

## add ticks on right side (by over-laying axis)
axis(4,labels=FALSE)


## 2nd plot: atospheric CO2
ylim <- c( 0, max(out$pC_a_out) )
plot( seq( len ), out$pC_a_out,
     type='l',
     xlab="simulation year",
     ylab=expression(paste("atmospheric CO"[2], " [ppm]")),
     col="red", lwd=2,
     ylim=ylim
     )

## add a title (normal font, magnification parameter=1.5 )
title( expression(paste("atmospheric CO"[2])),  font.main=1, cex.main=1.5 )

## add ticks on top side (by over-laying axis)
axis(3,labels=FALSE)

## add ticks on right side (by over-laying axis)
axis(4,labels=FALSE)


## 3rd plot: oceanic DIC
ylim <- c( 0, max(out$C_o_out) )
plot( seq( len ), out$C_o_out,
     type='l',
     xlab="simulation year",
     ylab=expression(paste("oceanic DIC [GtC]")),
     col="red", lwd=2,
     ylim=ylim
     )

## add a title (normal font, magnification parameter=1.5 )
title( expression(paste("oceanic DIC")),  font.main=1, cex.main=1.5 )

## add ticks on top side (by over-laying axis)
axis(3,labels=FALSE)

## add ticks on right side (by over-laying axis)
axis(4,labels=FALSE)


dev.off()
