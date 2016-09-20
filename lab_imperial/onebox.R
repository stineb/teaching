## --------------------------------------------------
## Introduction to the (terrestrial) Carbon Cycle
## Computer Lab.
## Author: Benjamin Stocker <b.stocker@imperial.ac.uk>
## Responsible: Colin Prentice <c.prentice@imperial.ac.uk>
## Oct. 2015
## --------------------------------------------------

## //////////////////////////////////////////////////
## 1-BOX MODEL
## --------------------------------------------------
onebox <- function( cpool0, tau, c_influx ) {

  ## ------------------------------------------------
  ## cpool0:    initial pool size (GtC)
  ## tau:       turnover (residence) time (a)
  ## c_influx:  flux into pool (GtC a-1)
  ## ------------------------------------------------

  ## determine integration length (number of time steps) from length of 'c_influx'
  len <- length(c_influx)

  ## initialise output variable (time series of pool size)
  out_cpool <- rep( NA, len )

  ## copy initial pool size to first element in output time series
  cpool <- cpool0

  ## integrate over each time step (this is an implementation of the differential equation)
  for (yr in seq(len) ) {

    ## copy current pool size to output time series
    out_cpool[yr] <- cpool

    ## update pool size with input and decay
    cpool <- cpool + c_influx[yr] - 1/tau * cpool

  }

  ## function return value is a vector containing the output time series
  return( out_cpool )

}


## //////////////////////////////////////////////////
## Declare which case you want
## (emission scenario for which problem)
## You could do the same by "commenting" and "un-commenting"
## respective code parts. This way is more illustrative,
## cleaner and more nerdy.
## --------------------------------------------------
problem <- 3
## //////////////////////////////////////////////////


if (problem==1) {
  ## //////////////////////////////////////////////////
  ## 1. Decay from initial pool = 3000 GtC
  ## --------------------------------------------------
  len       <- 500
  cpool0    <- 3000
  c_influx  <- rep( 0, len )
  tau       <- 50

  ## execute function 'onebox' with above determined arguments
  ## and store output time series (function return value) to 'out1'
  out1 <- onebox( cpool0, tau, c_influx )

  ## do the same but now with increased turnover time, store
  ## function return values to 'out2'
  tau  <- 60
  out2 <- onebox( cpool0, tau, c_influx )

  ## modify plotting parameters
  par( xaxs="i", # plotting domain starts at first and ends at last value (by default, R adds ~5% space on either side)
       las=1     # horizontal axis tick labels 
      )

  ## label x- and y-axis, plot a thick red line:
  plot( seq( len ), out1,
       type='l',
       xlab="simulation year",
       ylab=expression(paste("pool size [GtC]")),
       col="red", lwd=2
       )
  ## add ticks on top side (by over-laying axis)
  axis(3,labels=FALSE)
  ## add ticks on right side (by over-laying axis)
  axis(4,labels=FALSE) 

  ## overlay second curve (out2 with longer turnover time)
  lines( seq(len), out2, col="red", lty=2, lwd=2 )

  ## add legend
  legend( "topright", c( expression( tau ~ "= 50"), expression( tau ~ "= 60") ), lty=c(1,2), bty="n", col="red", lwd=2 )

} else if (problem==2) {
  ## //////////////////////////////////////////////////
  ## 2. "spin up with constant input flux"
  ## --------------------------------------------------
  len       <- 500
  cpool0    <- 0
  c_influx  <- rep( 60, len )
  tau       <- 50

  ## execute function 'onebox' with above determined arguments
  ## and store output time series (function return value) to 'out1'
  out1 <- onebox( cpool0, tau, c_influx )

  ## do the same but now with increased turnover time, store
  ## function return values to 'out2'
  tau  <- 60
  out2 <- onebox( cpool0, tau, c_influx )

  ## determine y-axis limits from the maximum value in out1 and out2
  ylim <- c( 0, max( out1, out2 ) )

  ## modify plotting parameters
  par( xaxs="i", # plotting domain starts at first and ends at last value (by default, R adds ~5% space on either side)
       las=1     # horizontal axis tick labels 
      )

  ## label x- and y-axis, plot a thick red line:
  plot( seq( len ), out1,
       type='l',
       xlab="simulation year",
       ylab=expression(paste("pool size [GtC]")),
       col="red", lwd=2,
       ylim=ylim
       )
  ## add ticks on top side (by over-laying axis)
  axis(3,labels=FALSE)
  ## add ticks on right side (by over-laying axis)
  axis(4,labels=FALSE) 

  ## overlay second curve (out2 with longer turnover time)
  lines( seq(len), out2, col="red", lty=2, lwd=2 )

  ## add legend
  legend( "bottomright", c( expression( tau ~ "= 50"), expression( tau ~ "= 60") ), lty=c(1,2), bty="n", col="red", lwd=2 )

} else if (problem==3) {
  ## //////////////////////////////////////////////////
  ## 3. simulate the additional sink in response to a 
  ## step increase in the input flux. Start from pre-
  ## determined equilibrium. Turnover time is held 
  ## constant
  ## --------------------------------------------------
  len       <- 500
  c_value1  <- 60
  c_influx  <- c( rep( c_value1, 100 ), rep( 70, len-100 ) )
  tau       <- 50
  cpool0_1   <- c_influx[1] * tau

  ## execute function 'onebox' with above determined arguments
  ## and store output time series (function return value) to 'out1'
  out1 <- onebox( cpool0_1, tau, c_influx )

  ## do the same but now with increased turnover time, store
  ## function return values to 'out2'. Note that pre-determined
  ## equilibrium is a function of tau and therefore has to
  ## be re-calculated
  tau      <- 60
  cpool0_2 <- c_influx[1] * tau
  out2     <- onebox( cpool0_2, tau, c_influx )

  ## determine y-axis limits from the maximum value in out1 and out2
  ylim <- c( 0, max( out1, out2 ) )

  ## modify plotting parameters
  par( xaxs="i", # plotting domain starts at first and ends at last value (by default, R adds ~5% space on either side)
       las=1     # horizontal axis tick labels 
      )

  ## label x- and y-axis, plot a thick red line:
  plot( seq( len ), out1,
       type='l',
       xlab="simulation year",
       ylab=expression(paste("pool size [GtC]")),
       col="red", lwd=2,
       ylim=ylim
       )
  ## add ticks on top side (by over-laying axis)
  axis(3,labels=FALSE)
  ## add ticks on right side (by over-laying axis)
  axis(4,labels=FALSE) 

  ## overlay second curve (out2 with longer turnover time)
  lines( seq(len), out2, col="red", lty=2, lwd=2 )

  ## evaluate additional sink
  sink1 <- out1[len] - cpool0_1
  sink2 <- out2[len] - cpool0_2

  char_sink1 <- as.character( format( sink1, digits=3 ) )
  char_sink2 <- as.character( format( sink2, digits=3 ) )

  text( 130, 390, paste("sink=",char_sink1,"GtC"), adj=c(0,0))
  text( 130,  90, paste("sink=",char_sink2,"GtC"), adj=c(0,0))

  ## add legend
  # legend( "bottomleft", c( paste(expression( tau ~ "= 50"), "sink:", format( sink1, digits=3 )), paste(expression( tau ~ "= 60"), "sink:", format( sink2, digits=3 )) ), lty=c(1,2), bty="n", col="red", lwd=2 )
  legend( "bottomleft", c( expression( tau ~ "= 50"  ), expression( tau ~ "= 60"  ) ), lty=c(1,2), bty="n", col="red", lwd=2 )

}

