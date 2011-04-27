# 
#  RedistToRight.ani.R
#  
#  Created by Polley, Eric on 2010-10-13.
#

RedistToRight.ani <- function(survData = Surv(time = c(10, 25, 44, 55, 118, 153, 289), event = c(1, 1, 1, 0, 1, 1, 1)), nSteps = 20, waitTime = ani.options("interval")) {
  require(animation)
  require(survival)
  # data checks:
  if(class(survData) != "Surv") stop("survData must be of class Surv, see ?Surv for details")
  if(attributes(survData)$type != "right") stop("survData must be right censored data")
  n <- dim(survData)[1L]
  if(sum(survData[, "status"]) < (n - 1)) stop("Only one censoring time allowed")
  # function for setting up plot
  makePlot <- function(time, vertical = TRUE, horizontal = TRUE, whichCensor = NULL) {
      n <- length(time)
      ySteps <- seq.int(from = 1, to = 0, by = -1/n)
      xSteps <- c(0, time)
      plot(x = 1, y = 1, xlim = c(0, max(time)* 1.04), ylim = c(0, 1), type = "n", xlab = "Time", ylab = "Survival")
      # only plot to censor point
      if(!is.null(whichCensor)) {
        if(vertical){
            for(ii in seq(whichCensor)) {
                segments(x0 = xSteps[ii+1], y0 = ySteps[ii] , x1 = xSteps[ii+1], y1 = ySteps[ii+1])
            }
        }
        if(horizontal) {
            for(ii in seq(whichCensor)) {
                segments(x0 = xSteps[ii], y0 = ySteps[ii], x1 = xSteps[ii+1], y1 = ySteps[ii])
            }
        }
      } else if(is.null(whichCensor)) { 
      # plot full curve
      if(vertical){
          for(ii in seq(n)) {
              segments(x0 = xSteps[ii+1], y0 = ySteps[ii] , x1 = xSteps[ii+1], y1 = ySteps[ii+1])
          }
      }
      if(horizontal) {
          for(ii in seq(n)) {
              segments(x0 = xSteps[ii], y0 = ySteps[ii], x1 = xSteps[ii+1], y1 = ySteps[ii])
          }
      }
    }
    invisible(list(xSteps = xSteps, ySteps = ySteps))
  }
  # first do all calculations:
  whichCensor <- which(survData[, "status"] == 0)
  censorTime <- survData[whichCensor, "time"]
  maxTime <- max(survData[, "time"])  
  nAfterCensor <- sum(survData[, "time"] > censorTime)
  heightLine <- (1/n)/nAfterCensor
  segmentBreaks <- (1 - whichCensor/n) + seq.int(from = 0, to = 1/n, by = heightLine)
  buffer <- .1*heightLine
  xSteps <- matrix(NA, nrow = nSteps, ncol = nAfterCensor)
  for(ii in seq(nAfterCensor)) {
      xSteps[, ii] <- seq.int(from = censorTime, to = survData[whichCensor + ii, "time"], length.out = nSteps)
  }
  yStepsFinal <- seq.int(from = 1, to = 0, by = -1/n)
  yStepsFinal[yStepsFinal < yStepsFinal[whichCensor]] <- yStepsFinal[yStepsFinal < yStepsFinal[whichCensor]] - seq(nAfterCensor + 1)*(heightLine)
  yStepsFinal <- yStepsFinal[-length(yStepsFinal)]
  xStepsFinal <- c(0, survData[survData[, "status"]==1, "time"])
  ySteps1 <- matrix(NA, nrow = nSteps, ncol = nAfterCensor)
  for(ii in seq(nAfterCensor)) {
      ySteps1[, ii] <- seq.int(from = segmentBreaks[ii], to = (yStepsFinal[whichCensor + (ii - 1)] - heightLine), length.out = nSteps)
  }
  ySteps2 <- matrix(NA, nrow = nSteps, ncol = nAfterCensor)
  for(ii in seq(nAfterCensor)) {
      ySteps2[, ii] <- seq.int(from = (1 - (whichCensor + ii - 1)/n), to = yStepsFinal[whichCensor + ii - 1] - heightLine, length.out = nSteps)
  }
  
  # plot 1, full KM without censoring
  makePlot(time = survData[, "time"], horizontal = TRUE)
  Sys.sleep(waitTime)
  
  #  plot 2, only vertical bars
  makePlot(time = survData[, "time"], horizontal = FALSE)
  Sys.sleep(waitTime)

  # add censoring polygon
  polygon(x = c(censorTime, censorTime, maxTime + 0.1*maxTime, maxTime + 0.1*maxTime), y = c(1 - (whichCensor - 1)/n, 1 - whichCensor/n, 1 - whichCensor/n, 1 - (whichCensor - 1)/n), col = rgb(red = .7, blue = .9, green = .7, alpha = .5), border = grey(9/10))
  Sys.sleep(waitTime)
  
  # break vertical bar into pieces
  for(ii in seq(nAfterCensor)) {
      segments(x0 = censorTime, y0 = segmentBreaks[ii] + buffer, x1 = censorTime, y1 = segmentBreaks[ii + 1] - buffer, lwd = 2)
  }
  Sys.sleep(waitTime)
  
  # move the lines to the right
  for(tt in seq(nSteps)) { 
    makePlot(time = survData[, "time"], horizontal = FALSE)
    polygon(x = c(censorTime, censorTime, maxTime + 0.1*maxTime, maxTime + 0.1*maxTime), y = c(1 - (whichCensor - 1)/n, 1 - whichCensor/n, 1 - whichCensor/n, 1 - (whichCensor - 1)/n), col = rgb(red = .7, blue = .9, green = .7, alpha = .5), border = grey(9/10))
    for(ii in seq(nAfterCensor)) {
      segments(x0 = xSteps[tt, ii], y0 = segmentBreaks[ii] + buffer, x1 = xSteps[tt, ii], y1 = segmentBreaks[ii + 1] - buffer, lwd = 2)
    }
    Sys.sleep(waitTime)
  }

  # Now move points up/down 
  # 2 lines to move, ySteps1 for the breaks, ySteps2 for the original lines
  for(tt in seq(nSteps)) {
    makePlot(time = survData[, "time"], horizontal = FALSE, whichCensor = whichCensor)
    polygon(x = c(censorTime, censorTime, maxTime + 0.1*maxTime, maxTime + 0.1*maxTime), y = c(1 - (whichCensor - 1)/n, 1 - whichCensor/n, 1 - whichCensor/n, 1 - (whichCensor - 1)/n), col = rgb(red = .7, blue = .9, green = .7, alpha = .5), border = grey(9/10))
    for(ii in seq(nAfterCensor)) {
      segments(x0 = xStepsFinal[whichCensor + ii], y0 = ySteps1[tt, ii] + buffer, x1 = xStepsFinal[whichCensor + ii], y1 = ySteps1[tt, ii] - buffer + heightLine, lwd = 2)
    }
    for(ii in seq(nAfterCensor)) {
      segments(x0 = xStepsFinal[whichCensor + ii], y0 = ySteps2[tt, ii], x1 = xStepsFinal[whichCensor + ii], y1 = ySteps2[tt, ii] - (1/n), lwd = 1)
    }
    Sys.sleep(waitTime)
  }
  Sys.sleep(5*waitTime)
  plot(x = 1, y = 1, xlim = c(0, maxTime* 1.04), ylim = c(0, 1), type = "n", xlab = "Time", ylab = "Survival")
  # vertical
  for(ii in seq(n - 1)) {
    segments(x0 = xStepsFinal[ii + 1], y0 = yStepsFinal[ii] , x1 = xStepsFinal[ii + 1], y1 = yStepsFinal[ii+1])
  }
  # horizontal
  for(ii in seq(n - 1)) {
    segments(x0 = xStepsFinal[ii], y0 = yStepsFinal[ii], x1 = xStepsFinal[ii+1], y1 = yStepsFinal[ii]) 
  }
  Sys.sleep(5*waitTime)
}
