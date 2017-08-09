setWorkDir<-function() {
  frame_files <- lapply(sys.frames(), function(x) x$ofile)
  frame_files <- Filter(Negate(is.null), frame_files)
  #EOC added 8.4.2017 to avoid error ##########################################
  if (length(frame_files)>0){
    dir <- dirname(frame_files[[length(frame_files)]])
  } else {dir <- getwd()}
  ############################################################################
  setwd(dir)
}
# setWorkDir()
# source("Menu.r")
# source("configureFrame.r")
# source("File.r")
# source("Edit.r")
# source("Plotting.r")
# source("Diversity.r")
# source("GMM.r")
# source("Transformations.r")
# source("Spatial.r")
# source("Sampling.r")
# source("MultivariateStatistics.r")
# source("UnivariateStatistics.r")
# source("Bayesian.r")
# source("2dDigitize.r")
# source("2dDigitize.link.r")
# source("2dDigitize.slider.r")
# source("2dDigitize.digitize.r")
