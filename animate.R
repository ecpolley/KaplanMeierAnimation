
# how to save animation:
library(animation)
source("RedistToRight.ani.R")

# creates HTML page
oopt <- ani.options(nmax = 100, outdir = getwd(), interval = 0.3, title = "Demonstration of the Redistribute-to-the-Right Algorithm", description = "Visual illustration of the Kaplan-Meier estimate of the survival distribution with right censoring")
ani.start()
RedistToRight.ani()
ani.stop()


# or a gif:
oopt <- ani.options(nmax = 100)
saveMovie(RedistToRight.ani(), width = 500, height = 500, interval = 0.3, outdir = "/Users/polleyec/Desktop/animation")
ani.options(oopt)

# or LaTeX:
# PDF only works in Adobe Reader/Acrobat
oopt = ani.options(interval = 0.3, nmax = 100)
saveLatex({
  RedistToRight.ani()
}, ani.basename = "Redist", ani.opts = "controls,loop,width=0.8\\textwidth",
    latex.filename = "Redistribute.tex")
ani.options(oopt)