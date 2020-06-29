library("colortools")

# constants
xSize = 11
ySize = 11
hFactor = 0.1

# create regular sequences to traverse x and y of our grid
xseq <- seq.int(1:xSize)
yseq <- seq.int(1:ySize)

# Draw empty plot
plot(
  c(0,xSize+2), c(0,ySize+2), 
  col = "white", xlab = "", ylab = "", 
  axes=F, 
  asp = 1
)            

# plot polygons in a grid
for (i in xseq) {
  for (j in yseq) {
    
    # determine color depending on y
    if (j<=6) {
      color_idx = j -1
    } else {
      color_idx = xSize - j
    }
    
    # amount of hysteresis
    hyst <- rnorm(8, 0, hFactor*sin(pi * (j-1)/(ySize-1) ))
    
    # calculate x and y coordinates
    x <- c(0+i + hyst[1], 1+i+hyst[2], 1+i+hyst[3], 0+i+hyst[4])
    y <- c(0+j+hyst[5], 0+j+hyst[6], 1+j+hyst[7], 1+j+hyst[8])
    
    # plot polygon
    polygon(
      x, y, 
      col = sequential("deepskyblue", plot = FALSE)[color_idx],
      # border = NA
    )
  }
}


