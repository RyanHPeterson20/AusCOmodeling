
#update to fields envelopePlot() function with color gradients

## development section
#TODO: remove the below test code,

x <- seq(0, 1, length.out = 100)
y <- seq(0, 1, length.out = 100)
z <- outer(x, y, function(x, y) y)  # Gradient in y-direction

#test plot
x1 = time.plot
y1 = resp.top
x2 = time.plot
y2 = rep(0, length(resp.time))
plot(NA, xlim = range(x1, x2), ylim = range(y1, y2), xlab = "", ylab = "")
polygon(c(x1, rev(x2)), c(y1, rev(y2)), col = top.col, border = NA)


#using rasterImage
plot(NA, xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "")

#test case polygon
poly.x <- c(0, 1, 0.5, 0)
poly.y <- c(0, 0, 1, 0)

polygon(poly.x, poly.y, col = NA, border = "black", lwd = 2)      # outline only
#using in.poly(.grid)




# Convert to raster image:
#TODO: generalize the colors
z.row <- heat.colors(100)[cut(z, breaks = 100)]
z.matrix <- matrix(z.row, ncol = 100)

rasterImage(as.raster(z.matrix), 0, 0, 1, 1, angle = 0)

poly_x <- c(0, 1, 0.5, 0)
poly_y <- c(0, 0, 1, 0)

# Now mask the area *outside* the polygon with white polygons
polygon(c(0, 1, 1, 0), c(0, 0, 1, 1), col = "white", border = NA)  # full background
polygon(poly_x, poly_y, col = NA, border = "black", lwd = 2)      # outline only

rasterImage(as.raster(z.matrix), 0, 0, 1, 1)

# final function: #

#TODO: move everything down here when done.

#add in updated envelopePlot() from fields
envelopePlot <- function(x1, y1, x2 = x1, y2, 
                         col ="thistle1" , lineCol = "thistle3", ...) {
  #  sort the curves -- just in case they are passed out of order
  ind<- order( x1)
  x1<- x1[ind]
  y1<- y1[ind]
  ind<- order( x2)
  x2<- x2[ind]
  y2<- y2[ind]
  
  
  #TODO: update elif from gradient true/false
  polygon(c(x1, rev(x2)), c(y1, rev(y2)), col = col, border = NA, ...)
  
  
  lines(x1, y1, lwd = 3, col = lineCol)
  lines(x2, y2, lwd = 3, col = lineCol)
}





#old, failed work.
##TODO: delete when done

# Create a gradient image using image()
image(x, y, z, col = heat.colors(100), useRaster = TRUE)
poly_x <- c(0, 1, 0.5)
poly_y <- c(0, 0, 1)

# Mask outside: draw a big white polygon covering everything
polygon(c(-1, 2, 2, -1), c(-1, -1, 2, 2), col = "white", border = NA)

# Now cut the polygon hole using the polygon you want to KEEP
# We do this by redrawing the desired polygon in the opposite direction
polygon(poly_x, poly_y, col = NA, border = "black", lwd = 2)

