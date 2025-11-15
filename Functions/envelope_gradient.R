
#update to fields envelopePlot() function with color gradients

## development section
#TODO: remove the below test code,

vert.res <- 200
x <- seq(0, 1, length.out = 100)
y <- seq(0, 1, length.out = vert.res)
z <- outer(x, y, function(x, y) y)  # Gradient in y-direction

#using rasterImage
plot(NA, xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "")

#test case polygon
poly.x <- c(0, 1, 0.5, 0)
poly.y <- c(0, 0, 1, 0)

polygon(poly.x, poly.y, col = NA, border = "black", lwd = 2)      # outline only

x.1 <- min(x)
x.2 <- max(x)
y.1 <- min(y)
y.2 <- max(y)

#using in.poly(.grid)
#temp setup, adjust later
grid.list <- list(x = seq(x.1, x.2, length.out = 100),
                  y = seq(y.1, y.2, length.out = vert.res))
xp<- cbind( poly.x, poly.y)

temp.in <- in.poly.grid(grid.list, xp)


#generate as.raster object

#TODO: generalize the colors
#temp library for testing colors:
suppressMessages(library( rcartocolor))
test.colors = carto_pal(vert.res, "Peach")
display_carto_pal(vert.res, "Peach")[20:100]
#heat.colors(vert.res)
#z.col <- heat.colors(vert.res)[cut(z, breaks = vert.res)]

z.col <- test.colors[cut(z, breaks = vert.res)]


#pass the color scale object to this
z.matrix <- matrix(rev(z.col), nrow = 100)

temp.raster <- as.raster(t(z.matrix))
temp.raster[!rev(t(temp.in))] <- 0

#re-run plot with rasterImage()
plot(NA, xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "")
rasterImage(temp.raster,
            xleft=min(poly.x),
            ybottom=min(poly.y),
            xright=max(poly.x),
            ytop=max(poly.y))




#TODO: get this all working correctly:
## there are issues with the color/raster matrices, get this working

#test plot for time series data
x1 = time.plot
y1 = resp.top
x2 = time.plot
y2 = rep(0, length(resp.time))
plot(NA, xlim = range(x1, x2), ylim = range(y1, y2), xlab = "", ylab = "")

#col: passes solid color or gradient
col <- carto_pal(200, "Peach") 
h.res <- 1000
#vertical res
vert.res <- 400

#functional parameters
#passes x1, x2, y1, y2 (and others)

#polygon vectors
poly.x <- c(x1, rev(x2))
poly.y <- c(y1, rev(y2))

#get in polygon values for grid
grid.list <- list(x = seq(min(poly.x), max(poly.x), length.out = h.res),
                  y = seq(min(poly.y), max(poly.y), length.out = vert.res))
xp <- cbind( poly.x, poly.y)

#TODO: get in.poly/in.poly.grid working faster this is a hold-up for several things. Currently too slow.
temp.in <- in.poly.grid(grid.list, xp)

#get raster image colors
z <- outer(grid.list$x, grid.list$y, function(x, y) y)  # Gradient in y-direction
z.col <- col[cut(z, breaks = vert.res)]

z.matrix <- matrix(rev(z.col), nrow = h.res)
temp.raster <- as.raster(z.matrix)
temp.raster[!temp.in] <- 0

plot(NA, xlim = range(x1, x2), ylim = range(y1, y2), xlab = "", ylab = "")
rasterImage(temp.raster,
            xleft=min(poly.x),
            ybottom=min(poly.y),
            xright=max(poly.x),
            ytop=max(poly.y))

polygon(poly.x,poly.y, col = NA, border = "black")



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
  
  #if(!gradient)
  #else: call gradient function (written below)
  
  lines(x1, y1, lwd = 3, col = lineCol)
  lines(x2, y2, lwd = 3, col = lineCol)
}


## gradient function 






## --- old (failed) work --- ##
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



#old rasterImage
z.row <- heat.colors(100)[cut(z, breaks = 100)]
z.matrix <- matrix(z.row, ncol = 100)

rasterImage(as.raster(z.matrix), 0, 0, 1, 1, angle = 0)

poly_x <- c(0, 1, 0.5, 0)
poly_y <- c(0, 0, 1, 0)

# Now mask the area *outside* the polygon with white polygons
polygon(c(0, 1, 1, 0), c(0, 0, 1, 1), col = "white", border = NA)  # full background
polygon(poly_x, poly_y, col = NA, border = "black", lwd = 2)      # outline only

rasterImage(as.raster(z.matrix), 0, 0, 1, 1)