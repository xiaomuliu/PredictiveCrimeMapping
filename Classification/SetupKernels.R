sigma <- 2  # unit: pixel size 
lambda <- 1/floor(2*7/groupSize)  # unit: time interval

window.x <- 3 
window.y <- 3
window.t <- 8 

kernel.Xgrd <- seq(-window.x,window.x,length.out=2*window.x+1)
kernel.Ygrd <- seq(-window.y,window.y,length.out=2*window.y+1)
kernel.Tgrd <- seq(0,window.t-1,length.out=window.t)

kernel.x <- 1/(sqrt(2*pi)*sigma)*exp(-kernel.Xgrd^2/(2*sigma^2))
kernel.y <- 1/(sqrt(2*pi)*sigma)*exp(-kernel.Ygrd^2/(2*sigma^2))
kernel.t <- exp(-lambda*kernel.Tgrd)