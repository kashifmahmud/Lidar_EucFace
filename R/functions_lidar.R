#----------------------------------------------------------------------------------------------------------------
#- This is a collection of many functions that do the actual work of data analysis and plotting. These functions
#    are called by just a few lines of code in "CentralScript.R" to recreate the analyses and figures.
#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------
# ################ Figure 1 #####################
# # Script to read and gerenate the model diagram
# plot.model <- function() { 
#   img <- readPNG("raw_data/Figure_1.png")
#   
#   #get size
#   h<-dim(img)[1]
#   w<-dim(img)[2]
#   
#   #open new file for saving the image in "output" folder
#   png("output/Figure_1_CBM.png", width=w, height=h)
#   par(mar=c(0,0,0,0), xpd=NA, mgp=c(0,0,0), oma=c(0,0,0,0), ann=F)
#   plot.new()
#   plot.window(0:1, 0:1)
#   
#   #fill plot with image
#   usr<-par("usr")    
#   rasterImage(img, usr[1], usr[3], usr[2], usr[4])
#   
#   #close image
#   dev.off()
# }


#----------------------------------------------------------------------------------------------------------------
################ Figure 1 #####################
# Script to read and gerenate the model diagram
plot.model.lidar <- function() {
  img <- readPNG("processed_data/tree_1.png")

  #get size
  h<-dim(img)[1]
  w<-dim(img)[2]

  #open new file for saving the image in "output" folder
  png("output/Figure_1_CBM_wtc3.png", width=w, height=h)
  par(mar=c(0,0,0,0), xpd=NA, mgp=c(0,0,0), oma=c(0,0,0,0), ann=F)
  plot.new()
  plot.window(0:1, 0:1)

  #fill plot with image
  usr<-par("usr")
  rasterImage(img, usr[1], usr[3], usr[2], usr[4])
  #close image
  dev.off()
  grid.raster(img)
}
#----------------------------------------------------------------------------------------------------------------
# plot.model.lidar <- function() { 
#   img <- readPNG("processed_data/tree_1.png")
#   
#   # #get size
#   # h<-dim(img)[1]
#   # w<-dim(img)[2]
#   # 
#   # # #open new file for saving the image in "output" folder
#   # # png("output/Figure_1_CBM_wtc3.png", width=w, height=h)
#   # par(mar=c(0,0,0,0), xpd=NA, mgp=c(0,0,0), oma=c(0,0,0,0), ann=F)
#   # plot.new()
#   # plot.window(0:1, 0:1)
#   # 
#   # #fill plot with image
#   # usr<-par("usr")    
#   # rasterImage(img, usr[1], usr[3], usr[2], usr[4])
#   # #close image
#   # dev.off()
#   grid.raster(img,  width = 4)
# }

#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------
standard.error <- function(dat,na.rm=F,...){
  if(na.rm==T){
    dat <- subset(dat,is.na(dat)==F)
  }
  std <- sd(dat)
  n <- length(dat)
  se <- std/sqrt(n)
  return(se)
}

#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------
