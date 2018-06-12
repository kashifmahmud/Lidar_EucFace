wood.pool = data.frame(matrix(vector(), 6, 5, dimnames=list(c(), c("Year", "Ring", "CO2","Woodmass_allometric", "Woodmass_Lidar"))), stringsAsFactors=F)

wood.pool$Year = as.Date("2015-05-26")
wood.pool$Year = format(as.Date(wood.pool$Year, format="%Y-%m-%d"),"%Y")
wood.pool$Ring = 1:6
wood.pool$CO2 = as.factor(c("ele","amb","amb","ele","ele","amb"))
wood.pool$Woodmass_allometric = c(5565.991, 5285.748, 5372.639, 3609.480, 6738.106, 5311.698)

# # subset Lidar data for all plots
# data_sub = subset(data, d.bh > 0.1)

plot1 = subset(data,plot %in% as.factor("plot 1"))
plot2 = subset(data,plot %in% as.factor("plot 2"))
plot3 = subset(data,plot %in% as.factor("plot 3"))
plot4 = subset(data,plot %in% as.factor("plot 4"))
plot5 = subset(data,plot %in% as.factor("plot 5"))
plot6 = subset(data,plot %in% as.factor("plot 6"))

wood.pool$Woodmass_Lidar = c(sum(plot1$m.so),sum(plot2$m.so),sum(plot3$m.so),sum(plot4$m.so),sum(plot5$m.so),sum(plot6$m.so))
wood.pool$Woodmass_stem = c(sum(plot1$m.st),sum(plot2$m.st),sum(plot3$m.st),sum(plot4$m.st),sum(plot5$m.st),sum(plot6$m.st))
wood.pool$Woodmass_branch = c(sum(plot1$m.br),sum(plot2$m.br),sum(plot3$m.br),sum(plot4$m.br),sum(plot5$m.br),sum(plot6$m.br))

# ----------------------------------------------------------------------------------
# Preprocess the Lidar data for wood surface area
# Merge all lidar plot data into one file
# lidar_data_euc_area = rbind(lidar_data_euc_1,lidar_data_euc_2,lidar_data_euc_3,lidar_data_euc_4,lidar_data_euc_5,lidar_data_euc_6)
# keeps <- c("Total.cylinder.area", "Trunk.surface.area", "plot")
# lidar_data_euc_area = lidar_data_euc_area[ , keeps, drop = FALSE]

# # subset Lidar data for all plots
# lidar_data_euc = subset(lidar_data_euc, d.bh > 0.1)
plot1 = subset(lidar_data_euc_mean_sub_1,plot %in% as.factor("plot 1"))
plot2 = subset(lidar_data_euc_mean_sub_1,plot %in% as.factor("plot 2"))
plot3 = subset(lidar_data_euc_mean_sub_1,plot %in% as.factor("plot 3"))
plot4 = subset(lidar_data_euc_mean_sub_1,plot %in% as.factor("plot 4"))
plot5 = subset(lidar_data_euc_mean_sub_1,plot %in% as.factor("plot 5"))
plot6 = subset(lidar_data_euc_mean_sub_1,plot %in% as.factor("plot 6"))

wood.pool$Woodarea_total = c(sum(plot1$wood.area),sum(plot2$wood.area),sum(plot3$wood.area),sum(plot4$wood.area),sum(plot5$wood.area),sum(plot6$wood.area))
wood.pool$Woodarea_stem = c(sum(plot1$stem.area),sum(plot2$stem.area),sum(plot3$stem.area),sum(plot4$stem.area),sum(plot5$stem.area),sum(plot6$stem.area))
wood.pool$Woodarea_branch = wood.pool$Woodarea_total - wood.pool$Woodarea_stem
# ----------------------------------------------------------------------------------
# Calculate the light interception wood area
plot.area = pi*12.5^2
wood.pool$Wood_interception = wood.pool$Woodarea_total / plot.area
mean(wood.pool$Wood_interception)
write.csv(wood.pool, file = "output/wood_interception_eucface.csv", row.names = FALSE)
# write.csv(wood.pool[c("Year","Ring","CO2","Wood_interception")], file = "output/wood_interception_eucface.csv", row.names = FALSE)

# ----------------------------------------------------------------------------------
# read the allometry-based AGB
wood_pool_allometry = read.csv("output/wood_pool_allometry.csv")

keeps <- c("plot", "CO2", "tree", "d.bh_mean", "m.st_mean", "m.br_mean", "m.so_mean", "height_mean")
lidar_data_euc_crop = lidar_data_euc_mean_sub[ , keeps, drop = FALSE]
names(lidar_data_euc_crop) = c("plot", "CO2", "tree", "d.bh", "m.st", "m.br", "m.so", "height")

# ----------------------------------------------------------------------------------
# Plot biomass against DBH with different growing conditions
# cbPalette = rainbow(nlevels(baad_data_euc$growingCondition))[rank(1:nlevels(baad_data_euc$growingCondition))]
cbPalette = c("gray", "orange", "skyblue", "green3", "yellow3", "#0072B2", "#D55E00")
font.size = 12
pd <- position_dodge(0)
plot = list() 
plot[[1]] = ggplot(wood.pool, aes(x=Woodmass_allometric, y=Woodmass_Lidar, group = CO2, colour=CO2)) + 
  geom_point(position=pd, size = 1.5) + geom_abline(intercept = 0, slope = 1) + 
  scale_x_continuous(limits = c(min(wood.pool$Woodmass_allometric), max(wood.pool$Woodmass_Lidar))) + 
  scale_y_continuous(limits = c(min(wood.pool$Woodmass_allometric), max(wood.pool$Woodmass_Lidar))) +
  xlab("Allometry-based AGB (kg)") + ylab("Lidar-based AGB (kg)") + 
  labs(colour="Treatment") +
  scale_colour_manual(breaks=c("amb", "ele"), values=c("blue","red")) +
  theme_bw() + 
  theme(legend.position = c(0.8,0.25),legend.direction = "vertical") +
  theme(legend.key.height=unit(0.7,"line")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

plot[[2]] = ggplot(lidar_data_euc_crop, aes(x=d.bh, y=m.so, group = CO2, colour=CO2)) + 
  geom_point(position=pd, size = 1.5) + 
  # coord_trans(y = "log10", x = "log10") +
  geom_smooth(method="lm",se=FALSE) + 
  ylab("Lidar-based AGB (kg)") + xlab("DBH (m)") + 
  labs(colour="Treatment") +
  # scale_x_continuous(limits = c(min(data$d.bh, na.rm = T), max(data$d.bh, na.rm = T)), breaks=c(.05,.1,.5,1,2),labels=c(.05,.1,.5,1,2)) +
  # scale_y_continuous(limits = c(min(data$m.so, na.rm = T), max(data$m.so, na.rm = T)), breaks=c(1,5,10,50,100,500,1000,5000,10000,50000),labels=c(1,5,10,50,100,500,1000,5000,10000,50000)) +
  # scale_colour_manual(breaks=c("FW", "PM"), labels=c("Field Wild", "Plantation Managed"), values=c("red","green","blue")) +
  theme_bw() + 
  theme(legend.position = c(0.25,0.85),legend.direction = "vertical") +
  theme(legend.key.height=unit(0.7,"line")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

plot[[3]] = ggplot(lidar_data_euc_crop, aes(x=d.bh, y=m.st, group = CO2, colour=CO2)) + 
  geom_point(position=pd, size = 1.5) + 
  # coord_trans(y = "log10", x = "log10") +
  geom_smooth(method="lm",se=FALSE) + 
  ylab("Lidar-based Stem mass (kg)") + xlab("DBH (m)") + 
  labs(colour="Treatment") +
  # scale_x_continuous(limits = c(min(data$d.bh, na.rm = T), max(data$d.bh, na.rm = T)), breaks=c(.05,.1,.5,1,2),labels=c(.05,.1,.5,1,2)) +
  # scale_y_continuous(limits = c(min(data$m.so, na.rm = T), max(data$m.so, na.rm = T)), breaks=c(1,5,10,50,100,500,1000,5000,10000,50000),labels=c(1,5,10,50,100,500,1000,5000,10000,50000)) +
  # scale_colour_manual(breaks=c("FW", "PM"), labels=c("Field Wild", "Plantation Managed"), values=c("red","green","blue")) +
  theme_bw() + 
  theme(legend.position = c(0.25,0.85),legend.direction = "vertical") +
  theme(legend.key.height=unit(0.7,"line")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plot[[4]] = ggplot(lidar_data_euc_crop, aes(x=d.bh, y=m.br, group = CO2, colour=CO2)) + 
  geom_point(position=pd, size = 1.5) + 
  # coord_trans(y = "log10", x = "log10") +
  geom_smooth(method="lm",se=FALSE) + 
  ylab("Lidar-based Branch mass (kg)") + xlab("DBH (m)") + 
  labs(colour="Treatment") +
  # scale_x_continuous(limits = c(min(data$d.bh, na.rm = T), max(data$d.bh, na.rm = T)), breaks=c(.05,.1,.5,1,2),labels=c(.05,.1,.5,1,2)) +
  # scale_y_continuous(limits = c(min(data$m.so, na.rm = T), max(data$m.so, na.rm = T)), breaks=c(1,5,10,50,100,500,1000,5000,10000,50000),labels=c(1,5,10,50,100,500,1000,5000,10000,50000)) +
  # scale_colour_manual(breaks=c("FW", "PM"), labels=c("Field Wild", "Plantation Managed"), values=c("red","green","blue")) +
  theme_bw() + 
  theme(legend.position = c(0.25,0.85),legend.direction = "vertical") +
  theme(legend.key.height=unit(0.7,"line")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

pdf(file = "output/3.woodmass_figures.pdf")
# plot
do.call(grid.arrange,  plot)
dev.off()

# do.call(grid.arrange,  plot)

# ----------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------
# Plotting observation and modelled data
# Preprocess the Lidar data
# # Merge all lidar plot data into one file
# lidar_data_euc = rbind(lidar_data_euc_1,lidar_data_euc_2,lidar_data_euc_3,lidar_data_euc_4,lidar_data_euc_5,lidar_data_euc_6)
# 
# lidar_data_euc = lidar_data_euc_1
# lidar_data_euc$Total.mass = lidar_data_euc$Total.volume * wood.density # Unit = Kg
# lidar_data_euc$Branch.mass = lidar_data_euc$Branch.volume * wood.density # Unit = Kg
# lidar_data_euc$Stem.mass = lidar_data_euc$Trunk.volume * wood.density # Unit = Kg
# 
# keeps <- c("Dbh.QSM", "Stem.mass", "Branch.mass", "Total.mass", "Total.height", "plot", "tree", "CO2")
# lidar_data_euc = lidar_data_euc[ , keeps, drop = FALSE]
# write.csv(lidar_data_euc, file = "output/lidar_data_eucface.csv", row.names = FALSE)
# 
# names(lidar_data_euc) = c("d.bh", "m.st", "m.br", "m.so", "height", "plot", "tree", "CO2")
# # lidar_data_euc = subset(lidar_data_euc, d.bh > 10)

# plots = list() 
# par(mfrow = c(2, 3))
# ring = as.factor(c("plot 1","plot 2","plot 3","plot 4","plot 5","plot 6"))
# for (i in 1:length(ring)) {
#   lidar_data_euc_sub = subset(lidar_data_euc_crop,plot %in% ring[i])
#   wood_pool_allometry_sub = subset(wood_pool_allometry,Ring %in% i)
#   xmin = min(lidar_data_euc_sub$d.bh,wood_pool_allometry_sub$diam)
#   xmax = max(lidar_data_euc_sub$d.bh,wood_pool_allometry_sub$diam)
#   ymin = min(lidar_data_euc_sub$m.so,wood_pool_allometry_sub$biom)
#   ymax = max(lidar_data_euc_sub$m.so,wood_pool_allometry_sub$biom)
#   plot(lidar_data_euc_sub$d.bh,lidar_data_euc_sub$m.so,col="red",main=paste(as.character(ring[i])), pch=15, 
#        xlim=c(xmin, xmax), ylim=c(ymin, ymax),
#        ylab="AGB(kg)", xlab="DBH(cm)")
#   text(lidar_data_euc_sub$d.bh,lidar_data_euc_sub$m.so, labels=lidar_data_euc_sub$tree, cex= 0.7, pos=1)
#   lines(wood_pool_allometry_sub$diam,wood_pool_allometry_sub$biom,type="p", col="green", pch=20)
#   legend('topleft', c("Allomerty-based", "Lidar-based"), bty='n', col=c('green','red'),cex=1,lty=1,lwd=2,seg.len=0.25,y.intersp=0.5)
# }
# mtext(paste("AGB vs DBH for all plots"), side = 3, line = -1.5, outer = TRUE)
# plots[[1]] = recordPlot()
# 
# pdf(file = "output/5.AGB_vs_dbh_normal_scale.pdf")
# plots[[1]]
# dev.off()

plots = list() 
par(mfrow = c(2, 3))
ring = as.factor(c("plot 1","plot 2","plot 3","plot 4","plot 5","plot 6"))
# for (i in 1:length(ring)) {
for (i in 1:length(ring)) {
  lidar_data_euc_sub = subset(lidar_data_euc_mean_sub,plot %in% ring[i])
  wood_pool_allometry_sub = subset(wood_pool_allometry,Ring %in% i)
  xmin = min(lidar_data_euc_sub$d.bh_mean-lidar_data_euc_sub$d.bh_sd,wood_pool_allometry_sub$diam)
  xmax = max(lidar_data_euc_sub$d.bh_mean+lidar_data_euc_sub$d.bh_sd,wood_pool_allometry_sub$diam)
  ymin = min(lidar_data_euc_sub$m.so_mean-lidar_data_euc_sub$m.so_sd,wood_pool_allometry_sub$biom)
  ymax = max(lidar_data_euc_sub$m.so_mean+lidar_data_euc_sub$m.so_sd,wood_pool_allometry_sub$biom)
  plot(lidar_data_euc_sub$d.bh_mean,lidar_data_euc_sub$m.so_mean,col="red",main=paste(as.character(ring[i])), pch=15, 
       xlim=c(xmin, xmax), ylim=c(ymin, ymax),
       ylab="AGB(kg)", xlab="DBH(cm)")
  arrows(x0=lidar_data_euc_sub$d.bh_mean, y0=lidar_data_euc_sub$m.so_mean-lidar_data_euc_sub$m.so_sd, x1=lidar_data_euc_sub$d.bh_mean, 
         y1=lidar_data_euc_sub$m.so_mean+lidar_data_euc_sub$m.so_sd, angle=90, code=3, length=0.04, lwd=0.4)
  arrows(x0=lidar_data_euc_sub$d.bh_mean-lidar_data_euc_sub$d.bh_sd, y0=lidar_data_euc_sub$m.so_mean, x1=lidar_data_euc_sub$d.bh_mean+lidar_data_euc_sub$d.bh_sd, 
         y1=lidar_data_euc_sub$m.so_mean, angle=90, code=3, length=0.04, lwd=0.4)
  
  text(lidar_data_euc_sub$d.bh_mean,lidar_data_euc_sub$m.so_mean, labels=lidar_data_euc_sub$tree, cex= 0.7, pos=1)
  lines(wood_pool_allometry_sub$diam,wood_pool_allometry_sub$biom,type="p", col="green", pch=20)
  legend('topleft', c("Allomerty-based", "Lidar-based"), bty='n', col=c('green','red'),cex=1,lty=1,lwd=2,seg.len=0.25,y.intersp=0.5)
}
mtext(paste("AGB vs DBH for all plots"), side = 3, line = -1.5, outer = TRUE)
plots[[1]] = recordPlot()

pdf(file = "output/5.AGB_vs_dbh_normal_scale.pdf")
plots[[1]]
dev.off()


# ----------------------------------------------------------------------------------
plots = list() 
par(mfrow = c(2, 3))
ring = as.factor(c("plot 1","plot 2","plot 3","plot 4","plot 5","plot 6"))
for (i in 1:length(ring)) {
  lidar_data_euc_sub = subset(lidar_data_euc_crop,plot %in% ring[i])
  wood_pool_allometry_sub = subset(wood_pool_allometry,Ring %in% i)
  
  exponential.model <- lm(log(m.so) ~ log(d.bh), lidar_data_euc_sub)
  exponential <- exp(fitted(exponential.model))
  
  # plot(lidar_data_euc_sub$d.bh,lidar_data_euc_sub$m.so,log="xy",col="red",main=paste(as.character(ring[i])), pch=15, 
  #      ylab="log(AGB(kg))", xlab="log(DBH(cm))")
  plot(lidar_data_euc_sub$d.bh,lidar_data_euc_sub$m.so,log="xy",col="red",main=paste(as.character(ring[i])), pch=15, 
       xlim=c(10, 45), ylim=c(40, 1000), 
       ylab="log(AGB(kg))", xlab="log(DBH(cm))")
  lines(lidar_data_euc_sub$d.bh, exponential, lwd = 2, col = "grey")
  lines(wood_pool_allometry_sub$diam,wood_pool_allometry_sub$biom,type="p", col="green", pch=20)
  legend('topleft', c("Allomerty-based", "Lidar-based", "Lidar-based Regression"), bty='n', col=c('green','red','grey'),cex=0.8,lty=1,lwd=2,seg.len=0.25,y.intersp=0.5)
}
mtext(paste("AGB vs DBH for all plots"), side = 3, line = -1.5, outer = TRUE)
plots[[1]] = recordPlot()

par(mfrow = c(2, 3))
for (i in 1:length(ring)) {
  lidar_data_euc_sub = subset(lidar_data_euc_crop,plot %in% ring[i])
  wood_pool_allometry_sub = subset(wood_pool_allometry,Ring %in% i)
  
  exponential.model <- lm(log(m.so) ~ log(height), lidar_data_euc_sub)
  exponential <- exp(fitted(exponential.model))
  
  plot(lidar_data_euc_sub$height,lidar_data_euc_sub$m.so,log="xy",col="red",main=paste(as.character(ring[i])), pch=15, 
       ylab="log(AGB(kg))", xlab="log(Height(m))")
  lines(lidar_data_euc_sub$height, exponential, lwd = 2, col = "grey")
  lines(wood_pool_allometry_sub$Height,wood_pool_allometry_sub$biom,type="p", col="green", pch=20)
  legend('topleft', c("Allomerty-based", "Lidar-based", "Lidar-based Regression"), bty='n', col=c('green','red','grey'),cex=0.8,lty=1,lwd=2,seg.len=0.25,y.intersp=0.5)
}
mtext(paste("AGB vs Tree height for all plots"), side = 3, line = -1.5, outer = TRUE)
plots[[2]] = recordPlot()

par(mfrow = c(2, 3))
for (i in 1:length(ring)) {
  lidar_data_euc_sub = subset(lidar_data_euc_crop,plot %in% ring[i])
  
  # exponential.model <- lm(log(m.so) ~ log(height), lidar_data_euc_sub)
  # exponential <- exp(fitted(exponential.model))
  
  plot(lidar_data_euc_sub$d.bh,lidar_data_euc_sub$m.st,col="red",main=paste(as.character(ring[i])), pch=15, ylab="Lidar-based wood mass (kg))", xlab="DBH (cm)")
  lines(lidar_data_euc_sub$d.bh,lidar_data_euc_sub$m.br,col="green", type="p", pch=20)
  # lines(lidar_data_euc_sub$height, exponential, lwd = 2, col = "grey")
  # lines(wood_pool_allometry_sub$Height,wood_pool_allometry_sub$biom,type="p", col="green", pch=20)
  legend('topleft', c("Stem mass", "Branch mass"), bty='n', col=c('red','green'),cex=0.8,lty=1,lwd=2,seg.len=0.25,y.intersp=0.5)
}
mtext(paste("Wood mass vs DBH for all plots"), side = 3, line = -1.5, outer = TRUE)
plots[[3]] = recordPlot()

pdf(file = "output/4.AGB_vs_dbh_and_height.pdf")
plots[[1]]
plots[[2]]
plots[[3]]
p1 = do.call(grid.arrange,  plot)
dev.off()

#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
match_tree_plot1 = read.csv("lidar_data/match_tree_plot1.csv",col.names=c("x", "y", "z", "scalar","lidar.tree","census.tree"), header=FALSE)
colnames(wood_pool_allometry)[colnames(wood_pool_allometry)=="tree"] <- "census.tree"
colnames(lidar_data_euc_1)[colnames(lidar_data_euc_1)=="tree"] <- "lidar.tree"

data_comb_plot1 = merge(wood_pool_allometry, match_tree_plot1[,c("census.tree","lidar.tree")], by="census.tree")
data_comb_plot1 = merge(data_comb_plot1, lidar_data_euc_1, by="lidar.tree")

par(mfrow = c(2, 2))
plot(data_comb_plot1$Dbh.QSM, data_comb_plot1$diam, col="red", main=paste("Matched DBH - plot #1"), pch=15, 
     xlim=c(min(data_comb_plot1$Dbh.QSM), max(data_comb_plot1$Dbh.QSM)), ylim=c(min(data_comb_plot1$Dbh.QSM), max(data_comb_plot1$Dbh.QSM)), ylab="Lidar-based DBH (cm)", xlab="Census-based DBH (cm)")
abline(a=0,b=1)

plot(data_comb_plot1$Total.height, data_comb_plot1$Height, col="red", main=paste("Matched Tree height - plot #1"), pch=15, 
     xlim=c(min(data_comb_plot1$Total.height), max(data_comb_plot1$Total.height)), ylim=c(min(data_comb_plot1$Total.height), max(data_comb_plot1$Total.height)), ylab="Lidar-based height (m)", xlab="Census-based height (m)")
abline(a=0,b=1)

plot(data_comb_plot1$Total.volume*wood.density, data_comb_plot1$biom, col="red", main=paste("Matched AGB - plot #1"), pch=15, 
     xlim=c(min(data_comb_plot1$biom), max(data_comb_plot1$biom)), ylim=c(min(data_comb_plot1$biom), max(data_comb_plot1$biom)), ylab="Lidar-based AGB (kg)", xlab="Census-based AGB (kg)")
abline(a=0,b=1)

plots[[4]] = recordPlot()
pdf(file = "output/5.matched_tree_plot1.pdf")
plots[[4]]
dev.off()
#-----------------------------------------------------------------------------------------
# plotting without matching
par(mfrow = c(2, 2))
plot(sort(data_comb_plot1$diam), sort(data_comb_plot1$Dbh.QSM), col="red", main=paste("DBH - plot #1"), pch=15, 
     xlim=c(min(data_comb_plot1$Dbh.QSM), max(data_comb_plot1$Dbh.QSM)), ylim=c(min(data_comb_plot1$Dbh.QSM), max(data_comb_plot1$Dbh.QSM)), ylab="Lidar-based DBH (cm)", xlab="Census-based DBH (cm)")
abline(a=0,b=1)

plot(sort(data_comb_plot1$Height), sort(data_comb_plot1$Total.height), col="red", main=paste("Tree height - plot #1"), pch=15, 
     xlim=c(min(data_comb_plot1$Total.height), max(data_comb_plot1$Total.height)), ylim=c(min(data_comb_plot1$Total.height), max(data_comb_plot1$Total.height)), ylab="Lidar-based height (m)", xlab="Census-based height (m)")
abline(a=0,b=1)

plot(sort(data_comb_plot1$biom), sort(data_comb_plot1$Total.volume)*wood.density, col="red", main=paste("AGB - plot #1"), pch=15, 
     xlim=c(min(data_comb_plot1$biom), max(data_comb_plot1$biom)), ylim=c(min(data_comb_plot1$biom), max(data_comb_plot1$biom)), ylab="Lidar-based AGB (kg)", xlab="Census-based AGB (kg)")
abline(a=0,b=1)

plots[[5]] = recordPlot()

pdf(file = "output/5.matched_tree_plot1.pdf")
plots[[4]]
plots[[5]]
dev.off()
