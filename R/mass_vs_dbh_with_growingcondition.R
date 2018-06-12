# ----------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------
# Plot biomass against DBH with different growing conditions in log scale
# cbPalette = rainbow(nlevels(baad_data_euc$growingCondition))[rank(1:nlevels(baad_data_euc$growingCondition))]
cbPalette = c("gray", "orange", "skyblue", "green3", "yellow3", "#0072B2", "#D55E00")
font.size = 10
pd <- position_dodge(0)
plot = list() 
plot[[1]] = ggplot(data, aes(x=d.bh, y=m.so, group = interaction(growingCondition,datatype), colour=growingCondition, shape=plot)) + 
  geom_point(position=pd, size = 1.5) + 
  coord_trans(y = "log10", x = "log10") +
  ylab("Above-ground wood mass (kg)") + xlab("DBH (m)") + 
  labs(colour="Growing Condition", shape="Data type") +
  scale_x_continuous(limits = c(min(data$d.bh, na.rm = T), max(data$d.bh, na.rm = T)), breaks=c(.05,.1,.5,1,2),labels=c(.05,.1,.5,1,2)) +
  scale_y_continuous(limits = c(min(data$m.so, na.rm = T), max(data$m.so, na.rm = T)), breaks=c(1,5,10,50,100,500,1000,5000,10000,50000),labels=c(1,5,10,50,100,500,1000,5000,10000,50000)) +
  scale_colour_manual(breaks=c("FW", "PM"), labels=c("Field Wild", "Plantation Managed"), values=c("red","green")) +
  theme_bw() + scale_shape_manual(values = c(46, 49, 50, 51, 52, 53, 54)) +
  theme(legend.position = c(0.25,0.85),legend.direction = "vertical") +
  theme(legend.key.height=unit(0.7,"line")) +
  guides(shape=FALSE) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

plot[[2]] = ggplot(data, aes(x=d.bh, y=m.st, group = interaction(growingCondition,datatype), colour=growingCondition, shape=plot)) + 
  geom_point(position=pd, size = 1.5) + 
  coord_trans(y = "log10", x = "log10") +
  ylab("Stem mass (kg)") + xlab("DBH (m)") + 
  labs(colour="Growing Condition", shape="Data type") +
  scale_x_continuous(limits = c(min(data$d.bh, na.rm = T), max(data$d.bh, na.rm = T)), breaks=c(.05,.1,.5,1,2),labels=c(.05,.1,.5,1,2)) +
  scale_y_continuous(limits = c(min(data$m.st, na.rm = T), max(data$m.st, na.rm = T)), breaks=c(1,5,10,50,100,500,1000,5000,10000,50000),labels=c(1,5,10,50,100,500,1000,5000,10000,50000)) +
  scale_colour_manual(breaks=c("FW", "PM"), labels=c("Field Wild", "Plantation Managed"), values=c("red","green")) +
  theme_bw() + scale_shape_manual(values = c(46, 49, 50, 51, 52, 53, 54)) +
  theme(legend.position = c(0.15,0.7),legend.direction = "vertical") +
  theme(legend.key.height=unit(0.7,"line")) +
  guides(colour=FALSE) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

# data = data[!is.na(data$m.br),]
data = data[!data$m.br==0, ]

plot[[3]] = ggplot(data, aes(x=d.bh, y=m.br, group = interaction(growingCondition,datatype), colour=growingCondition, shape=plot)) + 
  geom_point(position=pd, size = 1.5) + 
  coord_trans(y = "log10", x = "log10") +
  ylab("Branch mass (kg)") + xlab("DBH (m)") +
  # labs(colour="Growing Condition", shape="Data type") +
  scale_x_continuous(limits = c(min(data$d.bh, na.rm = T), max(data$d.bh, na.rm = T)), breaks=c(.02,.05,.1,.2,.5,1)) +
  scale_y_continuous(limits = c(min(data$m.br, na.rm = T), max(data$m.br, na.rm = T)), breaks=c(.5,1,5,10,50,100,500,1000),labels=c(.5,1,5,10,50,100,500,1000)) +
  scale_colour_manual(breaks=c("FW", "PM"), labels=c("Field Wild", "Plantation Managed"), values=c("red","green")) +
  theme_bw() + scale_shape_manual(values = c(46, 49, 50, 51, 52, 53, 54)) +
  # theme(legend.position = c(0.8,0.15),legend.direction = "vertical") +
  # theme(legend.key.height=unit(0.7,"line")) +
  guides(colour=FALSE,shape=FALSE) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

data$br_to_st =  data$m.br / data$m.st
plot[[4]] = ggplot(data, aes(x=d.bh, y=br_to_st, group = interaction(growingCondition,datatype), colour=growingCondition, shape=plot)) +
  geom_point(position=pd, size = 1.5) + 
  coord_trans(y = "log10", x = "log10") +
  ylab("Branch mass / Stem mass") + xlab("DBH (m)") +
  # labs(colour="Growing Condition", shape="Data type") +
  scale_x_continuous(limits = c(min(data$d.bh, na.rm = T), max(data$d.bh[!is.na(data$m.br)], na.rm = T)), breaks=c(.02,.05,.1,.2)) +
  scale_y_continuous(limits = c(min(data$br_to_st, na.rm = T), max(data$br_to_st, na.rm = T)), breaks=c(.1,.5,1,5),labels=c(.1,.5,1,5)) +
  scale_colour_manual(breaks=c("FW", "PM"), labels=c("Field Wild", "Plantation Managed"), values=c("red","green")) +
  theme_bw() + scale_shape_manual(values = c(46, 49, 50, 51, 52, 53, 54)) +
  # theme(legend.position = c(0.2,0.15),legend.direction = "vertical") +
  # theme(legend.key.height=unit(0.7,"line")) +
  guides(colour=FALSE,shape=FALSE) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

pdf(file = "output/1.mass_vs_dbh_with_growingcondition_logscale.pdf")
# plot
do.call(grid.arrange,  plot)
dev.off()

do.call(grid.arrange,  plot)
# ----------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------
# Plot biomass against DBH with different growing conditions in normal scale
# cbPalette = rainbow(nlevels(baad_data_euc$growingCondition))[rank(1:nlevels(baad_data_euc$growingCondition))]
cbPalette = c("gray", "orange", "skyblue", "green3", "yellow3", "#0072B2", "#D55E00")
font.size = 10
pd <- position_dodge(0)
plot = list() 
plot[[1]] = ggplot(data, aes(x=d.bh, y=m.so, group = interaction(growingCondition,datatype), colour=growingCondition, shape=plot)) + 
  geom_point(position=pd, size = 1.5) + 
  # coord_trans(y = "log10", x = "log10") +
  ylab("Above-ground wood mass (kg)") + xlab("DBH (m)") + 
  labs(colour="Growing Condition", shape="Data type") +
  scale_x_continuous(limits = c(min(data$d.bh, na.rm = T), max(data$d.bh[!is.na(data$m.so)], na.rm = T))) +
  scale_y_continuous(limits = c(min(data$m.so, na.rm = T), max(data$m.so, na.rm = T))) +
  # scale_x_continuous(limits = c(min(data$d.bh, na.rm = T), max(data$d.bh, na.rm = T)), breaks=c(.05,.1,.5,1,2),labels=c(.05,.1,.5,1,2)) +
  # scale_y_continuous(limits = c(min(data$m.so, na.rm = T), max(data$m.so, na.rm = T)), breaks=c(1,5,10,50,100,500,1000,5000,10000,50000),labels=c(1,5,10,50,100,500,1000,5000,10000,50000)) +
  scale_colour_manual(breaks=c("FW", "PM"), labels=c("Field Wild", "Plantation Managed"), values=c("red","green")) +
  theme_bw() + scale_shape_manual(values = c(46, 49, 50, 51, 52, 53, 54)) +
  theme(legend.position = c(0.25,0.85),legend.direction = "vertical") +
  theme(legend.key.height=unit(0.7,"line")) +
  guides(shape=FALSE) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

plot[[2]] = ggplot(data, aes(x=d.bh, y=m.st, group = interaction(growingCondition,datatype), colour=growingCondition, shape=plot)) + 
  geom_point(position=pd, size = 1.5) + 
  # coord_trans(y = "log10", x = "log10") +
  ylab("Stem mass (kg)") + xlab("DBH (m)") + 
  labs(colour="Growing Condition", shape="Data type") +
  scale_x_continuous(limits = c(min(data$d.bh, na.rm = T), max(data$d.bh[!is.na(data$m.st)], na.rm = T))) +
  scale_y_continuous(limits = c(min(data$m.st, na.rm = T), max(data$m.st, na.rm = T))) +
  scale_colour_manual(breaks=c("FW", "PM"), labels=c("Field Wild", "Plantation Managed"), values=c("red","green")) +
  theme_bw() + scale_shape_manual(values = c(46, 49, 50, 51, 52, 53, 54)) +
  theme(legend.position = c(0.15,0.7),legend.direction = "vertical") +
  theme(legend.key.height=unit(0.7,"line")) +
  guides(colour=FALSE) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

# data = data[!is.na(data$m.br),]
data = data[!data$m.br==0, ]

plot[[3]] = ggplot(data, aes(x=d.bh, y=m.br, group = interaction(growingCondition,datatype), colour=growingCondition, shape=plot)) + 
  geom_point(position=pd, size = 1.5) + 
  # coord_trans(y = "log10", x = "log10") +
  ylab("Branch mass (kg)") + xlab("DBH (m)") +
  # labs(colour="Growing Condition", shape="Data type") +
  scale_x_continuous(limits = c(min(data$d.bh, na.rm = T), max(data$d.bh[!is.na(data$m.br)], na.rm = T))) +
  scale_y_continuous(limits = c(min(data$m.br, na.rm = T), max(data$m.br, na.rm = T))) +
  scale_colour_manual(breaks=c("FW", "PM"), labels=c("Field Wild", "Plantation Managed"), values=c("red","green")) +
  theme_bw() + scale_shape_manual(values = c(46, 49, 50, 51, 52, 53, 54)) +
  # theme(legend.position = c(0.8,0.15),legend.direction = "vertical") +
  # theme(legend.key.height=unit(0.7,"line")) +
  guides(colour=FALSE,shape=FALSE) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

data$br_to_st =  data$m.br / data$m.st
plot[[4]] = ggplot(data, aes(x=d.bh, y=br_to_st, group = interaction(growingCondition,datatype), colour=growingCondition, shape=plot)) +
  geom_point(position=pd, size = 1.5) + 
  # coord_trans(y = "log10", x = "log10") +
  ylab("Branch mass / Stem mass") + xlab("DBH (m)") +
  # labs(colour="Growing Condition", shape="Data type") +
  scale_x_continuous(limits = c(min(data$d.bh, na.rm = T), max(data$d.bh[!is.na(data$m.br)], na.rm = T))) +
  scale_y_continuous(limits = c(min(data$br_to_st, na.rm = T), max(data$br_to_st, na.rm = T))) +
  scale_colour_manual(breaks=c("FW", "PM"), labels=c("Field Wild", "Plantation Managed"), values=c("red","green")) +
  theme_bw() + scale_shape_manual(values = c(46, 49, 50, 51, 52, 53, 54)) +
  # theme(legend.position = c(0.2,0.15),legend.direction = "vertical") +
  # theme(legend.key.height=unit(0.7,"line")) +
  guides(colour=FALSE,shape=FALSE) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

pdf(file = "output/1.mass_vs_dbh_with_growingcondition.pdf")
# plot
do.call(grid.arrange,  plot)
dev.off()

do.call(grid.arrange,  plot)
# ----------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------
# Plot biomass against DBH with different growing conditions in normal scale
# read the allometry-based AGB
wood_pool_allometry = read.csv("output/wood_pool_allometry.csv")

par(mfrow = c(2, 1))
# with(data[data$datatype %in% as.factor("BAAD"),], plot(d.bh,m.so,col="grey",main="DBH vs AGB", pch=1, 
#      xlim=c(min(data$d.bh, na.rm = T), max(data$d.bh[!is.na(data$m.so)], na.rm = T)), ylim=c(min(data$m.so, na.rm = T), max(data$m.so, na.rm = T)),
#      ylab="AGB(kg)", xlab="DBH(m)"))
with(data[data$datatype %in% as.factor("BAAD"),], plot(d.bh,m.so,col="grey",main="DBH vs AGB", pch=1, 
                                                       xlim=c(0, 0.65), ylim=c(0, 2000),
                                                       ylab="AGB(kg)", xlab="DBH(m)"))
lines(wood_pool_allometry$diam/100,wood_pool_allometry$biom,col="green",type="p",pch='*')
legend('topleft', c("Allomerty-based", "BAAD"), bty='n', col=c('green','grey'),cex=0.8,lty=1,lwd=2,seg.len=0.75,y.intersp=0.75)


with(data[data$datatype %in% as.factor("BAAD"),], plot(d.bh,m.so,col="grey",main="DBH vs AGB", pch=1, 
                                                       xlim=c(0, 0.65), ylim=c(0, 2000),
                                                       ylab="AGB(kg)", xlab="DBH(m)"))
with(data[data$datatype %in% as.factor(c("Lidar")),], lines(d.bh,m.so,col="red",type="p",pch='+'))
legend('topleft', c("Lidar-based", "BAAD"), bty='n', col=c('red','grey'),cex=0.8,lty=1,lwd=2,seg.len=0.75,y.intersp=0.75)

plots[[5]] = recordPlot()
pdf(file = "output/6.AGB_vs_dbh_all_data.pdf")
plots[[5]]
dev.off()
# ----------------------------------------------------------------------------------








