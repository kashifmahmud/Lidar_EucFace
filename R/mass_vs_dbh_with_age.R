# ----------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------
# Plot biomass against DBH with plant age
baad_data_euc = baad_data %>% filter(str_detect(species, "Eucalyptus"))
baad_data_euc = baad_data_euc[!is.na(baad_data_euc$age),]
baad_data_euc$age.group = "old (>75yr)"
baad_data_euc$age.group[baad_data_euc$age <= 1] = "seedling (0-1yr)"
baad_data_euc$age.group[baad_data_euc$age > 1 & baad_data_euc$age <= 3] = "sapling (1-3yr)"
baad_data_euc$age.group[baad_data_euc$age > 3 & baad_data_euc$age <= 10] = "young (3-10yr)"
baad_data_euc$age.group[baad_data_euc$age > 10 & baad_data_euc$age <= 75] = "mature (10-75yr)"
baad_data_euc$age.group = as.factor(baad_data_euc$age.group)
lidar_data_euc$age.group = as.factor("mature (10-75yr)")
data = rbind(baad_data_euc,lidar_data_euc[,c("species", "growingCondition", "age", "d.bh", "m.st", "m.so", "m.br", "datatype", "plot", "CO2", "age.group")])

# cbPalette = rainbow(nlevels(data$age.group))[rank(1:nlevels(data$age.group))]
cbPalette = c("orange", "skyblue", "green3", "#0072B2", "#D55E00")
plot = list() 

plot[[1]] = ggplot(data, aes(x=d.bh, y=m.so, group = interaction(age.group,datatype), colour=age.group, shape=plot)) +
  geom_point(position=pd, size = 1.5) + coord_trans(y = "log10", x = "log10") +
  ylab("Above-ground wood mass (kg)") + xlab("DBH (m)") +
  labs(colour="Tree age group", shape="Data type") +
  scale_x_continuous(limits = c(min(data$d.bh, na.rm = T), max(data$d.bh, na.rm = T)), breaks=c(.02,.05,.1,.2,.5,1,2)) +
  scale_y_continuous(limits = c(min(data$m.so, na.rm = T), max(data$m.so, na.rm = T)), breaks=c(1,5,10,50,100,500,1000,5000,10000,50000),labels=c(1,5,10,50,100,500,1000,5000,10000,50000)) +
  scale_colour_manual(breaks=c("seedling (0-1yr)", "sapling (1-3yr)", "young (3-10yr)", "mature (10-75yr)", "old (>75yr)"), values=cbPalette) +
  theme_bw() + scale_shape_manual(values = c(46, 49, 50, 51, 52, 53, 54)) +
  theme(legend.position = c(0.22,0.75),legend.direction = "vertical") +
  theme(legend.key.height=unit(0.7,"line")) +
  guides(shape=FALSE) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plot[[2]] = ggplot(data, aes(x=d.bh, y=m.st, group = interaction(age.group,datatype), colour=age.group, shape=plot)) +
  geom_point(position=pd, size = 1.5) + coord_trans(y = "log10", x = "log10") +
  ylab("Stem mass (kg)") + xlab("DBH (m)") +
  labs(colour="Tree age group", shape="Data type") +
  scale_x_continuous(limits = c(min(data$d.bh, na.rm = T), max(data$d.bh, na.rm = T)), breaks=c(.02,.05,.1,.2,.5,1,2)) +
  scale_y_continuous(limits = c(min(data$m.st, na.rm = T), max(data$m.st, na.rm = T)), breaks=c(1,5,10,50,100,500,1000,5000,10000,50000),labels=c(1,5,10,50,100,500,1000,5000,10000,50000)) +
  scale_colour_manual(breaks=c("seedling (0-1yr)", "sapling (1-3yr)", "young (3-10yr)", "mature (10-75yr)", "old (>75yr)"), values=cbPalette) +
  theme_bw() + scale_shape_manual(values = c(46, 49, 50, 51, 52, 53, 54)) +
  theme(legend.position = c(0.15,0.7),legend.direction = "vertical") +
  theme(legend.key.height=unit(0.7,"line")) +
  guides(colour=FALSE) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plot[[3]] = ggplot(data, aes(x=d.bh, y=m.br, group = interaction(age.group,datatype), colour=age.group, shape=plot)) +
  geom_point(position=pd, size = 1.5) + coord_trans(y = "log10", x = "log10") +
  ylab("Branch mass (kg)") + xlab("DBH (m)") +
  # labs(colour="Tree age group", shape="Data type") +
  scale_x_continuous(limits = c(min(data$d.bh, na.rm = T), max(data$d.bh[!is.na(data$m.br)], na.rm = T)), breaks=c(.02,.05,.1,.2,.5,1)) +
  scale_y_continuous(limits = c(min(data$m.br, na.rm = T), max(data$m.br, na.rm = T)), breaks=c(.5,1,5,10,50,100,500,1000),labels=c(.5,1,5,10,50,100,500,1000)) +
  scale_colour_manual(breaks=c("seedling (0-1yr)", "sapling (1-3yr)", "young (3-10yr)", "mature (10-75yr)", "old (>75yr)"), values=cbPalette) +
  theme_bw() + scale_shape_manual(values = c(46, 49, 50, 51, 52, 53, 54)) +
  # theme(legend.position = c(0.8,0.15),legend.direction = "vertical") +
  # theme(legend.key.height=unit(0.7,"line")) +
  guides(colour=FALSE,shape=FALSE) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

data$br_to_st =  data$m.br / data$m.st
plot[[4]] = ggplot(data, aes(x=d.bh, y=br_to_st, group = interaction(age.group,datatype), colour=age.group, shape=plot)) +
  geom_point(position=pd, size = 1.5) + 
  coord_trans(y = "log10", x = "log10") +
  ylab("Branch mass / Stem mass") + xlab("DBH (m)") +
  # labs(colour="Tree age group", shape="Data type") +
  scale_x_continuous(limits = c(min(data$d.bh, na.rm = T), max(data$d.bh[!is.na(data$m.br)], na.rm = T)), breaks=c(.02,.05,.1,.2)) +
  scale_y_continuous(limits = c(min(data$br_to_st, na.rm = T), max(data$br_to_st, na.rm = T)), breaks=c(.1,.5,1,5),labels=c(.1,.5,1,5)) +
  scale_colour_manual(breaks=c("seedling (0-1yr)", "sapling (1-3yr)", "young (3-10yr)", "mature (10-75yr)", "old (>75yr)"), values=cbPalette) +
  theme_bw() + scale_shape_manual(values = c(46, 49, 50, 51, 52, 53, 54)) +
  # theme(legend.position = c(0.2,0.15),legend.direction = "vertical") +
  # theme(legend.key.height=unit(0.7,"line")) +
  guides(colour=FALSE,shape=FALSE) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

pdf(file = "output/2.mass_vs_dbh_with_age.pdf")
# plot
do.call(grid.arrange,  plot)
dev.off()

do.call(grid.arrange,  plot)
# ----------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------






