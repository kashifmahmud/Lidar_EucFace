# ----------------------------------------------------------------------------------
wood.density = 0.82 # to calculate mass from wood volume (Zanne et al) - Unit is g cm-3 or Kg/L

# ----------------------------------------------------------------------------------
# read plot 1 lidar data 
# getwd()
# tree21 = read.mat('lidar_data/results/ModelData_ring6_tree_22_2.mat')
# tree21$ModelData[[1]][[3]][c(1:16,41)]

# dataFiles <- lapply(Sys.glob("lidar_data/results_plot1/ModelData_tree_*.mat"), readMat)
myFiles <- list.files("lidar_data/results", pattern="*.mat",full.names = TRUE)
myFiles <- mixedsort(myFiles)
dataFiles<-lapply(myFiles,readMat)
# dataFiles[[2]]$ModelData[[3]][[1]][c(1:16,41)]
iter = 3; plot1_tree = 30; plot2_tree = 35; plot3_tree = 36; plot4_tree = 41; plot5_tree = 42; plot6_tree = 22 
total_tree = plot1_tree + plot2_tree + plot3_tree + plot4_tree + plot5_tree + plot6_tree

# iter = 3; plot1_tree = 0; plot2_tree = 0; plot3_tree = 0; plot4_tree = 0; plot5_tree = 0; plot6_tree = 22
# total_tree = plot1_tree + plot2_tree + plot3_tree + plot4_tree + plot5_tree + plot6_tree
lidar_data_euc = data.frame(matrix(vector(), total_tree*iter, 17, dimnames=list(c(), c("Total.volume", "Trunk.volume", 
                                                                            "Branch.volume", "Total.height", "Trunk.length", "Branch.length", 
                                                                            "Number.of.branches", "Maximum.branch.order", "Total.cylinder.area", "Dbh.QSM", "Dbh.cylinder", 
                                                                            "Dbh.triangulation", "Triangulated.trunk.volume", "Triangulated.trunk.length", "Mixed.trunk.volume", 
                                                                            "Mixed.total.volume", "Trunk.surface.area"))), stringsAsFactors=F)
# UNITS = volumes are in L; DBH in cm; rest are in m;
for (i in 1:(total_tree*iter)) {
  lidar_data_euc[i, ] = dataFiles[[i]]$ModelData[[3]][[1]][c(1:16,41)]
}
lidar_data_euc$plot = as.factor(c(rep("plot 1",plot1_tree*iter), rep("plot 2",plot2_tree*iter), rep("plot 3",plot3_tree*iter), 
                                  rep("plot 4",plot4_tree*iter), rep("plot 5",plot5_tree*iter), rep("plot 6",plot6_tree*iter)))
# lidar_data_euc$tree = c(rep(seq(plot1_tree),each=iter), rep(seq(plot2_tree),each=iter), rep(seq(plot5_tree),each=iter))
lidar_data_euc$tree = c(rep(seq(plot1_tree),each=iter), rep(seq(plot2_tree),each=iter), rep(seq(plot3_tree),each=iter),
                        rep(seq(plot4_tree),each=iter), rep(seq(plot5_tree),each=iter), rep(seq(plot6_tree),each=iter))
lidar_data_euc$CO2 = as.factor(c(rep("ele",plot1_tree*iter), rep("amb",plot2_tree*iter), rep("amb",plot3_tree*iter), 
                                 rep("ele",plot4_tree*iter), rep("ele",plot5_tree*iter), rep("amb",plot6_tree*iter)))

lidar_data_euc$Total.mass = lidar_data_euc$Total.volume * wood.density # Unit = Kg
lidar_data_euc$Branch.mass = lidar_data_euc$Branch.volume * wood.density # Unit = Kg
lidar_data_euc$Stem.mass = lidar_data_euc$Trunk.volume * wood.density # Unit = Kg

keeps <- c("Dbh.QSM", "Stem.mass", "Branch.mass", "Total.mass", "Total.height", "Total.cylinder.area", "Trunk.surface.area", "plot", "tree", "CO2")
lidar_data_euc_mean = lidar_data_euc[ , keeps, drop = FALSE]
# write.csv(lidar_data_euc, file = "output/lidar_data_eucface.csv", row.names = FALSE)

names(lidar_data_euc_mean) = c("d.bh", "m.st", "m.br", "m.so", "height", "wood.area", "stem.area", "plot", "tree", "CO2")

lidar_data_euc_mean <- summaryBy(d.bh+m.st+m.br+m.so+height+wood.area+stem.area ~ plot+CO2+tree, data=lidar_data_euc_mean, FUN=c(mean,standard.error), keep.names=T, na.rm=T ) # Mean of all iterations
names(lidar_data_euc_mean) = c("plot","CO2","tree","d.bh_mean","m.st_mean","m.br_mean","m.so_mean","height_mean","wood.area_mean","stem.area_mean","d.bh_sd","m.st_sd","m.br_sd","m.so_sd","height_sd","wood.area_sd","stem.area_sd")

# ----------------------------------------------------------------------------------
# Discarding the trees with no canopy, covered by mosses, and do not seem to be reasonable
# remove trees form plot #1
lidar_data_euc_mean_sub = lidar_data_euc_mean[!(lidar_data_euc_mean$plot %in% as.factor("plot 1") & lidar_data_euc_mean$tree %in% c(5,15,16,17,27)), ]
# remove trees form plot #2
lidar_data_euc_mean_sub = lidar_data_euc_mean_sub[!(lidar_data_euc_mean_sub$plot %in% as.factor("plot 2") & lidar_data_euc_mean_sub$tree %in% c(1,2,7,12,23,24,25)), ]
# remove trees form plot #3
lidar_data_euc_mean_sub = lidar_data_euc_mean_sub[!(lidar_data_euc_mean_sub$plot %in% as.factor("plot 3") & lidar_data_euc_mean_sub$tree %in% c(1,2,6,8,18,23)), ]
# remove trees form plot #4
lidar_data_euc_mean_sub = lidar_data_euc_mean_sub[!(lidar_data_euc_mean_sub$plot %in% as.factor("plot 4") & lidar_data_euc_mean_sub$tree %in% c(2,5,6,8,14,18,23,25)), ]
# remove trees form plot #5
lidar_data_euc_mean_sub = lidar_data_euc_mean_sub[!(lidar_data_euc_mean_sub$plot %in% as.factor("plot 5") & lidar_data_euc_mean_sub$tree %in% c(10,21,23,26,30,37,40)), ]
# remove trees form plot #6
lidar_data_euc_mean_sub = lidar_data_euc_mean_sub[!(lidar_data_euc_mean_sub$plot %in% as.factor("plot 6") & lidar_data_euc_mean_sub$tree %in% c(8)), ]

                             
# # ----------------------------------------------------------------------------------
# # ----------------------------------------------------------------------------------
# 
# myFiles <- list.files("lidar_data/results_plot1_all", pattern="*.mat",full.names = TRUE)
# myFiles <- mixedsort(myFiles)
# dataFiles<-lapply(myFiles,readMat)
# # dataFiles[[2]]$ModelData[[3]][[1]][c(1:16,41)]
# 
# lidar_data_euc_1 = data.frame(matrix(vector(), 25, 17, dimnames=list(c(), c("Total.volume", "Trunk.volume", 
#                                                                             "Branch.volume", "Total.height", "Trunk.length", "Branch.length", 
#                                                                             "Number.of.branches", "Maximum.branch.order", "Total.cylinder.area", "Dbh.QSM", "Dbh.cylinder", 
#                                                                             "Dbh.triangulation", "Triangulated.trunk.volume", "Triangulated.trunk.length", "Mixed.trunk.volume", 
#                                                                             "Mixed.total.volume", "Trunk.surface.area"))), stringsAsFactors=F)
# # UNITS = volumes are in L; DBH in cm; rest are in m;
# for (i in 1:25) {
#   lidar_data_euc_1[i, ] = dataFiles[[i]]$ModelData[[3]][[1]][c(1:16,41)]
# }
# lidar_data_euc_1$plot = as.factor("plot 1")
# lidar_data_euc_1$tree = c(1:4,6:14,18:26,28:30)
# # lidar_data_euc_1$tree = c(1,10:14,18,19,2,20:26,28,29,3,30,4,6:9)
# lidar_data_euc_1$CO2 = as.factor("ele")
# # # ----------------------------------------------------------------------------------
# # myFiles <- list.files("lidar_data/results_plot1_dbh", pattern="*.mat",full.names = TRUE)
# # myFiles <- mixedsort(myFiles)
# # dataFiles<-lapply(myFiles,readMat)
# # 
# # # UNITS = volumes are in L; DBH in cm; rest are in m;
# # for (i in 1:25) {
# #   lidar_data_euc_1[i,c(10:12)] = dataFiles[[i]]$ModelData[[3]][[1]][c(10:12)]
# # }
# # ----------------------------------------------------------------------------------
# # read plot 2 lidar data
# # dataFiles <- lapply(Sys.glob("lidar_data/results_plot2/ModelData_tree_*.mat"), readMat)
# myFiles <- list.files("lidar_data/results_plot2_dbh", pattern="*.mat",full.names = TRUE)
# myFiles <- mixedsort(myFiles)
# dataFiles<-lapply(myFiles,readMat)
# 
# lidar_data_euc_2 = data.frame(matrix(vector(), 35, 17, dimnames=list(c(), c("Total.volume", "Trunk.volume",
#                                                                             "Branch.volume", "Total.height", "Trunk.length", "Branch.length",
#                                                                             "Number.of.branches", "Maximum.branch.order", "Total.cylinder.area", "Dbh.QSM", "Dbh.cylinder",
#                                                                             "Dbh.triangulation", "Triangulated.trunk.volume", "Triangulated.trunk.length", "Mixed.trunk.volume",
#                                                                             "Mixed.total.volume", "Trunk.surface.area"))), stringsAsFactors=F)
# # UNITS = volumes are in L; DBH in cm; rest are in m;
# for (i in 1:35) {
#   lidar_data_euc_2[i, ] = dataFiles[[i]]$ModelData[[3]][[1]][c(1:16,41)]
# }
# lidar_data_euc_2$plot = as.factor("plot 2")
# lidar_data_euc_2$tree = 1:35
# lidar_data_euc_2$CO2 = as.factor("amb")
# 
# # # ----------------------------------------------------------------------------------
# # myFiles <- list.files("lidar_data/results_plot2_dbh", pattern="*.mat",full.names = TRUE)
# # myFiles <- mixedsort(myFiles)
# # dataFiles<-lapply(myFiles,readMat)
# # 
# # # UNITS = volumes are in L; DBH in cm; rest are in m;
# # for (i in 1:35) {
# #   lidar_data_euc_2[i,c(10:12)] = dataFiles[[i]]$ModelData[[3]][[1]][c(10:12)]
# # }
# 
# # ----------------------------------------------------------------------------------
# # ----------------------------------------------------------------------------------
# # read plot 3 lidar data
# # dataFiles <- lapply(Sys.glob("lidar_data/results_plot3/ModelData_tree_*.mat"), readMat)
# myFiles <- list.files("lidar_data/results_plot3_1", pattern="*.mat",full.names = TRUE)
# myFiles <- mixedsort(myFiles)
# dataFiles<-lapply(myFiles,readMat)
# 
# lidar_data_euc_3 = data.frame(matrix(vector(), 36, 17, dimnames=list(c(), c("Total.volume", "Trunk.volume",
#                                                                             "Branch.volume", "Total.height", "Trunk.length", "Branch.length",
#                                                                             "Number.of.branches", "Maximum.branch.order", "Total.cylinder.area", "Dbh.QSM", "Dbh.cylinder",
#                                                                             "Dbh.triangulation", "Triangulated.trunk.volume", "Triangulated.trunk.length", "Mixed.trunk.volume",
#                                                                             "Mixed.total.volume", "Trunk.surface.area"))), stringsAsFactors=F)
# # UNITS = volumes are in L; DBH in cm; rest are in m;
# for (i in 1:36) {
#   lidar_data_euc_3[i, ] = dataFiles[[i]]$ModelData[[3]][[1]][c(1:16,41)]
# }
# lidar_data_euc_3$plot = as.factor("plot 3")
# lidar_data_euc_3$tree = 1:36
# lidar_data_euc_3$CO2 = as.factor("amb")
# # # ----------------------------------------------------------------------------------
# # myFiles <- list.files("lidar_data/results_plot3_dbh", pattern="*.mat",full.names = TRUE)
# # myFiles <- mixedsort(myFiles)
# # dataFiles<-lapply(myFiles,readMat)
# # 
# # # UNITS = volumes are in L; DBH in cm; rest are in m;
# # for (i in 1:36) {
# #   lidar_data_euc_3[i,c(10:12)] = dataFiles[[i]]$ModelData[[3]][[1]][c(10:12)]
# # }
# # ----------------------------------------------------------------------------------
# # ----------------------------------------------------------------------------------
# # read plot 4 lidar data
# # dataFiles <- lapply(Sys.glob("lidar_data/results_plot4/ModelData_tree_*.mat"), readMat)
# myFiles <- list.files("lidar_data/results_plot4", pattern="*.mat",full.names = TRUE)
# myFiles <- mixedsort(myFiles)
# dataFiles<-lapply(myFiles,readMat)
# 
# lidar_data_euc_4 = data.frame(matrix(vector(), 41, 17, dimnames=list(c(), c("Total.volume", "Trunk.volume",
#                                                                             "Branch.volume", "Total.height", "Trunk.length", "Branch.length",
#                                                                             "Number.of.branches", "Maximum.branch.order", "Total.cylinder.area", "Dbh.QSM", "Dbh.cylinder",
#                                                                             "Dbh.triangulation", "Triangulated.trunk.volume", "Triangulated.trunk.length", "Mixed.trunk.volume",
#                                                                             "Mixed.total.volume", "Trunk.surface.area"))), stringsAsFactors=F)
# # UNITS = volumes are in L; DBH in cm; rest are in m;
# for (i in 1:41) {
#   lidar_data_euc_4[i, ] = dataFiles[[i]]$ModelData[[3]][[1]][c(1:16,41)]
# }
# lidar_data_euc_4$plot = as.factor("plot 4")
# lidar_data_euc_4$tree = 1:41
# lidar_data_euc_4$CO2 = as.factor("ele")
# # # ----------------------------------------------------------------------------------
# # myFiles <- list.files("lidar_data/results_plot4_dbh", pattern="*.mat",full.names = TRUE)
# # myFiles <- mixedsort(myFiles)
# # dataFiles<-lapply(myFiles,readMat)
# # 
# # # UNITS = volumes are in L; DBH in cm; rest are in m;
# # for (i in 1:41) {
# #   lidar_data_euc_4[i,c(10:12)] = dataFiles[[i]]$ModelData[[3]][[1]][c(10:12)]
# # }
# # ----------------------------------------------------------------------------------
# # ----------------------------------------------------------------------------------
# # read plot 5 lidar data
# # dataFiles <- lapply(Sys.glob("lidar_data/results_plot5/ModelData_tree_*.mat"), readMat)
# myFiles <- list.files("lidar_data/results_plot5", pattern="*.mat",full.names = TRUE)
# myFiles <- mixedsort(myFiles)
# dataFiles<-lapply(myFiles,readMat)
# 
# lidar_data_euc_5 = data.frame(matrix(vector(), 41, 17, dimnames=list(c(), c("Total.volume", "Trunk.volume",
#                                                                             "Branch.volume", "Total.height", "Trunk.length", "Branch.length",
#                                                                             "Number.of.branches", "Maximum.branch.order", "Total.cylinder.area", "Dbh.QSM", "Dbh.cylinder",
#                                                                             "Dbh.triangulation", "Triangulated.trunk.volume", "Triangulated.trunk.length", "Mixed.trunk.volume",
#                                                                             "Mixed.total.volume", "Trunk.surface.area"))), stringsAsFactors=F)
# # UNITS = volumes are in L; DBH in cm; rest are in m;
# for (i in 1:41) {
#   lidar_data_euc_5[i, ] = dataFiles[[i]]$ModelData[[3]][[1]][c(1:16,41)]
# }
# lidar_data_euc_5$plot = as.factor("plot 5")
# lidar_data_euc_5$tree = c(1:9,11:42)
# lidar_data_euc_5$CO2 = as.factor("ele")
# # ----------------------------------------------------------------------------------
# myFiles <- list.files("lidar_data/results_plot5_dbh", pattern="*.mat",full.names = TRUE)
# myFiles <- mixedsort(myFiles)
# dataFiles<-lapply(myFiles,readMat)
# 
# # UNITS = volumes are in L; DBH in cm; rest are in m;
# for (i in 1:41) {
#   lidar_data_euc_5[i,c(10:12)] = dataFiles[[i]]$ModelData[[3]][[1]][c(10:12)]
# }
# 
# # ----------------------------------------------------------------------------------
# # ----------------------------------------------------------------------------------
# # read plot 6 lidar data
# # dataFiles <- lapply(Sys.glob("lidar_data/results_plot6/ModelData_tree_*.mat"), readMat)
# myFiles <- list.files("lidar_data/results_plot6", pattern="*.mat",full.names = TRUE)
# myFiles <- mixedsort(myFiles)
# dataFiles<-lapply(myFiles,readMat)
# 
# lidar_data_euc_6 = data.frame(matrix(vector(), 22, 17, dimnames=list(c(), c("Total.volume", "Trunk.volume",
#                                                                             "Branch.volume", "Total.height", "Trunk.length", "Branch.length",
#                                                                             "Number.of.branches", "Maximum.branch.order", "Total.cylinder.area", "Dbh.QSM", "Dbh.cylinder",
#                                                                             "Dbh.triangulation", "Triangulated.trunk.volume", "Triangulated.trunk.length", "Mixed.trunk.volume",
#                                                                             "Mixed.total.volume", "Trunk.surface.area"))), stringsAsFactors=F)
# # UNITS = volumes are in L; DBH in cm; rest are in m;
# for (i in 1:22) {
#   lidar_data_euc_6[i, ] = dataFiles[[i]]$ModelData[[3]][[1]][c(1:16,41)]
# }
# lidar_data_euc_6$plot = as.factor("plot 6")
# lidar_data_euc_6$tree = 1:22
# lidar_data_euc_6$CO2 = as.factor("amb")
# # ----------------------------------------------------------------------------------
# myFiles <- list.files("lidar_data/results_plot6_dbh", pattern="*.mat",full.names = TRUE)
# myFiles <- mixedsort(myFiles)
# dataFiles<-lapply(myFiles,readMat)
# 
# # UNITS = volumes are in L; DBH in cm; rest are in m;
# for (i in 1:22) {
#   lidar_data_euc_6[i,c(10:12)] = dataFiles[[i]]$ModelData[[3]][[1]][c(10:12)]
# }
# ----------------------------------------------------------------------------------
# Preprocess the Lidar data
# Merge all lidar plot data into one file
# lidar_data_euc = rbind(lidar_data_euc_1,lidar_data_euc_2,lidar_data_euc_3,lidar_data_euc_4,lidar_data_euc_5,lidar_data_euc_6)

# # Testing
# lidar_data_euc = lidar_data_euc_2

# lidar_data_euc$Total.mass = lidar_data_euc$Total.volume * wood.density # Unit = Kg
# lidar_data_euc$Branch.mass = lidar_data_euc$Branch.volume * wood.density # Unit = Kg
# lidar_data_euc$Stem.mass = lidar_data_euc$Trunk.volume * wood.density # Unit = Kg

# keeps <- c("Dbh.QSM", "Stem.mass", "Total.mass", "Branch.mass", "Total.cylinder.area", "Trunk.surface.area", "plot", "tree", "CO2")
# lidar_data_euc = lidar_data_euc[ , keeps, drop = FALSE]

keeps <- c("plot", "tree", "CO2", "d.bh_mean", "m.st_mean", "m.so_mean", "m.br_mean", "wood.area_mean", "stem.area_mean")
lidar_data_euc_mean_sub_1 = lidar_data_euc_mean_sub[ , keeps, drop = FALSE]

names(lidar_data_euc_mean_sub_1) = c("plot", "tree", "CO2", "d.bh", "m.st", "m.so", "m.br", "wood.area", "stem.area")
lidar_data_euc_mean_sub_1$d.bh = lidar_data_euc_mean_sub_1$d.bh/100 # unit conversion: cm to m
lidar_data_euc_mean_sub_1$species = as.character("Eucalyptus")
lidar_data_euc_mean_sub_1$age = as.numeric(30)
lidar_data_euc_mean_sub_1$growingCondition = as.factor("FW")
lidar_data_euc_mean_sub_1$datatype = as.factor("Lidar")

# ----------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------
#- Read  BAAD data
baad_data = read.csv("baad_data/baad_data.csv") # Leaf count data (weekly measurements)
baad_data$m.so = baad_data$m.so - baad_data$m.lf

keeps <- c("species", "growingCondition", "age", "d.bh", "m.st", "m.so", "m.br")
baad_data = baad_data[ , keeps, drop = FALSE]
baad_data$datatype = as.factor("BAAD")
baad_data$plot = as.factor("BAAD")
baad_data$species = as.character(baad_data$species)
baad_data$CO2 = as.factor("BAAD")

baad_data_euc = baad_data %>% filter(str_detect(species, "Eucalyptus"))
baad_data_euc = baad_data_euc[!is.na(baad_data_euc$d.bh),]
# baad_data_euc = baad_data_euc[!is.na(baad_data_euc$age),]

# ----------------------------------------------------------------------------------
# combine both lidar and BAAD datasets
data = rbind(baad_data_euc, lidar_data_euc_mean_sub_1[,c("species", "growingCondition", "age", "d.bh", "m.st", "m.so", "m.br", "datatype", "plot", "CO2")])

# ----------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------






