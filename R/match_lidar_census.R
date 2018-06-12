# # Clear the workspace (if needed)
# rm(list=ls())

#-------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------
# read plot 1 lidar data 
##Read files named xyz1111.csv, xyz2222.csv, etc.
filenames <- list.files(path="lidar_data/results_plot1", pattern="tree_+.*pts")

##Create list of data frame names without the ".pts" part 
names <-substr(filenames,1,23)

###Load all files
for(i in names){
  filepath <- file.path("lidar_data/results_plot1",paste(i,sep=""))
  assign(i, read.csv(filepath, col.names=c("x","y","z","scalar"), sep = ""))
}

# dataFiles <- lapply(Sys.glob("lidar_data/results_plot1/tree_*.pts"), read.csv)
# # lidar_pts_euc_1 = data.frame(matrix(vector(), 30, 4, dimnames=list(c(), c("x", "y", "z", "intensity"))), stringsAsFactors=F)
# lidar_pts_euc_1 = list()
# # UNITS = volumes are in L; DBH in cm; rest are in m;
# for (i in 1:30) {
#   lidar_pts_euc_1[i] = dataFiles[[i]]
# }

# Import data
data_census = read.csv("lidar_data/FACE_AUX_RA_TREE-DESCRIPTIONS_R_20130201_matlab.csv",col.names=c("ring","tree","x","y","height","dbh"))
# m1 = dlmread('Cloud_eucface1_DBH.txt');
data_lidar = read.csv("lidar_data/dbh_section_all_trees_ring1 - Cloud.txt",col.names=c("x", "y", "z", "scalar"), sep = "")

# ----------------------------------------------------------------------------------
# find tree 101 in lidar dbh map
tree_101_sub = subset(tree_101_pointcloud.pts, z==1.3)
tree_101_x = mean(tree_101_sub$x)
tree_101_y = mean(tree_101_sub$y)

data_lidar_sub = data_lidar
data_lidar_sub$dist = ((data_lidar_sub$x-tree_101_x)^2 + (data_lidar_sub$y-tree_101_y)^2)^0.5
data_lidar$tree = exp
data_lidar$tree[min(data_lidar_sub$dist)] = as.factor(101)

ans <- vapply(data_lidar, function(x,y) ((x-tree_101_x)^2 + (y-tree_101_y)^2)^0.5, numeric(1))
indx <- apply(abs(ans), 2, which.min)
cbind(df1, df2[indx, ][-2])

y <- rbind(c(.1, .2),c(.11, .22), c(.3, .4), c(.31, .41), c(.32, 5))
x <- rbind(c(.09,.21), c(.29,.39))
y
x
w <- find.matches(x, y, maxmatch=5, tol=c(.05,.05))

set.seed(111)       # so can replicate results
x <- matrix(runif(500), ncol=2)
y <- matrix(runif(2000), ncol=2)
w <- find.matches(x, y, maxmatch=5, tol=c(.02,.03))
w$matches[1:5,]
w$distance[1:5,]
# Find first x with 3 or more y-matches
num.match <- apply(w$matches, 1, function(x)sum(x > 0))
j <- ((1:length(num.match))[num.match > 2])[1]
x[j,]
y[w$matches[j,],]
summary(w)


ans <- vapply(df1$Column_B, function(x) x-df2$Column_B, numeric(3))
indx <- apply(abs(ans), 2, which.min)
cbind(df1, df2[indx, ][-2])


min_dist = data_lidar$x - tree_101_sub$x

data_lidar_merge = merge(data_lidar,tree_101_sub,by=c("x","z"))

tree_102_sub = subset(tree_102_pointcloud.pts, z %in% 1.3)
data_lidar_merge = merge(data_lidar,tree_102_sub,by=c("x","y","z","scalar"))





