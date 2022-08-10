library(tidyr)
library(dtwclust)
library(dplyr)
library(ggplot2)
library(reshape)
#read dateset (the testdata.csv that I sent)
Db <- read.csv("~/Desktop/ML-Hong/rawdata/testdata.csv")
# cut the unnecessary columns (no need for testdata)
Dbneat <- Db[,c(5,21,22)]
#check and change the variable type
str(Db)
Db2 <- mutate(Db,CellID = as.character(CellID))
# tranfer Date from character to numeric
Db2$Date <- as.Date.character(testdata$Date)
Db2$Date <- as.numeric(as.Date(as.character(testdata$Date)))
str(Db2)
#plot by Cellid 
Db2  %>% 
    ggplot(aes(x= Date, y= SAL_avg, color= CellID))+
    geom_line(size= 0.2)+
    ggtitle("Control chart sequences") +
    facet_wrap(~ CellID, scales = 'free_x', nrow = 2)
df_list <- as.list(utils::unstack(Db2, SAL_avg ~ CellID))
df_list_z <- dtwclust::zscore(df_list)
summary(df_list)
#hierarchical clustering with 10% window size for up to k=10 clusters
cluster_dtw_h <- list()
for (i in 2:10) # i is the number of clusters 
{cluster_dtw_h[[i]] <- tsclust(df_list_z, type = "h", k = i,  
                               distance = "dtw", 
                               control = hierarchical_control(method = "complete"), 
                               seed = 390, preproc = NULL, 
                               args = tsclust_args(dist = list(window.size = 3L)))
}
# take a look at the object
cluster_dtw_h[[10]]
# some cluster information
cluster_dtw_h[[4]]@clusinfo
# plot dendrogram for k= 4
plot(cluster_dtw_h[[4]])
#  The series and the obtained prototypes can be plotted too
plot(cluster_dtw_h[[]], type = "sc")
# the representative prototype 
plot(cluster_dtw_h[[4]], type = "centroid")


##reference :http://rstudio-pubs-static.s3.amazonaws.com/398402_abe1a0343a4e4e03977de8f3791e96bb.html





