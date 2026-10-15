dx <- read.csv("datasets/dat-neon-pele.csv")

length(unique(dx$tagID))
nrow(dx)

dx$grid <- paste(dx$siteID, dx$decimalLatitude, dx$decimalLongitude, sep="_")

sites <- sort(unique(dx$siteID))
nsite <- length(sites)
glist <- vector("list", length=nsite)

gr <- unique(dx[,c("siteID", "grid")])
gr$gridid <- NA

for(i in 1:nsite){
    flag <- which(gr$siteID == sites[i])
    nflag <- length(flag)
    gr$gridid[flag] <- paste(sites[i], 
                       formatC(1:nflag, width=2, flag="0"),
                       sep="_")
}
dx <- merge(dx, gr)
dx <- dx[order(dx$gridid),]
dx[,c(1,2,ncol(dx))]

cols <- c("siteID", "gridid", "nlcdClass",
          "decimalLatitude", "decimalLongitude",
          "tagID", "sex", 
          "hindfootLength", "earLength", "tailLength",
          "totalLength", "weight")
dz <- dx[,cols]
