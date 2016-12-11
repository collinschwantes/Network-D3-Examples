library(networkD3)
library(jsonlite)
library(RJSONIO)
library(httr)
library(devtools)
library(dplyr)


#install_github("christophergandrud/networkD3")

flight_data <- list.files("./",pattern = "tableExport.csv",full.names = T)

fdf <- read.csv(file = flight_data)

pty_data <- list.files("./",pattern = "pty",full.names = T)

pty <- read.csv(file = pty_data)


mex_data <- list.files("./",pattern = "MEx",full.names = T)

mex <- read.csv(file = mex_data)

fdf <- rbind(fdf,pty,mex)

fdf <- fdf[fdf$Origin != "No data",]

fdf <- fdf[fdf$Total.Seats > 10000,]

fdf <-droplevels(fdf)

str(fdf)
# 
# levels(fdf$Origin) <- unique(c(levels(fdf$Destination),levels(fdf$Origin)))
# 
# levels(fdf$Destination) <- levels(fdf$Origin)
# 
# fdf[fdf$Origin == levels(fdf$Origin)[1],1] <- "MEX"
# fdf[fdf$Origin == levels(fdf$Origin)[2],1] <- "PTY"
# fdf[fdf$Origin == levels(fdf$Origin)[3],1] <- "MEX"

#complete list of nodes
node_id <- unique(as.character(fdf$Origin))

node_id <- c(node_id, unique(as.character(fdf$Destination)))

node_id <- unique(node_id)

node_num <- 1:length(node_id)-1

str(fdf)



dest_df <- fdf[,6:10]

names(dest_df) <- names(fdf[,c(1:5)])

node_df <- rbind(fdf[,1:5], dest_df)

matches <- match(node_id,node_df$Origin)

nodes <- cbind(node_df[matches,c(1,3,5)],node_num)

origins <- length(unique(as.character(fdf$Origin)))

dests <- length(node_id) - origins

nodes$Role <- c(rep(0,origins), rep(1,dests))

cols<- 1:3
#margin = 2 means column wise
nodes[,cols] <- apply(nodes[,cols],2, function(x) as.character(x)) 

str(nodes)

names(nodes) <- c("ID","CITY","COUNTRY","OID","ROlE")

## links
fdf_links <- fdf
cols <- 1:10
fdf_links[,cols] <- apply(fdf_links[,cols],2, function(x) as.character(x))

str(fdf_links)

matched_source <- match(fdf_links$Origin,nodes$ID)

#get OID numbers for source
fdf_links$source <- nodes[matched_source,4]

#get OID numbers for Target
matched_target <- match(fdf_links$Destination,nodes$ID)

fdf_links$target <- nodes[matched_target,4]

node_size <- tapply(fdf_links$Total.Seats,fdf_links$Destination, mean)

node_size <- as.data.frame(node_size)
node_size$name <- row.names(node_size)


node_size_origin <- tapply(fdf_links$Total.Seats,fdf_links$Origin, mean)

nsz_origin <- as.data.frame(node_size_origin)
nsz_origin$name <- row.names(nsz_origin)

names(node_size) <- names(nsz_origin)

node_szf <-  rbind(nsz_origin,node_size)

node_szf <- node_szf[unique(node_szf$name),]

str(node_szf)

str(nodes)

nodes[order(nodes$ID),6] <- sqrt(node_szf[order(node_szf$name),1])

names(nodes)[6] <- "node_size"


str(fdf_links)

#get higer volume flights

# flt_vol <- 4000
# 
# links <- fdf_links[fdf_links$Total.Seats > flt_vol,]
# 
# nodes_d <- nodes[(nodes$V5)^2 > flt_vol & nodes$Role > 0,]
# nodes_o <- nodes[nodes$Role < 1,]
# nodes_f <- rbind(nodes_o,nodes_d)

sankeyNetwork(Links = fdf_links, Nodes = nodes, 
              Source = "source", LinkGroup = "Destination.Country",
              Target = "target", Value = "Total.Seats", NodeID = "CITY",
              fontSize = 12, nodeWidth = 30)

forceNetwork(Links = fdf_links,zoom = T,Nodesize = "node_size", legend = T,
             Nodes = nodes ,Source = "source", 
             Target = "target",Value = "Direct.Flights",
             NodeID = "CITY",Group = "COUNTRY")








