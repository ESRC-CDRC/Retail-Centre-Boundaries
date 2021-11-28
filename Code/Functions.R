########################################################################################
########  An open source delineation and hierarchical classification of UK
#######     retail centres from point clusters
######
#####     Jacob L. Macdonald, Les Dolega, Alex Singleton
####      Last Updated: October, 2021
###
##  FUNCTIONS DEVELOPED FOR SPATIAL OPERATIONS
##   
##  - This script contains the functions which are loaded prior to the data cleaning
##    and retail delineation. 

## GENERATING CONTIGUOUS AND MUTUALLY EXCLUSIVE TRACT IDS
##  - The get.tractID function uses the connectivity of h3 geometries and the k_ring
##    function to identify nearby neighbours which are connected in a contiguous and
##    mutually exclusive set apart from all other (connected) h3 geometries.
##
## INPUTS:
##    - H3_ADDRESS    : Vector of individual h3 addresses
##    - RETAIL_N      : Corresponding vector of retail units within each h3
##    - retail.count  : T/F whether a retail centre level count of retail units should
##                      be provided along with the boundary output
##    - source        : Vector of sources for the h3 additions - whether being added
##                      "Manually", through the "VOA", "OSM", or others. If the vector
##                      is provided, a count distribution of how many types of each source
##                      in each retail center is provided.
##    - NAME          : Identifier ID to append to the random ID

get.tractID <- function(H3_ADDRESS, RETAIL_N=NULL, retail.count=FALSE, source=NULL, NAME="tractID"){
  hex_Graph <- as_tibble(do.call(rbind, lapply(as.list(H3_ADDRESS), function(x) k_ring(x, 1))))
  names(hex_Graph) <- c("h3_address", paste0("neigh.", 1:(length(colnames(hex_Graph))-1)))
  
  hex_Graph <- hex_Graph %>%
    gather("neighbourN", "h3_neighbour", -h3_address) %>%
    select(h3_address, h3_neighbour) %>%
    arrange(h3_address, h3_neighbour) %>%
    filter(h3_neighbour %in% unique(H3_ADDRESS) & h3_address %in% unique(H3_ADDRESS)) %>%
    filter(!is.na(h3_address) | h3_address!="") %>%
    filter(!is.na(h3_neighbour) | h3_neighbour!="") %>% 
    unique()
  
  hex_Graph <- as_tbl_graph(hex_Graph, directed=FALSE)
  hex_Graph <- split(names(V(hex_Graph)), components(hex_Graph)$membership)
  
  hex_Graph.N <- lapply(as.list(1:length(hex_Graph)), function(x) paste0("tractID_", names(hex_Graph[x])))
  
  hex_Graph <- as_tibble(do.call(rbind, lapply(as.list(1:length(hex_Graph)), function(x) cbind(hex_Graph.N[[x]], hex_Graph[[x]]))))
  names(hex_Graph) <- c("tractID", "h3_address")
  
  hex_Graph <- hex_Graph %>%
    filter(h3_address %in% unique(H3_ADDRESS)) %>%
    select(tractID, h3_address) %>%
    filter(!is.na(tractID) | tractID!="") %>%
    filter(!is.na(h3_address) | h3_address!="") %>% 
    group_by(tractID) %>%
    add_count(name = "tractN") %>%
    unique()
  
  if(length(unique(hex_Graph$h3_address))!=length(H3_ADDRESS)){
    t1 <- as.data.frame(H3_ADDRESS[!(H3_ADDRESS %in% hex_Graph$h3_address)])
    names(t1) <- "h3_address"
    t1$tractID <- paste0("tractID_", (max(as.numeric(gsub("tractID_", "", hex_Graph$tractID)), na.rm = T)+1):(max(as.numeric(gsub("tractID_", "", hex_Graph$tractID)), na.rm = T)+dim(t1)[1]))
    t1$tractN <- 1
    
    hex_Graph <- rbind(as.data.frame(hex_Graph), t1)
    hex_Graph <- as_tibble(hex_Graph)
  }
  
  if(retail.count){
    t2 <- as.data.frame(cbind(h3_address=H3_ADDRESS, RETAIL_N))
    t2$RETAIL_N <- as.numeric(as.character(t2$RETAIL_N))
    hex_Graph <- left_join(hex_Graph, as_tibble(t2)) %>%
      select(tractID, h3_address, tractN, RETAIL_N) %>%
      dplyr::group_by(tractID) %>%
      dplyr::mutate(tr_retailN = sum(RETAIL_N, na.rm=T)) %>%
      select(tractID, h3_address, tractN, tr_retailN) %>% 
      filter(!is.na(h3_address) | h3_address!="") %>% 
      unique() }
  hex_Graph$tractID <- gsub("tractID", NAME, hex_Graph$tractID)
  
  if(!is.null(source)){
    t3 <- hex_Graph %>%
      left_join(as_tibble(as.data.frame(cbind(h3_address=H3_ADDRESS, source=source)))) %>%
      dplyr::rename(source=(dim(hex_Graph)[2]+1)) %>%
      select(tractID, source) %>%
      dplyr::group_by(tractID, source) %>%
      tally() %>%
      distinct() %>%
      dplyr::mutate(source = sprintf("%s_N", source)) %>%
      spread(key = source, value = n, fill=0)
    hex_Graph <- hex_Graph %>%
      left_join(t3) %>%
      unique()
  }
  return(hex_Graph)
}


## SPLITTING UP LARGE BOUNDING BOXES TO MAKE OSM EXTRACTION FEASIBLE
##  - The bb.list function breaks up large bounding boxes which are not
##    feasible to use when wanting to extract OSM or related data. 
##    For example, attempting to extract all of a given OSM point group
##    for the entirety of England is often not possible. The bb.list
##    function takes in the large bounding box and tiles it into smaller
##    more manageable components. 
##
## INPUTS:
##    - BB    : Large bounding box to tile into smaller components. 
##    - sq.N  : Number of rows/columns to split into. Eg. sq.N=2 would 
##              tile the bounding box into 2*2=4 smaller tiles.

bb.list <- function(BB, sq.N){
  x.n <- (BB[1,2] - BB[1,1])/sq.N
  y.n <- (BB[2,2] - BB[2,1])/sq.N
  x <- BB[1,1] + x.n*c(0:sq.N)
  y <- BB[2,1] + y.n*c(0:sq.N)
  
  bb.x <- list()
  bb.y <- list()
  for(i in 1:sq.N){
    x.1 <- cbind(x[i], x[i+1])
    y.1 <- cbind(y[i], y[i+1])
    
    bb.x[[i]] <- x.1    
    bb.y[[i]] <- y.1
  }
  bb.N <- unlist(lapply(bb.x, function(a) lapply(bb.y, function (b) rbind(a, b))), recursive=FALSE)
  bb.N <- lapply(bb.N, function(x) {dimnames(x) <- list(c("x", "y"), c("min", "max")); return(x)})
  return(bb.N)
}