#' Compile List of Network Edges from a Contact Table
#'
#' This function takes the output from contactDur.all or contactDur.area and 
#'    generates a data frame showing the list of edges in the contact network.
#' 
#' @param x Output from the contactDur.all or contactDur.area functions. Can be
#'    either a data frame or list.
#' @param importBlocks Logical. If true blocks will be carried over from x 
#'    input, allowing for time-ordered and time-aggregated network creation. 
#'    Defaults to FALSE.
#' @param removeDuplicates Logical. If x is from contactDur.all, to-from node 
#'    pairs in output will be reported twice (i.e., nodes will be listed as 
#'    both a to- and a from-node). If removeDuplicates == true, duplicated 
#'    edges are removed. Defaults to TRUE.
#' @keywords data-processing contact
#' @return Output is a data frame with the following columns, and can easily 
#'    be used as input for igraph functions. 
#' 
#'    \item{from}{The "from" nodes in a contact network. Can also be considered
#'    "tail" nodes.}
#'    \item{to}{The "to" nodes in a contact network. Can also be considered
#'    "head" nodes.}
#'    \item{durations}{The duration of each contact reported in \code{x}.}
#'    \item{block}{(if applicable) time block during which reported contacts 
#'    occurred.}
#' @examples
#' data("calves")
#' 
#' calves.dateTime<-datetime.append(calves, date = calves$date,
#'    time = calves$time) #create a dataframe with dateTime identifiers for location fixes.
#' 
#' calves.agg<-tempAggregate(calves.dateTime, id = calves.dateTime$calftag,
#'    dateTime = calves.dateTime$dateTime, point.x = calves.dateTime$x,
#'    point.y = calves.dateTime$y, secondAgg = 300, extrapolate.left = FALSE,
#'    extrapolate.right = FALSE, resolutionLevel = "reduced", parallel = FALSE,
#'    na.rm = TRUE, smooth.type = 1) #smooth to 5-min fix intervals.
#' 
#' calves.dist<-dist2All_df(x = calves.agg, parallel = FALSE,
#'    dataType = "Point", lonlat = FALSE) #calculate inter-animal distances at each timepoint.
#' 
#' calves.contact.NOblock<-contactDur.all(x = calves.dist, dist.threshold=1,
#'    sec.threshold=10, blocking = FALSE, equidistant.time = FALSE,
#'    parallel = FALSE, reportParameters = TRUE) 
#' 
#' calves.edges1<-ntwrkEdges(x =calves.contact.NOblock, importBlocks = FALSE,
#'    removeDuplicates = TRUE)  
#' 
#' head(calves.edges1)
#' 
#' calves.network1 <- igraph::graph_from_data_frame(d=calves.edges1,
#'    directed=FALSE)
#' 
#' igraph::V(calves.network1)$color<- "orange1"
#' igraph::V(calves.network1)$size <-13
#' igraph::E(calves.network1)$width <- calves.edges1$duration
#' igraph::E(calves.network1)$color <- "black"
#' igraph::plot.igraph(calves.network1, vertex.label.cex=0.4,
#'    layout = igraph::layout.circle, main = "Inter-Calf Contacts") #plot the network
#'    
#' @export

ntwrkEdges<-function(x, importBlocks = FALSE, removeDuplicates = TRUE){
  blockAction<-importBlocks
  dupAction<-removeDuplicates
  
  summarizeContacts<- function(x, importBlocks = blockAction, avg = FALSE){
  
  distributeContacts1<- function(x,y, me){
    if(unname(unlist(x[1])) == me){
      spec.durations = 0
    }else{
      contact1 <- y[c(which(as.character(y$dyadMember1) == unname(unlist(x[1])))),]
      contact2 <- y[c(which(as.character(y$dyadMember2) == unname(unlist(x[1])))),]
      if((nrow(contact1) >= 1) & (nrow(contact2) >= 1)){
        contact.full <- data.frame(data.table::rbindlist(list(contact1,contact2)))
      }
      if((nrow(contact1) >= 1) & (nrow(contact2) == 0)){
        contact.full <- contact1
      }
      if((nrow(contact2) >= 1) & (nrow(contact1) == 0)){
        contact.full <- contact2
      }
      if((nrow(contact2) == 0) & (nrow(contact1) == 0)){
        contact.full <- contact1 #if neither contact1 or contact2 have any rows, contact.full won't have any rows either.
      }
      spec.durations <- ifelse(nrow(contact.full) >= 1, sum(contact.full$contactDuration),0)
    }
    return(spec.durations)
  }
  distributeContacts2<- function(x,y){
    contact.full <- y[c(which(y$area.id == unname(unlist(x[1])))),]
    spec.durations <- ifelse(nrow(contact.full) >= 1, sum(contact.full$contactDuration),0)
    return(spec.durations)
  }
  contSum <-function(x,y, indivSeq, areaSeq){
    me = (unname(unlist(x[1])))
    if(length(y$dyadMember1) > 0){ #This essentially determines if the input was created with dist.all or distToArea. If length(dyadMember1) >0, it was created with dist.all
      indivContact1 <- y[c(which(as.character(y$dyadMember1) == me)),] #had to make this as.character b/c "me" is a factor with levels that may be different than y$dyadMember1
      indivContact2 <- y[c(which(as.character(y$dyadMember2) == me)),] #had to make this as.character b/c "me" is a factor with levels that may be different than y$dyadMember2
    }else{
      indivContact1 <- y[c(which(as.character(y$indiv.id) == me)),] 
      indivContact2 <- matrix(nrow=0,ncol=0)
    }
    
    #Here identify the number of contact durations individuals had with others. How we do this is determined by the number of times individuals appear in y's dyadMember1 and dyadMember2 columns. Note that the only option if the function input originated from contactDur.area is (nrow(indivContact1) >= 1) & (nrow(indivContact2) == 0)
    if((nrow(indivContact1) >= 1) & (nrow(indivContact2) >= 1)){
      indivContact.full <- data.frame(data.table::rbindlist(list(indivContact1,indivContact2)))
      specIndivSeq = unique(c(as.character(indivContact.full$dyadMember1),as.character(indivContact.full$dyadMember2))) #had to add as.character call because contactDur functions now produce factor data. 02/05/2019
      specIndivSeq1 = specIndivSeq[-which(specIndivSeq == me)]
    }
    if((nrow(indivContact1) >= 1) & (nrow(indivContact2) == 0)){
      indivContact.full <- indivContact1
      if(length(y$dyadMember1) > 0){ #This essentially determines if the input was created with dist.all or distToArea. If length(dyadMember1) >0, it was created with dist.all
        specIndivSeq = unique(c(as.character(indivContact.full$dyadMember1),as.character(indivContact.full$dyadMember2))) #had to add as.character call because contactDur functions now produce factor data. 02/05/2019
        specIndivSeq1 = specIndivSeq[-which(specIndivSeq == me)]
      }else{
        specIndivSeq1 = unique(as.character(indivContact.full$area.id)) #had to add as.character call because contactDur functions now produce factor data. 02/05/2019
      }
    }
    if((nrow(indivContact2) >= 1) & (nrow(indivContact1) == 0)){
      indivContact.full <- indivContact2
      specIndivSeq = unique(c(as.character(indivContact.full$dyadMember1),as.character(indivContact.full$dyadMember2))) #had to add as.character call because contactDur functions now produce factor data. 02/05/2019
      specIndivSeq1 = specIndivSeq[-which(specIndivSeq == me)]
    }
    if((nrow(indivContact2) == 0) & (nrow(indivContact1) == 0)){
      indivContact.full <- indivContact1 #if neither indivContact1 or indivContact2 have any rows, indivContact.full won't have any rows either.
      specIndivSeq1 = 0
    }
    if(length(y$dyadMember1) > 0){ #This essentially determines if the input was created with dist.all or distToArea. If length(dyadMember1) >0, it was created with dist.all
      if(nrow(indivContact.full) > 1){
        indivSeqFrame1 <-data.frame(indivSeq)
        contactSum<-apply(indivSeqFrame1, 1, distributeContacts1, indivContact.full, me)
        sumTable <- data.frame(matrix(ncol = (3+length(indivSeq)), nrow = 1))
        colnames(sumTable) <- c("id","totalDegree","totalContactDurations", paste("contactDuration_Indiv",indivSeq, sep = ""))
        sumTable$id = me
        sumTable$totalDegree <- length(specIndivSeq1)
        sumTable$totalContactDurations = sum(indivContact.full$contactDuration)
        sumTable[1,4:ncol(sumTable)] <- contactSum
        sumTable[,match(paste("contactDuration_Indiv",me, sep = ""), names(sumTable))] = NA
      }else{ #if nrow !>0
        if(nrow(indivContact.full) == 1){
          sumTable <- data.frame(matrix(ncol = (3+length(indivSeq)), nrow = 1))
          colnames(sumTable) <- c("id","totalDegree","totalContactDurations", paste("contactDuration_Indiv",indivSeq, sep = ""))
          sumTable$id = me
          sumTable$totalDegree <- 1
          sumTable$totalContactDurations = indivContact.full$contactDuration
          sumTable[1,4:ncol(sumTable)] <- 0
          sumTable[,match(paste("contactDuration_Indiv",specIndivSeq1, sep = ""), names(sumTable))] = indivContact.full$contactDuration
          sumTable[,match(paste("contactDuration_Indiv",me, sep = ""), names(sumTable))] = NA
        }
        if(nrow(indivContact.full) == 0){
          sumTable <- data.frame(matrix(ncol = (3+length(indivSeq)), nrow = 1))
          colnames(sumTable) <- c("id","totalDegree","totalContactDurations", paste("contactDuration_Indiv",indivSeq, sep = ""))
          sumTable$id = me
          sumTable[1,2:ncol(sumTable)] <- 0
          sumTable[,match(paste("contactDuration_Indiv",me, sep = ""), names(sumTable))] = NA
        }
      }
    }else{ # length(y$dyadMember1) == 0
      if(nrow(indivContact.full) > 1){
        areaSeqFrame <- data.frame(areaSeq)
        contactSum<-apply(areaSeqFrame, 1, distributeContacts2, indivContact.full)
        sumTable <- data.frame(matrix(ncol = (3+length(areaSeq)), nrow = 1))
        colnames(sumTable) <- c("id","totalDegree","totalContactDurations", paste("contactDuration_Area_",areaSeq, sep = ""))
        sumTable$id = me
        sumTable$totalDegree <- length(specIndivSeq1)
        sumTable$totalContactDurations = sum(indivContact.full$contactDuration)
        sumTable[1,4:ncol(sumTable)] <- contactSum
      }else{ #if nrow !>1
        if(nrow(indivContact.full) == 1){
          areaVec <- unique(y$area.id)
          sumTable <- data.frame(matrix(ncol = (3+length(areaSeq)), nrow = 1))
          colnames(sumTable) <- c("id","totalDegree","totalContactDurations", paste("contactDuration_Area_",areaSeq, sep = ""))
          sumTable$id = me
          sumTable$totalDegree <- 1
          sumTable$totalContactDurations = indivContact.full$contactDuration
          sumTable[1,4:ncol(sumTable)] <- 0
          sumTable[,match(paste("contactDuration_Area_",areaVec, sep = ""), names(sumTable))] = indivContact.full$contactDuration
        }
        if(nrow(indivContact.full) == 0){
          sumTable <- data.frame(matrix(ncol = (3+length(areaSeq)), nrow = 1))
          colnames(sumTable) <- c("id","totalDegree","totalContactDurations", paste("contactDuration_Area_",areaSeq, sep = ""))
          sumTable$id = me
          sumTable[1,2:ncol(sumTable)] <- 0
        }
      }			
    }
    return(sumTable)
  }
  blockSum <-function(x,y, indivSeq, areaSeq){
    blockDurFrame<-y[which(y$block == unname(unlist(x[1]))),]
    indivSeqFrame <- data.frame(indivSeq)
    summary.contacts<-apply(indivSeqFrame, 1, contSum, blockDurFrame, indivSeq, areaSeq)
    indivSum.full<- data.frame(data.table::rbindlist(summary.contacts))
    indivSum.full$block <- unname(unlist(x[1]))
    
    #added 02/05/2019 - to maintain this new information created in the newest version of the contactDur functions.
    indivSum.full$block.start <- unique(lubridate::as_datetime(blockDurFrame$block.start)) # updated 06/02/2019 - converted the factor data to POSIXct format in order to avoid a "length is too large for hashing" error.
    indivSum.full$block.end <- unique(lubridate::as_datetime(blockDurFrame$block.end)) # updated 06/02/2019 - converted the factor data to POSIXct format in order to avoid a "length is too large for hashing" error.
    indivSum.full$numBlocks <- unique(blockDurFrame$numBlocks)
    return(indivSum.full)
  }
  summaryAgg.block<-function(x,y){ #calculates the mean contacts from multiple summarizeContacts outputs
    sumTable<-y[which(y$id == unname(unlist(x[1])) & y$block == unname(unlist(x[2]))),]
    blockStart<- unique(lubridate::as_datetime(sumTable$block.start)) #added 02/05/2019 - had to keep track of this new information ; updated 06/02/2019 - converted the factor data to POSIXct format in order to avoid a "length is too large for hashing" error.
    blockEnd<- unique(lubridate::as_datetime(sumTable$block.end)) #added 02/05/2019 - had to keep track of this new information ;  updated 06/02/2019 - converted the factor data to POSIXct format in order to avoid a "length is too large for hashing" error.
    blockNum<- unique(sumTable$numBlocks) #added 02/05/2019 - had to keep track of this new information
    sumTable.redac<-sumTable[,-c(match("id", names(sumTable)),match("block", names(sumTable)), match("block.start", names(sumTable)), match("block.end", names(sumTable)), match("numBlocks", names(sumTable)))]  #Remove the columns that cannot/shoud not be averaged.
    contact.mean <- apply(sumTable.redac,2,mean, na.rm = TRUE)
    output = sumTable[1,]
    output[1,match("id", names(sumTable))] = unname(unlist(x[1])) #add this information back into the table
    output[1,match("block", names(sumTable))] = unname(unlist(x[2])) #add this information back into the table
    output[1,match("block.start", names(sumTable))] = blockStart #add this information back into the table
    output[1,match("block.end", names(sumTable))] = blockEnd #add this information back into the table
    output[1,match("numBlocks", names(sumTable))] = blockNum #add this information back into the table
    output[1,match(names(sumTable.redac), names(output))] = contact.mean
    return(output)
  }
  summaryAgg.NoBlock<-function(x,y){
    sumTable<-y[which(y$id == unname(unlist(x[1]))),]
    sumTable.redac<-sumTable[,-match("id", names(sumTable))] #Remove the columns that cannot/shoud not be averaged.
    contact.mean <- apply(sumTable.redac,2,mean, na.rm = TRUE)
    output = sumTable[1,]
    output[1,match("id", names(sumTable))] = unname(unlist(x[1])) #add this information back into the table
    output[1,match(names(sumTable.redac), names(output))] = contact.mean
    return(output)
  }
  summary.generator<-function(x, importBlocks){
    
    if(importBlocks == TRUE){
      
      if(length(x$dyadMember1) > 0){ #This essentially determines if the input was created with dist.all or distToArea. If length(dyadMember1) >0, it was created with dist.all
        x<-x[order(x$block,x$dyadMember1,x$dyadMember2),]
        indivVec <- c(as.character(x[,match("dyadMember1", names(x))]), as.character(x[,match("dyadMember2", names(x))]))
        areaSeq = NULL
      }else{
        x<-x[order(x$block,x$indiv.id),]
        indivVec <- x[,match("indiv.id", names(x))]
        areaVec <- x[,match("area.id", names(x))]
        areaVec <- areaVec[order(areaVec)] #forces the data type to become character so that there will be no issues with apply functions later.
        areaSeq<-as.character(unique(areaVec))
      }
      indivSeq <- unique(indivVec)
      indivSeq<-indivSeq[order(indivSeq)]
      indivSeq<-as.character(indivSeq) #forces the data type to become character so that there will be no issues with apply functions later.
      blockVecFrame <- data.frame(unique(as.character(x$block)))
      summary.block <- apply(blockVecFrame, 1, blockSum, x, indivSeq, areaSeq) #according to Dan, this apply function is faster than parApply, so I've removed the parApply option 1/17
      summaryTable<- data.frame(data.table::rbindlist(summary.block))
      summaryTable<-summaryTable[order(summaryTable$block,summaryTable$id),]
      
    }else{ #importBlocks == FALSE
      if(length(x$dyadMember1) > 0){ #This essentially determines if the input was created with dist.all or distToArea. If length(dyadMember1) >0, it was created with dist.all
        x<-x[order(x$dyadMember1,x$dyadMember2),]
        indivVec <- c(as.character(x[,match("dyadMember1", names(x))]), as.character(x[,match("dyadMember2", names(x))])) #as.character forces the data type to become character so that there will be no issues with apply functions later.
        areaSeq = NULL
      }else{
        x<-x[order(x$indiv.id),]
        indivVec <- x[,match("indiv.id", names(x))]
        areaVec <- x[,match("area.id", names(x))]
        areaVec <- areaVec[order(areaVec)] #forces the data type to become character so that there will be no issues with apply functions later.
        areaSeq<-as.character(unique(areaVec))
      }
      indivSeq <- unique(indivVec)
      indivSeq<-indivSeq[order(indivSeq)]
      indivSeq<-as.character(indivSeq) #forces the data type to become character so that there will be no issues with apply functions later.
      indivSeqFrame <- data.frame(indivSeq)
      summary.contacts <- apply(indivSeqFrame, 1, contSum, x, indivSeq, areaSeq) #according to Dan, this apply function is faster than parApply, so I've removed the parApply option 1/17
      summaryTable<- data.frame(data.table::rbindlist(summary.contacts))
      summaryTable<-summaryTable[order(summaryTable$id),]
    }
    return(summaryTable)
  }
  
  if(is.data.frame(x) == FALSE & is.list(x) == TRUE){
    summaryList<-lapply(x, summary.generator, importBlocks) #changed to lapply 02/02/2019
    
    if(avg == TRUE){
      full.summary<- data.frame(data.table::rbindlist(summaryList, fill = TRUE)) #Now we need to average the number of contacts by id and block
      idSeq<-unique(full.summary$id)
      if(importBlocks == TRUE){
        blockSeq<-unique(full.summary$block)
        aggTab<- expand.grid(as.character(idSeq),as.character(blockSeq))
        sumTab <- apply(aggTab, 1, summaryAgg.block, y = full.summary)
      }else{ #if importBlocks == FALSE
        aggTab<-data.frame(idSeq)
        sumTab <- apply(aggTab, 1, summaryAgg.NoBlock, y = full.summary)
      }
      sumTab.agg <- data.frame(data.table::rbindlist(sumTab))
      summary.output<-list(sumTab.agg, summaryList)
      names(summary.output)<-c("avg.","contactSummaries.")
    }else{ #if avg == FALSE
      summary.output<- summaryList
    }
  }else{ #if x is NOT a list
    summary.output <- summary.generator(x, importBlocks)
  }
  return(summary.output)
}
  edgeGenerator.noBlock<-function(x, removeDuplicates = dupAction){
    confirm_edges.noBlock<-function(x,y){ #x = potential edges, y = contactSummary; essentially presents contact summary results in long form
      if(length(levels(unname(unlist(x[1])))) > 1){ #This has to be here to avoid an error when trying to coerce output into the out.frame
        x1.id <- droplevels(unname(unlist(x[1])))
      }else{ #if the number of levels <= 1
        x1.id <-unname(unlist(x[1]))
      }
      if(length(levels(unname(unlist(x[2])))) > 1){ #This has to be here to avoid an error when trying to coerce output into the out.frame
        x2.id <- droplevels(unname(unlist(x[2])))
      }else{ #if the number of levels <= 1
        x2.id <-unname(unlist(x[2]))
      }
      out.frame<-data.frame(from = x1.id, to = x2.id) #must be made into a data frame to avoid the "listCoercing LHS to a list" warning.
      y.ContactNames<-c(NA,NA,NA,substring((names(y[grep("contactDuration_", names(y))])),22)) #The three NAs represent the first 3 columns in y, which are irrelevent for our needs.
      duration <- unname(unlist(y[which(y$id == x1.id), which(y.ContactNames == x2.id)]))
      duration.corrected<-ifelse(duration > 0, duration, NA) 
      out.frame$durations<-duration.corrected #this is the total number of durations individuals were observed in contact with others
      return(out.frame)
    }
    contactSummary<-summarizeContacts(x) #generate a summary of the contact table using the summarizeContacts function (available in the package as a stand-alone function)
    contactSummary.node1 <- unique(contactSummary$id)
    contactSummary.node2 <- substring((names(contactSummary[grep("contactDuration_", names(contactSummary))])),22) #pulls out the contacted IDs
    potential_edges <- expand.grid(contactSummary.node1, contactSummary.node2) #create a data frame detailing all the potential edges that may have occurred in the dataset (including loops, but loops will ultimately be removed later).
    names(potential_edges) <- c("from", "to")
    
    if(removeDuplicates == TRUE){
      rm_Vec<-NULL
      for(i in unique(potential_edges$to)){
        last_edge<-max(which(potential_edges$to == i)) #identifies the last observation of an edge attached to node i
        rm<-(which(potential_edges$from[(last_edge + 1):nrow(potential_edges)] == i) + last_edge) #identifies which rows after max describe edges attached to node i
        rm_Vec<-c(rm_Vec, rm) #creates a vector of rows describing duplicated edges.
      }
      if(length(rm_Vec > 0)){ #this if statement ensures that no empty edgelist results from trying to remove an empty rm_Vec
        potential_edges<- potential_edges[-rm_Vec,] #now we have our potential network edge set with no duplicate edges.
      }
    }
    
    edgelist<-apply(potential_edges,1,confirm_edges.noBlock, y=contactSummary) #confirm whether edges existed or not. If "durations" is NA, then no edge existed. 
    edgeFrame<-data.frame(data.table::rbindlist(edgelist))
    confirmed_edges <- edgeFrame[is.na(edgeFrame$duration) == FALSE,] #So now we have a data frame detailing the undirected edges we observed and the number of contacts associated with them. 
    rownames(confirmed_edges)<-seq(1,nrow(confirmed_edges))
    return(confirmed_edges)
  }
  edgeGenerator.Block<-function(x, removeDuplicates = dupAction){
    confirm_edges.Block<-function(x,y){ #x = potential edges, y = contactSummary; essentially presents contact summary results in long form
      if(length(levels(unname(unlist(x[2])))) > 1){ #This has to be here to avoid an error when trying to coerce output into the out.frame
        x2.id <- droplevels(unname(unlist(x[2])))
      }else{ #if the number of levels <= 1
        x2.id <-unname(unlist(x[2]))
      }
      if(length(levels(unname(unlist(x[3])))) > 1){ #This has to be here to avoid an error when trying to coerce output into the out.frame
        x3.id <- droplevels(unname(unlist(x[3])))
      }else{ #if the number of levels <= 1
        x3.id <-unname(unlist(x[3]))
      }
      out.frame<-data.frame(from = x2.id, to = x3.id) #must be made into a data frame to avoid the "listCoercing LHS to a list" warning.
      y.ContactNames<-c(NA,NA,NA,substring((names(y[grep("contactDuration_", names(y))])),22)) #The three NAs represent the first 3 columns in y, which are irrelevent for our needs.
      duration <- unname(unlist(y[which(y$id == x2.id), which(y.ContactNames == x3.id)]))
      duration.corrected<-ifelse(duration > 0, duration, NA) 
      out.frame$durations<-duration.corrected #this is the total number of durations individuals were observed in contact with others
      out.frame$block <- unname(unlist(x[1])) #add block information
      out.frame$block.start <- unname(unlist(x[4]))
      out.frame$block.end <- unname(unlist(x[5]))
      return(out.frame)
    }
    contactSummary<-summarizeContacts(x) #generate a summary of the contact table using the summarizeContacts function (available in the package as a stand-alone function)
    contactSummary.node1 <- unique(contactSummary$id)
    contactSummary.node2 <- substring((names(contactSummary[grep("contactDuration_", names(contactSummary))])),22) #pulls out the contacted IDs
    block_info<-data.frame(block = unique(contactSummary$block), block.start = unique(contactSummary$block.start), block.end = unique(contactSummary$block.end)) #pulls the block information from contactSummary
    potential_edges1 <- expand.grid(contactSummary.node1, contactSummary.node2, block_info$block) #create a data frame detailing all the potential edges that may have occurred in the dataset (including loops, but loops will ultimately be removed later).
    names(potential_edges1) <- c("from", "to", "block")
    potential_edges2<-merge(potential_edges1, block_info, by = "block") #note that this merge reorders the columns, by placing the "block" column first.
    potential_edges2<-potential_edges2[order(as.numeric(as.character(potential_edges2$block))),] #orders rows by ascending block number.
    
    if(removeDuplicates == TRUE){ #I know this double for-loop is relatively inefficient. In future versions I will update this to be an apply function.
      rm_Vec<-NULL
      for(i in unique(potential_edges2$to)){
        for(j in unique(potential_edges2$block)){
          start_block<-min(which(potential_edges2$block == j))
          end_block<-max(which(potential_edges2$block == j))
          last_edge<-max(which(potential_edges2$to[1:end_block] == i)) #identifies the last observation of an edge attached to node i
          rm<-(which(potential_edges2$from[(last_edge + 1):end_block] == i) + last_edge) #identifies which rows should be removed to ensure duplicates are taken out of each block
          rm_Vec<-c(rm_Vec, rm) #creates a vector of rows describing duplicated edges.
        }
      }
      if(length(rm_Vec > 0)){ #this if statement ensures that no empty edgelist results from trying to remove an empty rm_Vec
        potential_edges2<- potential_edges2[-rm_Vec,] #now we have our potential network edge set with no duplicate edges.
      }
    }
    
    edgelist<-apply(potential_edges2,1,confirm_edges.Block, y=contactSummary) #confirm whether edges existed or not. If "durations" is NA, then no edge existed. 
    edgeFrame<-data.frame(data.table::rbindlist(edgelist))
    confirmed_edges <- edgeFrame[is.na(edgeFrame$duration) == FALSE,] #So now we have a data frame detailing the undirected edges we observed and the number of contacts associated with them. 
    rownames(confirmed_edges)<-seq(1,nrow(confirmed_edges))
    
    return(confirmed_edges)
  }
  
  if(importBlocks == FALSE){
    edgeSet<-edgeGenerator.noBlock(x)
  }else{ #i.e., if importBlocks == TRUE
    edgeSet<-edgeGenerator.Block(x)
  }
  return(edgeSet)
}
