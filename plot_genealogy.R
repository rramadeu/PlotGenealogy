######################################################################################
# 									
# Package: AGHmatrix 							
# 									
# File: plot_genealogy.R 							
# Contains: plot_genealogy
# 									
# Written by Rodrigo Rampazo Amadeu 					
# 									
# First version: 11-Aug-2014 						
# Last update: 19-June-2018						
# License: GPL-3	
# 									
######################################################################################

#' Dependences:
#' library(tidyverse)
#' library(stringr)
#' library(RColorBrewer)
#' library(ggtree)
#' library(stringi)

#' Construction of Genealogy for an individual
#'
#' Builds a genealogy for a given individual based on his pedigree
#'
#' @param data pedigree data name (data frame in a 3-column way format).
#' @param ind indiviudal name (string)
#' @param gen the max number of generations ago of the plot
#' @param color if TRUE, individuals repeated with different colors
#'
#' @examples
#' #Build Amatrix autotetraploidy (double reduction proportion=0.1)
#' data(ped.mrode)
#' plot_genealogy(data=ped.mrode,ind="Var4")
#'
#' @author Rodrigo R Amadeu, \email{rramadeu@@gmail.com}
#'
#' @export

plot_genealogy <- function(ped=NULL,
                           ind="",
                           flip=FALSE,
                           color=TRUE,
                           save=FALSE,
                           device="png",
                           generations=100,
                           height=40,
                           slanted=TRUE,
                           width=15,
                           gens=FALSE){
  if(is.null(ped))
    return()
  if(ind=="")
    return()
  
  cultivar_name <- ind
  ped <- ped %>% mutate_if(is.factor,as.character)
  ped_out<- ped %>% filter(ped[,1] == ind)
  if(nrow(ped_out)==0){
    stop(deparse(paste(ind,"doesn't exist in this pedigree.")))
  }
  
  if(ped_out[2] == "0")
    if(ped_out[3] == "0")
      stop(deparse(paste(ind,"with no pedigree records.")))
  
  
  trigger <- 1
  while(trigger>0){
    init <- nrow(ped_out)
    ped_in <- ped %>% filter( ped[,1] %in% c(ped_out[,2],ped_out[,3]))
    ped_out <- unique(rbind(ped_in, ped_out))
    trigger <- nrow(ped_out)-init
  }
  ped <- ped_out
  ind <- paste0(" ",ind," ")
  
  ped[,1] <- paste0(" ",ped[,1]," ")
  ped[,2] <- paste0(" ",ped[,2]," ")
  ped[,3] <- paste0(" ",ped[,3]," ")
  ped[,1] <- gsub("\\,|\\(|\\)","\\.",ped[,1])
  ped[,2] <- gsub("\\,|\\(|\\)","\\.",ped[,2])
  ped[,3] <- gsub("\\,|\\(|\\)","\\.",ped[,3])
  
  
  nwk.df <- data.frame(nwk=paste0("(",ped[,2],",",ped[,3],")",ped[,1]))
  nwk.df <- nwk.df %>% mutate_if(is.factor,as.character)
  row.names(nwk.df) <- ped[,1]
  
  
  ##    init <- which(ped[,1]==ind)
  ##    init.nwk <- nwk.df[init,1]
  rownames(ped) <- ped[,1]
  old.trigger.sup <- 0
  firstrun <- trigger <- trigger.sup <- loop<- TRUE
  while(trigger.sup > old.trigger.sup){
    trigger<-TRUE
    old.trigger.sup <- trigger.sup
    while(trigger){
      k <- ifelse(firstrun,0,1)
      for(i in k:nrow(ped)){
        if(i==0){
          init <- which(ped[,1]==ind)
          init.nwk <- nwk.df[init,1]
        }else{
          init <- ped[i,1]
        }
        
        if(ped[init,2]!=" 0 "){
          ind <- which(ped[,1]==ped[init,2])
          init.nwk.new <- gsub(ped[init,2], str_trim(nwk.df[ind,1]), init.nwk)
          init.nwk.new <- str_trim(init.nwk.new)
          if(init.nwk.new!=init.nwk){
            trigger1 <- TRUE #while loop trigger
            init.nwk <- init.nwk.new
          }
        }else{
          trigger1 <- FALSE #while loop trigger
        }
        
        if(ped[init,3]!=" 0 "){
          ind <- which(ped[,1]==ped[init,3])
          init.nwk.new <- gsub(ped[init,3], str_trim(nwk.df[ind,1]), init.nwk)
          init.nwk.new <- str_trim(init.nwk.new)
          if(init.nwk.new!=init.nwk){
            trigger2 <- TRUE #while loop trigger
            init.nwk <- init.nwk.new
          }
        }else{
          trigger2 <- FALSE #while loop trigger
        }
        trigger <- trigger1+trigger2
        trigger.sup <- trigger.sup + trigger
        trigger1 <- trigger2 <- firstrun <- FALSE
      }
    }
    #      print(paste(old.trigger.sup,trigger.sup))
  }
  
  init.nwk <- gsub(pattern="\\( 0 , 0 \\)","",x=init.nwk)
  init.nwk <- paste0(init.nwk,";")
  ## Replacing commas
  
  write(init.nwk,"tree.nwk")
  tree <- read.tree(text=init.nwk)
  tree$edge.length <- rep(1,nrow(tree$edge))
  
  ## ## Finding the duplicateds
  ## datadup <- as.factor(p$data$label)
  ## dups <- c(0,0,0)
  ## colors <- brewer.pal(12,"Paired")
  ## j <- 1
  ## for(i in 2:length(datadup)){
  ##     whichdup <- which(datadup==levels(dup)[i])
  ##     if(length(whichdup)>1){
  ##         comb <- combn(whichdup,2)
  ##         comb <- rbind(colors[j],comb)
  ##         dups <- cbind(dups,comb)
  ##         j <- (j + 1)%%12
  ##         if(j==0)
  ##             j <- j+1
  ##     }
  ## }
  ## dups <- dups[,-1]
  ## dups <- data.frame(color = dups[1,],
  ##                    taxa1 = as.numeric(dups[2,]),
  ##                    taxa2 = as.numeric(dups[3,]))
  ## p1 <- p
  
  ## Finding not unique labels
  

  p <- ggtree(tree,right = flip,layout = ifelse(slanted,"slanted","rectangular"))

  p$data$color <- !(!p$data$label %in% p$data$label[duplicated(p$data$label)])
  p$data$color[which(p$data$color)] <- p$data$label[which(p$data$color)]
  p$data$isTip <- TRUE
  colorspallete <- c("black",rep(brewer.pal(8,"Dark2"),ceiling(nrow(ped)/8)))
  names(colorspallete) <- unique(c("FALSE",p$data$color))
  colorspallete <- colorspallete[-which(is.na(names(colorspallete)))]
  p$data$color[which(p$data$color=="0")] <- FALSE
  if(color){
    p <- p + geom_tiplab(aes(color=color,geom="label")) + scale_colour_manual(values = colorspallete)
  }else{
    p <- p + geom_tiplab(geom="label")
  }
  
  if(gens){
    if(gens<max(p$data$x))
      p$data <- p$data[-which(p$data$x>gens),]
  }
  
  ## The next chunk makes a trick to add a blank space in the left part of the plot
  ## In this way it doesnt shirnk the plot names
  if(!slanted){
    p$data$group <- 0
    addline <- data.frame(parent=max(p$data$parent)+1,
                          node=max(p$data$node)+1,
                          branch.length=p$data$branch.length[1],
                          label=0,
                          isTip=FALSE,
                          x=max(p$data$x)+1,
                          y=0,
                          branch=1,
                          angle=0,
                          color="white",
                          group=1)
    p$data <- rbind(p$data,addline)
  p$data$group <- as.factor(p$data$group)
  p$labels$linetype <- "group"
  p$mapping$linetype <- rlang::new_quosure(quote(group), rlang::caller_env())
  }
  p<-p+scale_linetype_manual(values=c("solid", "blank")) + theme(legend.position="none") + expand_limits(x = 3)
  
  
  
  if(save)
    ggsave(p,file=paste0(cultivar_name,".",device),device=device,height=height,width=width)
  
  return(p)
}

