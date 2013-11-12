#data<-exprs.quantile 
#data<-RMA_F_CgeneListB_T2.csv
data<-write.table(file="RMA_F_CgeneListB_T2.csv", row.names=TRUE, sep=",", col.names=TRUE)


#idx.ko<-grep("neg",colnames(data))
idx.ko<-grep("C",colnames(data))

#indices <- lapply(c("0h","2h","6h","24h","48h"), function(p) {grep(p,

indices <- lapply(c("T1","T2","T3","T4","T8","T9","T10","T11","T12","T13","T14","T15"), function(p) {grep(p,colnames(data))}) 
# Group specific column indices

ko.data.means <- matrix(unlist(lapply(indices, function(idxs) {

  rowMeans(data[,intersect(idxs,idx.ko)])

})), nrow=nrow(data))

ko.data.sd <- matrix(unlist(lapply(indices, function(idxs) {
  apply(data[,intersect(idxs,idx.ko)],1,sd)

})), nrow=nrow(data))

# Get all WT averages and SDs into one matrix
#idx.wt<-grep("pos",colnames(data))
idx.wt<-grep("F",colnames(data))

#indices <- lapply(c("0h","2h","6h","24h","48h"), function(p) {grep(p, #colnames(data))}) # Group specific column indices
indices <- lapply(c("T1","T2","T3","T4","T8","T9","T10","T11","T12","T13","T14","T15"), function(p) {grep(p, colnames(data))}) 

wt.data.means <- matrix(unlist(lapply(indices, function(idxs) {
  rowMeans(data[,intersect(idxs,idx.wt)])
})), nrow=nrow(data))
wt.data.sd <- matrix(unlist(lapply(indices, function(idxs) {
  apply(data[,intersect(idxs,idx.wt)],1,sd)
})), nrow=nrow(data))
data.means<-cbind(ko.data.means,wt.data.means)

#colnames(data.means)<-c(c("neg-0h","neg-2h","neg-6h","neg-24h","neg-#48h"),c("pos-0h","pos-2h","pos-6h","pos-24h","pos-48h"))
colnames(data.means)<-c(c("C6068_01","C6068_02","C6068_03","C6068_4","C6068_08", "C6068_09", "C6068_10", "C6068_11", "C6068_12", "C6068_13", "C6068_14", "C6068_15", "C6089_02", "C6089_03", "C6089_04", "C6089_08", "C6089_09", "C6089_10", "C6089_11", "C6089_12", "C6089_13", "C6089_14", "C6089_15", "C6231_02", "C6231_03", "C6231_04", "C6231_08",  "C6231_09", "C6231_10", "C6231_11", "C6231_12", "C6231_13", "C6231_14", "C6231_15", "C6296_02", "C6296_03", "C6296_04", "C6296_08", "C6296_09", "C6296_10", "C6296_11", "C6296_12", "C6296_13", "C6296_14", "C6296_15", "C6297_02", "C6297_03", "C6297_04", "C6297_08", "C6297_09", "C6297_10", "C6297_11", "C6297_12", "C6297_13", "C6297_14", "C6297_15"), c("F6166_02", "F6166_03", "F6166_04", "F6166_08", "F6166_09", "F6166_10", "F6166_11", "F6166_12", "F6166_13", "F6166_14", "F6166_15", "F6207_02", "F6207_03", "F6207_04", "F6207_08", "F6207_09", "F6207_10", "F6207_11", "F6207_12", "F6207_13", "F6207_14", "F6207_15", "F6234_02", "F6234_03", "F6234_04", "F6234_08", "F6234_09", "F6234_10", "F6234_11", "F6234_12", "F6234_13", "F6234_14", "F6234_15", "F6291_02", "F6291_03", "F6291_04", "F6291_08", "F6291_09", "F6291_10", "F6291_11", "F6291_12", "F6291_13", "F6291_14", "F6291_15", "F6307_02", "F6307_03", "F6307_04", "F6307_08", "F6307_09", "F6307_10", "F6307_11", "F6307_12", "F6307_13", "F6307_14", "F6307_15", "F6336_02", "F6336_03", "F6336_04", "F6336_08", "F6336_09", "F6336_10", "F6336_11", "F6336_12", "F6336_13", "F6336_14",  "F6336_15", "F6341_02", "F6341_03", "F6341_05", "F6341_08", "F6341_09", "F6341_10", "F6341_11", "F6341_12", "F6341_13", "F6341_14", "F6341_15"))

rownames(data.means)<-rownames(data)
data.sd<-cbind(ko.data.sd,wt.data.sd)
#colnames(data.sd)<-c(c("neg-0h","neg-2h","neg-6h","neg-24h","neg-#48h"),c("pos-0h","pos-2h","pos-6h","pos-24h","pos-48h"))
colnames(data.means)<-c(c("C6068_01","C6068_02","C6068_03","C6068_4","C6068_08", "C6068_09", "C6068_10", "C6068_11", "C6068_12", "C6068_13", "C6068_14", "C6068_15", "C6089_02", "C6089_03", "C6089_04", "C6089_08", "C6089_09", "C6089_10", "C6089_11", "C6089_12", "C6089_13", "C6089_14", "C6089_15", "C6231_02", "C6231_03", "C6231_04", "C6231_08",  "C6231_09", "C6231_10", "C6231_11", "C6231_12", "C6231_13", "C6231_14", "C6231_15", "C6296_02", "C6296_03", "C6296_04", "C6296_08", "C6296_09", "C6296_10", "C6296_11", "C6296_12", "C6296_13", "C6296_14", "C6296_15", "C6297_02", "C6297_03", "C6297_04", "C6297_08", "C6297_09", "C6297_10", "C6297_11", "C6297_12", "C6297_13", "C6297_14", "C6297_15"), c("F6166_02", "F6166_03", "F6166_04", "F6166_08", "F6166_09", "F6166_10", "F6166_11", "F6166_12", "F6166_13", "F6166_14", "F6166_15", "F6207_02", "F6207_03", "F6207_04", "F6207_08", "F6207_09", "F6207_10", "F6207_11", "F6207_12", "F6207_13", "F6207_14", "F6207_15", "F6234_02", "F6234_03", "F6234_04", "F6234_08", "F6234_09", "F6234_10", "F6234_11", "F6234_12", "F6234_13", "F6234_14", "F6234_15", "F6291_02", "F6291_03", "F6291_04", "F6291_08", "F6291_09", "F6291_10", "F6291_11", "F6291_12", "F6291_13", "F6291_14", "F6291_15", "F6307_02", "F6307_03", "F6307_04", "F6307_08", "F6307_09", "F6307_10", "F6307_11", "F6307_12", "F6307_13", "F6307_14", "F6307_15", "F6336_02", "F6336_03", "F6336_04", "F6336_08", "F6336_09", "F6336_10", "F6336_11", "F6336_12", "F6336_13", "F6336_14",  "F6336_15", "F6341_02", "F6341_03", "F6341_05", "F6341_08", "F6341_09", "F6341_10", "F6341_11", "F6341_12", "F6341_13", "F6341_14", "F6341_15"))


rownames(data.sd)<-rownames(data)

# http://monkeysuncle.stanford.edu/?p=485
#error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
error.bar <- function(x, y, upper, lower=upper, length=0.1){

  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  #arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length)
}

timeplot<-function(probe) {
  row <- as.numeric(data.means[probe,]) # Get the numerical data

  names(row) <- colnames(data.means)  # Column names
  rMax <- max(row) + (max(row) - min(row)) * 0.1 # Y axis Max
  rMin <- max(c(min(row) - (max(row) - min(row)) * 0.1, 0)) # Y axis Min
  # print(rMin)
  if (rMin<1) {rMin<-0} # Min can't be negative'
  #KO <- grep("neg", names(row))     # Get the data for spacific #condition
  KO <- grep("C", names(row))     
 # WT <- grep("pos", names(row))
  WT <- grep("F", names(row))
  
  
  plot(row[KO], type='o', col='red', lwd=4, lty=2, ylim=c(rMin,rMax), 
       ylab='Log2 Expression', xlab='Time Points',xaxt='n',cex.lab=1.5,cex.axis=1.5)
  
  row.sd<-as.numeric(data.sd[probe,])
  error.bar(1:length(KO),row[KO],1.96*row.sd[KO]/sqrt(2),col='red',lwd=2)
  
  par(xaxt='s') # Add x axis to the plot
  #axis(1,at=1:5,labels=as.character(c(0,2,6,24,48)),lwd.ticks=3,cex.axis=1#.5) # Add x axis labels
axis(1,at=1:5,labels=as.character(c(1,2,3,4,8,9,10,11,12,13,14,15)),lwd.ticks=3,cex.axis=1.5) # Add x axis labels

  axis(2,labels=F,lwd.ticks=3,cex.axis=1.5)
  lines(row[WT], type='o', col='green', lwd=4, lty=1)
  error.bar(1:length(WT),row[WT],1.96*row.sd[WT]/sqrt(2),col='green',lwd=2)
  
 # title(as.character(c((probe),"-",annot[probe,])),font.main=1)
  title(as.character(c((probe),"-",annot[probe,])),font.main=1)
  #legend("topright", c('neg', 'pos'), 
  legend("topright", c('C', 'F'), 

         lty=c(2,1), lwd=c(rep(3,2)), col=c('red','green',cex=3,horiz=FALSE), box.lwd=3)
  box(lwd=3)
}
par(ask=F)
timeplot.gene<-function(gene) {
  probes<-rownames(annot)[annot==gene]
  sapply(probes, function(probe) {timeplot(probe)})
}

query<-readLines("clipboard")
dev.off()
#pdf("BANK1.selected.01-25-2013.pdf")
pdf(file="C:/T2_B.selected.11-06-2013.pdf")

par(mfrow=c(3,2))
sapply(query,timeplot)
dev.off()

#What is happening below? Useful if working with averaged data?
#write.table(as.matrix(annot[grep("Nfkb",annot$DEFINITION,ignore.case=T),#]),"clipboard-128",sep="\t")

# Getting correlations between conditions
#query<-readLines("clipboard") #Get the list of ILMNIDs
#writeLines(as.character(sapply(query,function(x){cor(data.means[x,grep("#neg",colnames(data.means))],
                                        #data.means[x,grep("pos",colnames(data.means))])})),"f:/111.txt")
#data.means[x,grep("F",colnames(data.means))])})),"f:/111.txt")
#writeLines(as.character(data.means[query,"pos-0h"]-#data.means[query,"neg-0h"]),"f:/111.txt")
#writeLines(as.character(data.means[query," F6166_02"]-data.means[query," #C6068_01"]),"f:/111.txt")

#write.table(annot[readLines("clipboard"),],"clipboard-#128",sep="\t",row.names=F,col.names=F)

#write.table(data.means[readLines("clipboard"),],"f:/111.txt",sep="\t",ro#w.names=F)
