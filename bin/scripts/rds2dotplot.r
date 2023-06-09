library(Seurat)
library(ggplot2)


parser = argparse::ArgumentParser(description="Script for, cellhashR")
parser$add_argument('-I','--input', help='input rds')
parser$add_argument('-M','--marker', help='input marker.csv')
parser$add_argument('-O','--outdir', help='outdir')
parser$add_argument('-G','--genelist', help='input markergene.list')
args = parser$parse_args()

object <- readRDS(args$input)

if(!is.null(args$genelist) ){
    data <- read.table(args$genelist,header=T,sep="\t")
    output<-data$MARKER_GENE
    output <- unique(output)
}



if(!is.null(args$marker) ){
    markerlist <- read.csv(args$marker)
    num <- length(unique(markerlist$cluster))
    output <- head(markerlist[(markerlist$cluster == 0),"X"],n=10)
    for (i in seq_len(num-1)){
        n <- head(markerlist[(markerlist$cluster == i),"X"],n=10)
        output <- append(output,n)
    }
}



p <- DotPlot(object, features = output)+theme(axis.text.x = element_text(angle = 45))
ggsave(paste(args$outdir,"/","DotPlot.pdf",sep=""),p,width=24,height=6)

