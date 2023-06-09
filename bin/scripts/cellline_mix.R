library(tidyverse)
library(cowplot)
library(patchwork)

parser = argparse::ArgumentParser(description="Script to cell mix, gene and UMI")
parser$add_argument('-I','--input', help='input counts.tsv')
parser$add_argument('-O','--out',help='out directory')
args = parser$parse_args()

#file <- "Vdo6_cellline_1_cell_counts.tsv"
file <- args$input
color <- c("Human" = "#1B9E77", "Mouse" =  "#D95F02", "Mix"="#B3B3B3")

data <- read_tsv(file)
data <- data %>% mutate(Total = Human_UB + Mouse_UB) %>%
  mutate(Species_UMI = case_when(Human_UB/Total < 0.1 ~ "Mouse",
                                 Mouse_UB/Total < 0.1 ~ "Human",
                                 T ~ "Mix")) %>%
  mutate(Species_UMI = factor(Species_UMI, levels = c("Human", "Mouse", "Mix")))

smy <- summary(data$Species_UMI)
ratio <- round(smy/sum(smy)*100,2)

p1 <- ggplot(data, aes(x=Human_UB, y=Mouse_UB, color=Species_UMI)) + 
  geom_point() +
  scale_color_manual(
    labels=c(paste0("Human: ", smy['Human'], " (", ratio['Human'], "%)"),
             paste0("Mouse: ", smy['Mouse'], " (", ratio['Mouse'], "%)"),
             paste0("Mix: ", smy['Mix'], " (", ratio['Mix'], "%)")),
    values=color) +
  xlab("Human UMI counts in per cell") + 
  ylab("Mouse UMI counts in per cell") + 
  theme_cowplot() +
  labs(colour = "") +
  theme(
    legend.position = c(1, 1),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

data_p2 <- data %>% filter(Species_UMI != "Mix") %>%
  mutate(GN = ifelse(Human_GN >= Mouse_GN, Human_GN, Mouse_GN))
smy_p2 <- data_p2 %>% group_by(Species_UMI) %>% summarise(mean = floor(mean(GN)))
gn_mean_human <- smy_p2 %>% filter(Species_UMI == "Human") %>% pull(mean)
gn_mean_mouse <- smy_p2 %>% filter(Species_UMI == "Mouse") %>% pull(mean)
p2 <- ggplot(data_p2, aes(x=Species_UMI, y=GN, fill=Species_UMI)) + 
  geom_violin(trim=T, color="white", adjust=1.2, scale = 'width') + 
  geom_boxplot(width=0.2, position=position_dodge(0.5), outlier.colour = NA, fill="white") + 
  scale_fill_manual(values=color)+
  theme_cowplot() +
  theme(legend.position = "none") +
  ylab("# detected genes") + xlab("")

data_p3 <- data %>% filter(Species_UMI != "Mix") %>%
  mutate(UB = ifelse(Human_UB >= Mouse_UB, Human_UB, Mouse_UB))
smy_p3 <- data_p3 %>% group_by(Species_UMI) %>% summarise(mean = floor(mean(UB)))
ub_mean_human <- smy_p3 %>% filter(Species_UMI == "Human") %>% pull(mean)
ub_mean_mouse <- smy_p3 %>% filter(Species_UMI == "Mouse") %>% pull(mean)
p3 <- data_p3 %>% ggplot(aes(x=Species_UMI, y=UB, fill=Species_UMI)) + 
  geom_violin(trim=T, color="white", adjust=1.2, scale = 'width') + 
  geom_boxplot(width=0.2, position=position_dodge(0.5), outlier.colour = NA, fill="white") + 
  scale_fill_manual(values=color)+
  theme_cowplot() +
  theme(legend.position = "none") +
  ylab("# detected UMIs") + xlab("")

p <- p1 | (p2 | p3)
#ggsave(paste(args$out,"/","Cell_Gene_UMI.png",sep=""),width = 12,height = 6)
png(paste(args$out,"/","Cell_Gene_UMI.png",sep=""),width=1200,height=600,res=80)
print(p)
dev.off()

if(length(ub_mean_mouse) == 0){ub_mean_mouse=0}
if(length(gn_mean_human) == 0){gn_mean_human=0}
if(length(gn_mean_mouse) == 0){gn_mean_mouse=0}
if(length(ub_mean_human) == 0){ub_mean_human=0}
cat(paste("Human mean genes,", gn_mean_human,"\n",sep=""),file=paste(args$out,"/cell_gene.stat",sep=""))
cat(paste("Mouse mean genes,", gn_mean_mouse,"\n",sep=""),file=paste(args$out,"/cell_gene.stat",sep=""), append=T)
cat(paste("Human mean UMIs,", ub_mean_human,"\n",sep=""),file=paste(args$out,"/cell_gene.stat",sep=""), append=T)
cat(paste("Mouse mean UMIs,", ub_mean_mouse,"\n",sep=""),file=paste(args$out,"/cell_gene.stat",sep=""), append=T)


