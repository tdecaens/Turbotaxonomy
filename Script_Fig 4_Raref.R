library(iNEXT)
library(ggplot2)
library(cowplot)
library(vegan)

setwd("~/Documents/Univ Montpellier 2/Pulications/Articles en cours/In prep Decaëns et al Mitaraka")

m<-read.csv2("Specimens.csv") ### load collecting data for whole dataset
tc<-as.data.frame.array(t(table(m$Exact.Site.2, m$OTU)))
tc.pa<-decostand(tc, method="pa")
ew_list<-list(Mitaraka=tc.pa)
CR<-iNEXT(ew_list, data="incidence_raw")
a<-ggiNEXT(CR,type=1) +
  theme_classic(base_size = 20) +
  scale_color_manual(values=c("forestgreen")) +
  scale_fill_manual(values=c("forestgreen")) +
  theme(legend.position = "right") +
  expand_limits(x=0, y=0)

tc2<-as.data.frame.array(table(m$OTU, m$Microhabitat.2))
CR2<-iNEXT(tc2)
b<-ggiNEXT(CR2,type=1) +
  theme_classic(base_size = 20) +
  scale_color_manual(values=c("darkolivegreen","yellow3","cadetblue","darkorange3")) +
  scale_fill_manual(values=c("darkolivegreen","yellow3","cadetblue","darkorange3")) +
  theme(legend.position = "right") +
  expand_limits(x=0, y=0)

tc3<-as.data.frame.array(table(m$OTU, m$Habitat))
tc3["Inselbergs"]<-rowSums(tc3[,c(2:4,9)])
tc3<-tc3[,-c(1:4, 8:10)]
colnames(tc3)<-c("Lowland forests","Plateau forests","Slope forests","Inselberg habitats")
CR3<-iNEXT(tc3)
c<-ggiNEXT(CR3,type=1) +
  theme_classic(base_size = 20) +
  scale_color_manual(values=c("darkolivegreen","yellow3","cadetblue","darkorange3")) +
  scale_fill_manual(values=c("darkolivegreen","yellow3","cadetblue","darkorange3")) +
  theme(legend.position = "right") +
  expand_limits(x=0, y=0)

#### Richesse spécifique à l'échelle des communautés
S<-as.data.frame(specnumber(t(tc)))
S<-as.data.frame(S[-c(25:27),]); colnames(S)<-"S"
Habitats<-c("Plateau", "Inselberg", "Inselberg", "Lowland", "Lowland", "Lowland", "Plateau", "Plateau", "Plateau", "Plateau", "Inselberg", "Inselberg", "Inselberg", "Inselberg", "Slope", "Slope", "Slope", "Slope", "Lowland", "Inselberg", "Inselberg", "Inselberg", "Inselberg", "Plateau")
S<-cbind(S,Habitats)
d<-ggplot(S, aes(x=Habitats, y=S, color=Habitats)) +
  theme_classic(base_size = 20) +
  geom_boxplot(outlier.size=2) +
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) +
  scale_color_manual(values=c("darkolivegreen","yellow3","cadetblue","darkorange3")) +
  geom_jitter(shape=16, position=position_jitter(0.2), color="grey") +
  labs(y = "Species diversity")

### 15*10
plot_grid(a, c, b, d,
          labels=c("A","B","C", "D"), label_size=25,
          label_x = 0.5,ncol = 2, nrow = 2,
          rel_widths = c(1, 1.1))

kruskal.test(S ~ Habitats, data = S)
pairwise.wilcox.test(S$S, S$Habitats,
                     p.adjust.method = "none")

aggregate(x= S$S,     
          
          # Specify group indicator
          by = list(S$Habitats),      
          
          # Specify function (i.e. mean)
          FUN = mean)
