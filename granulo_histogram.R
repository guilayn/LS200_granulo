rm(list=ls(all=TRUE)) 

#### 1.PREP ####
#__1.1 loading libraries####
libraries=c("Hmisc","colorspace","corrplot","pvclust","mixOmics","RColorBrewer",
            "FactoMineR","missMDA","FactoMineR","ggplot2", "tidyr",
            "gplots","scales","grid","plyr","gridExtra",
            "devtools","factoextra","ggrepel", "viridis",
            "WriteXLS","dendextend","cluster","reshape2","readxl","pvclust","pls",
            "sjstats",
            "my.stats","colourpicker","dplyr")

install_this=libraries[which(lapply(libraries, require, character.only = TRUE) == F)]
install.packages(install_this)
lapply(install_this, require, character.only  = T)
install_github("guilayn/my.stats")
## IMPORTING DATA ####
data_wd = "C:/Users/Felipe/OneDrive - Suez Environnement/THESE-FELIPE/00_EXPERIMENTS/2018_06 granulo/"
files=list.files(data_wd,pattern="\\.CVS")
sample_names = substr(files,1,9)
### PLOTS ####
for (i in 1:length(files)) {
data=read.csv(paste0(data_wd,files[i]),skip = 49)[,c(2,3)]
data_i = data.frame(sample=sample_names[i],data)
colnames(data_i)=c("Sample","Particle size (µm)","Volume (%)")

if (i == 1) {
  data_all = data_i
  }
if (i > 1) {
  data_all = rbind(data_all,data_i)
  }
colnames(data)=c("Particle size (µm)","Volume (%)")

data$`Volume (%)`
plot=ggplot(data=data,aes(x=`Particle size (µm)`,y=`Volume (%)`))
plot = plot +
  geom_line() +
  coord_cartesian(xlim = c(0,250)) +
  ggtitle(paste0("Sample: ",sample_names[i])) +
  theme_bw()
print(plot)
}
plot2=ggplot(data=data_all,aes(x=`Particle size (µm)`,y=`Volume (%)`))
plot2 = plot2 +
  geom_line() +
  facet_wrap(~Sample) +
  ggtitle("Particle size distribution") +
  theme_bw()

plot3 = plot2 + coord_cartesian(xlim = c(0,250)) #,ylim = c(0,3)) +
plot4 = plot2 + coord_cartesian(xlim = c(0,2000)) #,ylim = c(0,3)) +
ggsave(paste0(data_wd,my.functions.datetimelabel(),"plot_range_250.pdf"),plot3,device = "pdf")
ggsave(paste0(data_wd,my.functions.datetimelabel(),"plot_range_2000.pdf"),plot4,device = "pdf")

# % IN THE RANGE #
range_particle = c(50,150)
#data_range = data_all[which((data_all$`Particle size (µm)` >= range_particle[1]) & (data_all$`Particle size (µm)` <= range_particle[2])),] 
#more beautiful option than the manual code above
data_range_summary = data_all %>% 
  filter((`Particle size (µm)` >= range_particle[1]) & (`Particle size (µm)` <= range_particle[2])) %>% 
  group_by(Sample) %>% summarise(Range_volume = sum(`Volume (%)`,na.rm = T))
 summary(data_range_summary$Range_volume)
 