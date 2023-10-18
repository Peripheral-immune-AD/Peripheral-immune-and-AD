#Run library and read in data
library(dplyr)
library(ggplot2)
library(reshape2)
library(grid)

#Update the pathway accordingly
AD<-read.csv('{path/folder containing your files}/Data of summary of reported findings.csv',h=T)

#N of row and column
dim(AD)#286 192

#--------Panel A
names(AD)[5]<-'N.of.biomarkers'#Shorten the name of the column

# for(i in 1:286){
#   a<-as.data.frame(table(as.character(AD[i,6:192])))
#   b<-187-a[a$Var1=='NR','Freq']
#   AD$N.of.biomarkers[i]<-b
# }
# 
AD$N.of.biomarkers2<-cut(AD$N.of.biomarkers,breaks=c(-Inf,2,5,Inf),labels=c('1-2','3-5','>5'))#Categorization
table(AD$N.of.biomarkers2)

A<-ggplot(data=AD,aes(x=N.of.biomarkers2))+ geom_bar(color='black')+xlab('Number of biomakers in each study')+ylab('Number of studies')+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),legend.position = c(0.9, 0.9),legend.text = element_text(size =12),axis.line = element_line(colour = "black"),axis.title.x = element_text(size =12),axis.title.y = element_text(size = 12),axis.text.x = element_text(size =10),axis.text.y = element_text(size =10))

#---------Panel B
#Summarize the reported results
b<-data.frame('Var1'=c('+','-','0','NR'))
for(i in 6:192){
  a<-as.data.frame(print(table(as.character(AD[,i]))))
  b<-left_join(b,a,by='Var1')
}
names(b)[2:188]<-names(AD)[6:192]
View(b)
b[5,]<-c('all',colSums(b[1:3,2:188],na.rm=T))

#Rank the biomarkers by studied times
b<-b[,c(1,order(200-as.numeric(b[5,2:188]))+1)]

#Top 20 studied biomarkers
b<-b[,1:21]
#Add the name
names(b)<-c('Var1','IL-6','TNF-α','CRP','IL-1β','IL-10','IL-8','IFN-γ','MCP-1','ACT','IL-1α','IL-2','IL-4','IL-18','Lymphocyte','IL-12',"TGF-β1",'IL-7',"IL-5",'MIP-1α',"IL-6R")

#Prepare the figure data
b<-b[-c(4:5),]
b_1<-melt(b,id='Var1')
View(b_1)
b_1$Var1<-factor(b_1$Var1,levels=c('+','0','-'),labels=c('Positive','Null','Negative'))
b_1$value[which(is.na(b_1$value)==TRUE)]<-0
b_1$value<-as.numeric(b_1$value)

dim(b_1)#60 3

#Set the colors
colors<-c('#9DC3E7', 'gray', '#ECAD9E')

B<-ggplot(data=b_1,aes(x=variable,y=value,fill=Var1))+ geom_bar(stat= 'identity', position = 'stack',color='black',width=0.8)+scale_fill_manual(values=colors,name=NULL)+xlab('Top 20 most examined biomarkers across studies')+ylab('Number of studies')+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),legend.position = c(0.9, 0.9),legend.text = element_text(size =12),axis.line = element_line(colour = "black"),axis.title.x = element_text(size =12),axis.title.y = element_text(size = 12),axis.text.x = element_text(size =10),axis.text.y = element_text(size =10))

#----------------------
#Combind figure A and B and export the combined figure ——> Main Figure3
png('{path/folder containing your files}/combined_figure.png',width =5000,height =2000,res = 300)

grid.newpage()
pushViewport(viewport(layout = grid.layout(1,10)))
print(A,vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
print(B,vp = viewport(layout.pos.row = 1, layout.pos.col = 3:10))

dev.off()

#---------------------------------------------------------------------
#Supplementary figures
#Summarize the reported results
b<-data.frame('Var1'=c('+','-','0','NR'))
for(i in 6:192){
    a<-as.data.frame(print(table(as.character(AD[,i]))))
    b<-left_join(b,a,by='Var1')
  }
  names(b)[2:188]<-names(AD)[6:192]
  View(b)
  b[5,]<-c('all',colSums(b[1:3,2:188],na.rm=T))
  
  #Rank the biomarkers by studied times
  b<-b[,c(1,order(200-as.numeric(b[5,2:188]))+1)]
  
#Add the name
names(b)<-c('Var1','IL-6','TNF-α','CRP','IL-1β','IL-10','IL-8','IFN-γ','MCP-1','ACT','IL-1α','IL-2','IL-4','IL-18','Lymphocyte','IL-12',"TGF-β1",'IL-7',"IL-5",'MIP-1α',"IL-6R","CXCL-10","IL-17","MIP-1β","CD8","ICAM-1","VEGF","IL-1ra","IL-13","CD4","CD40","IgG" ,"Eotaxin-1","IL-16","GM-CSF","NK cell","Neutrophil","NLR", "Monocyte","AβN-40", "AβN-42","IL-3","IL-12 p70", "TNF-β","T cell","VCAM-1","Leucocyte","Eotaxin-3","IL-9","IL-15","IFN-α","MCP-3","RANTES","G-CSF","B cell","Neopterin","IgM","IgA",
"MMP-9","CD14","IL-11","IL-17A","IL-23","TNF-αR","CSF1","CD4 T cell","CD8 T cell","α2M","IL-2R","MIP-1","CXCL-1","CXCL-9","Fibrinogen","Treg cell","T helper cell","CD3 T cell","CD4/8","CD56","TNFR1","T Lymphocyte","C3","SAA","NGF","HGF","FGF","TRAIL","MCP-4","BDNF","IL-18-BP","IL-33","TGF-α","MDC",'CCL27',"CXCL12","CD19 B cell","CD16/56","CD3","CD19","CD163","TNFR2","Eosinophil","C4","HLA-A2","p-tau-181","t-tau","MIF","PDGF-BB","PLR",'SII',"CD4 Tn/Tm","CD8 Tn/Tm","ENRAGE","IL-12p35","IL-21","sST2","OPN","MCP-5","TARC","MIP-4","MIP-3β","YKL-40","CXCL-16","LCN2","CD14 monocytes", "CD3 B cell","CD80","CD86","CD2","CD5","CD7","CD25","DDP-4","CD28","CD34","CD91","LIF-R","SLAMF-1","Basophil","sCR1","C2","C5","C9","FH","FI","HLA-A9","HLA-A10","HLA-A11","HLA-B5","HLA-B7","HLA-B12","HLA-B15","HLA-B16","HLA-Bw22","HLA-B35","HLA-B40","HLA-Cw1","HLA-Cw3","HLA-Cw4","HLA-Cw7",
"IgE","ASC","sAPPα","sAPPβ","NfL","TMIP-1","MMP-1","MMP-2","MMP-19","HHV-6-IgG","anti-tau-IgG","anti-NFH-IgG","anti-NFL-IgG","SIRT1","TLR4","GDNF","β2M","CD95","NF-κB","ROR-γ",'GATA-3',"NFATc-1","GLR","LMR","Naive CD4 cell","Naive CD8 cell","Memory CD4 cell","Memory CD8 cell","Memory B cell")

#Prepare the figure data
b<-b[-c(4:5),]
b_1<-melt(b,id='Var1')
View(b_1)
b_1$Var1<-factor(b_1$Var1,levels=c('+','0','-'),labels=c('Positive','Null','Negative'))
b_1$value[which(is.na(b_1$value)==TRUE)]<-0
b_1$value<-as.numeric(b_1$value)

dim(b_1)#456 3

#Set the colors
colors<-c('#9DC3E7', 'gray', '#ECAD9E')

#Export the figure ——> Supplementary figure1
ggsave('{path/folder containing your files}/AD_result_figure_supple.png',width=50,height=20,units=c('cm'))

ggplot(data=b_1,aes(x=variable,y=value,fill=Var1))+ geom_bar(stat= 'identity', position = 'stack',color='black',width=0.8)+scale_fill_manual(values=colors,name=NULL)+xlab('Examined biomarkers across studies')+ylab('Number of studies')+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),legend.position = c(0.9, 0.9),legend.text = element_text(size =12),axis.line = element_line(colour = "black"),axis.title.x = element_text(size =12),axis.title.y = element_text(size = 12),axis.text.x = element_text(size =10,angle=90,hjust =1, vjust = 0.5),axis.text.y = element_text(size =10))

dev.off()





