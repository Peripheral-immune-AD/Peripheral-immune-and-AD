#Run library and read in data
library(dplyr)
library(ggplot2)
library(reshape2)
library(grid)

#Update the pathway accordingly
AD<-read.csv('{path/folder containing your files}/Data of summary of reported findings.csv',h=T)

#Row and column
dim(AD)#251,157

#--------Panel A
names(AD)[5]<-'N.of.biomarkers'#Shorten the name of the column
AD$N.of.biomarkers2<-cut(AD$N.of.biomarkers,breaks=c(-Inf,2,5,Inf),labels=c('1-2','3-5','>5'))#Categorization
table(AD$N.of.biomarkers2)

A<-ggplot(data=AD,aes(x=N.of.biomarkers2))+ geom_bar(color='black')+xlab('Number of biomakers in each study')+ylab('Number of studies')+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),legend.position = c(0.9, 0.9),legend.text = element_text(size =12),axis.line = element_line(colour = "black"),axis.title.x = element_text(size =12),axis.title.y = element_text(size = 12),axis.text.x = element_text(size =10),axis.text.y = element_text(size =10))

#---------Panel B
#Summarize the reported results
b<-data.frame('Var1'=c('+','-','0','NR'))
for(i in 6:157){
  a<-as.data.frame(print(table(as.character(AD[,i]))))
  b<-left_join(b,a,by='Var1')
}
names(b)[2:153]<-names(AD)[6:157]
View(b)
b[5,]<-c('all',colSums(b[1:3,2:153],na.rm=T))

#Rank the biomarkers by studied times
b<-b[,c(1,order(200-as.numeric(b[5,2:153]))+1)]

#Top 20 studied biomarkers
b<-b[,1:21]
#Add the name
names(b)<-c('Var1','IL-6','TNF-α','CRP','IL-1β','IL-8','IL-10','IFN-γ','MCP-1','ACT','IL-1α','IL-2','IL-18','IL-4','IL-12','Lymphocyte','MIP-1α',"TGF-β1","IL-5",'IL-7',"CXCL-10")

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
#Export the figure3
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
for(i in 6:157){
  a<-as.data.frame(print(table(as.character(AD[,i]))))
  b<-left_join(b,a,by='Var1')
}
names(b)[2:153]<-names(AD)[6:157]
View(b)
b[5,]<-c('all',colSums(b[1:3,2:153],na.rm=T))

#Rank the biomarkers by studied times
b<-b[,c(1,order(200-as.numeric(b[5,2:153]))+1)]

dim(b)#153          

#Add the name
names(b)<-c('Var1','IL-6','TNF-α','CRP','IL-1β','IL-8','IL-10','IFN-γ','MCP-1','ACT','IL-1α','IL-2','IL-18','IL-4','IL-12','Lymphocyte','MIP-1α',"TGF-β1","IL-5",'IL-7',"CXCL-10","ICAM-1","IL-13",'CD8',"VEGF","IgG","IL-1ra" ,"IL-16","IL-17","CCL4","CD4","AβN-40", "AβN-42","CD40","Eotaxin","IL-3","IL-6R","GM-CSF","Neutrophil", "Monocyte","IL-12 p70", "TNF-β","IFN-α","MCP-3","T cell",
"VCAM-1","Leucocyte","Neopterin","IgM","IgA","MMP-9","IL-11","IL-15","CCL5",'CSF1',"NK cell","B cell","CD14","IL-2R","IL-9","IL-23","MIP-1","G-CSF","Fibrinogen","CD8-T-cell","TNFR1","C3","NGF","TRAIL","IL-18-BP","IL-33","TNF-αR",'CCL27',"CXCL-1","CXCL-9","NLR","CD3-T-cell","CD4-T-cell","CD4/8","CD56","CD3","CD19","TNFR2","T Lymphocyte","C4","HLA-A2","p-tau-181","t-tau","SAA","α2M","HGF","MIF","MCP-4","IL-17A",
"sST2","OPN","TGF-α","MCP-5","TRAC","MIP-4","MIP-3β","MDC","YKL-40","CXCL12","LCN2","CD3-B-cell","CD19-B-cell","CD16/56","CD80","CD86","CD2","CD7","CD25","CD28","CD34","Eosinophil","sCR1","C2","C5","C9","FH","FI","HLA-A9","HLA-A10","HLA-A11","HLA-B5","HLA-B7","HLA-B12","HLA-B15","HLA-B16","HLA-Bw22","HLA-B35","HLA-B40","HLA-Cw1","HLA-Cw3","HLA-Cw4","HLA-Cw7",
"IgE","ASC","sAPPα","sAPPβ","NfL","TMIP-1","MMP-1","MMP-2","HHV-6-IgG","anti-tau-IgG","anti-NFH-IgG","anti-NFL-IgG","SIRT1","TLR4","GDNF","CD95")

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

#Export the figure
ggsave('{path/folder containing your files}/AD_result_figure_supple.png',width=50,height=20,units=c('cm'))

ggplot(data=b_1,aes(x=variable,y=value,fill=Var1))+ geom_bar(stat= 'identity', position = 'stack',color='black',width=0.8)+scale_fill_manual(values=colors,name=NULL)+xlab('Examined biomarkers across studies')+ylab('Number of studies')+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),legend.position = c(0.9, 0.9),legend.text = element_text(size =12),axis.line = element_line(colour = "black"),axis.title.x = element_text(size =12),axis.title.y = element_text(size = 12),axis.text.x = element_text(size =10,angle=90,hjust =1, vjust = 0.5),axis.text.y = element_text(size =10))

dev.off()





