#Run library and read in data
library(dplyr)
library(ggplot2)
library(reshape2)
library(grid)

#Update the pathway accordingly
AD<-read.csv('{path/folder containing your files}/Data of summary of reported findings.csv',h=T)

AD<-read.csv('/Users/liuchunyu/Downloads/To upload GitHub/Data of summary of reported findings.csv',h=T)

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
for(i in 6:dim(AD)[2]){
  a<-as.data.frame(print(table(as.character(AD[,i]))))
  b<-left_join(b,a,by='Var1')
}
names(b)[2:154]<-names(AD)[6:157]
View(b)
b[5,]<-c('all',colSums(b[1:3,2:154],na.rm=T))

#Rank the biomakers by studied times
b<-b[,c(1,order(200-as.numeric(b[5,2:154]))+1)]

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
#export the figure
png('{path/folder containing your files}/combined_figure.png',width =5000,height =2000,res = 300)

grid.newpage()
pushViewport(viewport(layout = grid.layout(1,10)))
print(A,vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
print(B,vp = viewport(layout.pos.row = 1, layout.pos.col = 3:10))

dev.off()


