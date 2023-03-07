#Run library and read in data
library(dplyr)
library(ggplot2)
library(reshape2)
library(foreign)
library(tidyverse)
library(ddply)
library(tidyr)

#Update the pathway accordingly
setwd("G:\\My Drive\\Dr. Grace Noppert\\Systematic review-Peripheral immunue & ADRD\\Quality assessment")

quality_assessment<-read.csv("G:\\My Drive\\Dr. Grace Noppert\\Systematic review-Peripheral immunue & ADRD\\Quality assessment\\Data of quality assessment.csv",h=T)

#Total score distribution

jpeg("Total score distribution.jpg", res = 500, w = 6000, h = 4000)

ggplot(quality_assessment, aes(Total)) + 
  geom_histogram(position = "identity", binwidth = 1, color="black") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), axis.text = element_text(size=16), axis.title=element_text(size=18, face="bold"),
        legend.position = "none", legend.key.height=unit(0.75, "cm"), legend.text = element_text(size = 12), 
        legend.key=element_blank(), legend.box="vertical", 
        panel.background = element_blank()) +
  scale_x_continuous(limits = c(1, 20), breaks = c(5, 10, 15, 20), label = c("5", "10", "15", "20")) +
  scale_y_continuous(limits = c(0, 50), breaks = c(0, 10, 20, 30, 40, 50), label = c("0", "10", "20", "30", "40", "")) +
  geom_vline(xintercept = c(13.5), size = 1, linetype = "dashed") + 
  geom_vline(xintercept = c(6.5), size = 1, linetype = "dashed") + 
  ylab("Number of studies") + xlab("Total score") +
  annotate("text", x= 3, y = 48, label = "Low", size = 6.8) +
  annotate("text", x= 10, y = 48, label = "Medium", size = 6.8) + 
  annotate("text", x= 17, y = 48, label = "High", size = 6.8)

dev.off()
 
#Score plot for each study

names(quality_assessment)

quality_assessment1 <- quality_assessment %>% distinct(PMID, .keep_all = TRUE) %>% 
  arrange(desc(Total), desc(Representativenss), desc(Sample_size), desc(Follow_up), 
          desc(ADRD_assessment), desc(Biomarker_assessment), desc(Systematic_assessment), desc(Comparison_selection),
          desc(Statistical_methods), desc(Confounding_adjustment), desc(Finding_reporting))

quality_assessment1$Id <- (1 : 251)

quality_assessment1 <- select(quality_assessment1, Representativenss:Id)

quality_assessment1 <- quality_assessment1 %>% gather(item, score, Representativenss:Finding_reporting)

quality_assessment1$item <- as.factor(quality_assessment1$item)

levels(quality_assessment1$item)

quality_assessment1$item <- factor(quality_assessment1$item, levels=c('Finding_reporting', 'Confounding_adjustment', 'Statistical_methods',
                                             'Comparison_selection', 'Systematic_assessment', 'Biomarker_assessment',
                                             'ADRD_assessment', 'Follow_up', 'Sample_size', 'Representativenss'))

cols <- c("2" = "chartreuse3", "1" = "gold2", "0" = "red3")

jpeg("Peripheral immune and AD quality assessment.jpg", res = 500, w = 6000, h = 3000)

ggplot(quality_assessment1, aes(x = Id, y = item, fill = as.factor(score))) + 
  geom_tile(color = "white", lwd = 1, linetype = 1) + 
  scale_fill_manual(values = cols, labels = c("Good", "Fair", "Poor")) +
  coord_fixed(ratio = 8) +
  xlab("Study ID") + ylab("") +
  scale_y_discrete(label = c("Finding\nReport", "Confounding\nAdjustment", "Statistical\nMethods",
                             "Comparison\nSelection", "Biomarker\nAssessment", "Laboratory\nMethods",
                             "Alzheimer\nAssessment", "Follow-up", "Sample\nSize", "Sampling\nRepresentativeness")) + 
  scale_x_continuous(limits = c(0, 252), expand = c(0, 0), breaks = c(1, 21, 41, 61, 81, 101, 121, 141, 161, 181, 201, 221, 241)) +
  geom_vline(xintercept = c(12.5), size = 1, linetype = "dashed") + 
  geom_vline(xintercept = c(193.5), size = 1, linetype = "dashed") + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), axis.text = element_text(size=10), axis.title=element_text(size=12, face="bold"),
        legend.key.height=unit(0.75, "cm"), legend.text = element_text(size = 12), legend.position="bottom",
        legend.key=element_blank(), legend.title=element_blank(), legend.box="vertical", 
        panel.background = element_rect(colour = "black", size=1),
        axis.ticks.length=unit(-0.1, "cm"), axis.ticks.margin=unit(0.2, "cm"),
        plot.margin = margin(0, 1, 0, 0, "cm"))

dev.off()

#Proportion for each item
jpeg("Score by domain.jpg", res = 500, w = 6000, h = 4000)

ggplot(quality_assessment1, aes(y = item, fill = as.factor(score))) + 
  geom_bar(position = "fill") + 
  scale_fill_manual(values = cols, labels = c("Good", "Fair", "Poor")) +
  scale_x_continuous(breaks = seq(from = 0, to = 1, by = 0.1), label = c("0", "10", "20", "30", "40", "50", "60", "70", "80", "90", "100")) +
  xlab("Proportion of studies (%)") + ylab("Quality assessment domains") +
  scale_y_discrete(label = c("Finding\nReport", "Confounding\nAdjustment", "Statistical\nMethods",
                             "Comparison\nSelection", "Biomarker\nAssessment", "Laboratory\nMethods",
                             "Alzheimer\nAssessment", "Follow-up", "Sample\nSize", "Sampling\nRepresentativeness")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), axis.text = element_text(size=16), axis.title=element_text(size=18, face="bold"),
        legend.key.height=unit(0.75, "cm"), legend.text = element_text(size = 12), legend.position="bottom",
        legend.key=element_blank(), legend.title=element_blank(), legend.box="vertical", 
        panel.background = element_blank())

dev.off()
