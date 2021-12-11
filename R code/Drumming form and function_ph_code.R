#Waibira PH phases plots----
#library
library(ggplot2)
setwd("/Users/vestaeleuteri/Desktop/Drumming Individual differences Analyses/Ph phase plots")

#Ph_start data preparation
setwd("~/Desktop/Drumming Coding sheets/Waibira")
data=read.table(file="/Users/vestaeleuteri/Desktop/Drumming Coding sheets/Waibira/Drumming form and function_ph_start_data.csv", header=T, sep=",", stringsAsFactors=T)
str(data)
anyNA(data)

cbPalette <- c("#F0E442", "#0072B2", "#E69F00")

#Ph_start plot (Figure 5)
ggplot(data, aes(fill=start_phase, y=percentage, x=individual)) + scale_fill_manual(values=c("black", "light grey", "dark grey")) +
  geom_bar(position="fill", stat="identity") + theme_classic() + theme(axis.title.x = element_text(colour="black", size=14, vjust=0.2),
                                                                        axis.text.x  = element_text(colour="black", angle=0, vjust=0.4, size=12),
                                                                        axis.title.y = element_text(colour="black", size=14, vjust=2.0),
                                                                        axis.text.y  = element_text(colour="black", angle=0, vjust=0.4, size=12),
                                                                        legend.title = element_text(colour="white", size=12),
                                                                        legend.text = element_text(colour="black", size = 12)) 


setwd("/Users/vestaeleuteri/Desktop/Drumming Individual differences Analyses/Ph phase plots")
dev.copy2pdf(file="ph_start.pdf", out.type = "pdf") #add pdf


#Ph_end data preparation
setwd("~/Desktop/Drumming Coding sheets/Waibira")
data=read.table(file="/Users/vestaeleuteri/Desktop/Drumming Coding sheets/Waibira/Drumming form and function_ph_end_data.csv", header=T, sep=",", stringsAsFactors=T)
str(data)
anyNA(data)

#Ph_end plot (Figure 5)
ggplot(data, aes(fill=end_phase, y=percentage, x=individual)) + scale_fill_manual(values=c("black", "light grey", "dark grey")) +
  geom_bar(position="fill", stat="identity") + theme_classic() + theme(axis.title.x = element_text(colour="black", size=14, vjust=0.2),
                                                                       axis.text.x  = element_text(colour="black", angle=0, vjust=0.4, size=12),
                                                                       axis.title.y = element_text(colour="black", size=14, vjust=2.0),
                                                                       axis.text.y  = element_text(colour="black", angle=0, vjust=0.4, size=12),
                                                                       legend.title = element_text(colour="white", size=12),
                                                                      legend.text = element_text(colour="black",size = 12))


setwd("/Users/vestaeleuteri/Desktop/Drumming Individual differences Analyses/Ph phase plots")
dev.copy2pdf(file="ph_end.pdf", out.type = "pdf") #add pdf

