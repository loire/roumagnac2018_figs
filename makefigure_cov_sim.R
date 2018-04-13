# get data
data = read.table("Final.data")
colnames(data) = c("type","pos","coverage","match","similarity")
require(tidyverse)


# First plot raw results:
svg("Brut_results.svg")
data %>% select(- match ) %>%  gather(variable, value, 3:4) %>% ggplot(aes(x=pos,y=value,group = variable)) + 
  geom_line() + facet_grid(variable ~ type ,scale="free_y") + theme_bw() + xlab("position")
dev.off()

# Plot both coverage on the same fig:
svg("Coverage.svg",width=12)
data %>% select(- match ) %>%  gather(variable, value, 3:4) %>% 
  filter(variable=="coverage") %>%
  ggplot() + geom_line(aes(group=type,color=type,x=pos,y=value),alpha=0.8) + 
  theme_bw() + ylab("coverage") + xlab("position") + 
  scale_color_brewer(type="qual",palette="Set1") + theme(text=element_text(size=16,family="Times"))
dev.off()

# Tried log but it sucks
svg("similarity_log.svg")
data %>% select(- match ) %>%  gather(variable, value, 3:4) %>% 
  filter(variable=="similarity")  %>%
  ggplot() + geom_point(aes(group=type,color=type,x=pos,y=value),alpha=0.8,size=0.3) + 
  theme_bw() + ylab("similarity") + xlab("position") + 
  scale_color_brewer(type="qual",palette="Set1") 
dev.off()

# Using zoo to compute sliding windows
require(zoo)

# Start with a 100bp windows size)

# Create subdatasets for each sequencing:
minionsim = rollapply(data %>% filter(type=="minion") %>% select(similarity), width = 100, by = 1, FUN = mean, align = "center")
illusim = rollapply(data %>% filter(type=="illumina") %>% select(similarity), width = 100, by = 1, FUN = mean, align = "center")
minionsimdf = data.frame("pos"=seq(1:length(minionsim)),"type"=rep("minion",length(minionsim)),"similariy"=minionsim)
illusimdf = data.frame("pos"= seq(1:length(illusim)),"type"=rep("illumina",length(illusim)),"similariy"=illusim)
datasim = rbind(illusimdf,minionsimdf)
# Draw fig
svg("similarity_sliding100.svg",width=12)
datasim %>% ggplot() + geom_line(aes(x=pos,y=similarity,group=type,color =type),size = 0.8,alpha=0.7) + 
  theme_bw() + ylab("similarity") + xlab("position") + 
  scale_color_brewer(type="qual",palette="Set1") + theme(text=element_text(size=16,family="Times"))
dev.off()

# Same for 10 bp
minionsim = rollapply(data %>% filter(type=="minion") %>% select(similarity), width = 10, by = 1, FUN = mean, align = "center")
illusim = rollapply(data %>% filter(type=="illumina") %>% select(similarity), width = 10, by = 1, FUN = mean, align = "center")
minionsimdf = data.frame("pos"=seq(1:length(minionsim)),"type"=rep("minion",length(minionsim)),"similariy"=minionsim)
illusimdf = data.frame("pos"= seq(1:length(illusim)),"type"=rep("illumina",length(illusim)),"similariy"=illusim)
datasim = rbind(illusimdf,minionsimdf)
svg("similarity_sliding10.svg",width=12)
datasim %>% ggplot() + geom_line(aes(x=pos,y=similarity,group=type,color =type),size = 0.8,alpha=0.7) + 
  theme_bw() + ylab("similarity") + xlab("position") + 
  scale_color_brewer(type="qual",palette="Set1") + theme(text=element_text(size=16,family="Times"))
dev.off()


# Same for 50bp
minionsim = rollapply(data %>% filter(type=="minion") %>% select(similarity), width = 50, by = 1, FUN = mean, align = "center")
illusim = rollapply(data %>% filter(type=="illumina") %>% select(similarity), width = 50, by = 1, FUN = mean, align = "center")
minionsimdf = data.frame("pos"=seq(1:length(minionsim)),"type"=rep("minion",length(minionsim)),"similariy"=minionsim)
illusimdf = data.frame("pos"= seq(1:length(illusim)),"type"=rep("illumina",length(illusim)),"similariy"=illusim)
datasim = rbind(illusimdf,minionsimdf)+ theme(text=element_text(size=16,family="Times"))
svg("similarity_sliding50.svg",width=12)
datasim %>% ggplot() + geom_line(aes(x=pos,y=similarity,group=type,color =type),size = 0.8,alpha=0.7) + 
  theme_bw() + ylab("similarity") + xlab("position") + 
  scale_color_brewer(type="qual",palette="Set1") 
dev.off()

# That's it. Inkscaped the best figures
