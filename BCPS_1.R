par(mar=c(5, 0, 2, 0))
par(oma=c(0,0,0,0))
H <- c(5,15,22,37,19,3) # Create the data for the chart.#
M <- c("Very \n Dissatisfied",
       "Somewhat \n Dissatisfied",
       "Neutral \n Somewhat",
       "Satisfied \n",
       "Very \nsatisfied",
       "No opinion/ \n Don't know")

bp <- barplot(H, col =c("dodgerblue3" ),  ylim = range(0,110),
              names.arg = M, horiz = F
              , border = NA, xlim = range(0,7),las=1,axes = F)
val <- seq(0, 100, 20)  #creating Y-axis values
text(bp, H+5 , paste(H, "%", sep="")) #automates data labels
par(las=1) #positions y-axis labels horizontally
#axis(2, at = val, labels = paste0(val, '%') ) #add % to Y-axis
#NOTE: SOMETIMES IT WILL NOT MAKE SENSE TO LABEL X-AXIS, JUST DELETE AXIS LABEL IN THOSE INSTANCES


# Graph 2
par(mar=c(5, 0, 2, 0))
par(oma=c(0,0,0,0))
counts<- structure(list(
  
  A = c(37,11),	
  B = c(26,15),	
  C = c(34,10),	
  D = c(30,46),	
  E = c(31,33)),
  
  .Names = c("Students are engaged in the \n distance learning environment.",						
             "The district's expectations for \n distance learning are clear.",					
             "My students made academic progress \n during the distance learning period.",
             "Access to technology has been a \n significant  barrier to my students learning.", 		
             "I have enough time in my day to \n give students the help they need."),
  class = "data.frame",
  row.names = c(NA, -2L)) #4L=number of numbers in each letter vector#

attach(counts)

# barplot
colors <- c("gray82", "dodgerblue3")
counts <- counts[, order(colSums(counts))]
xFun <- function(x) x/2 + c(0, cumsum(x)[-length(x)])
par(mar=c(2,18, 4, 2) + 0.1) # this sets margins to allow long labels
byc <- barplot(as.matrix(counts), horiz=TRUE, col=colors, #main="N=35"#, 
               border=NA, las=1, xaxt='n',
               ylim = range(0, 10), xlim = range(-10, 100),
               width = 1)
# labels
labs <- data.frame(x=as.vector(sapply(counts, xFun)),  # apply `xFun` here 
                   y=rep(byc, each=nrow(counts)),  # use `byc` here
                   labels=as.vector(apply(counts, 1:2, paste0, "%")), 
                   stringsAsFactors=FALSE)
labs$labels[labs$labels %in% paste0(0:(8*100)/100, "%")] <- "" #masks labels <8

invisible(sapply(seq(nrow(labs)), function(x)  # `invisible` prevents unneeded console output
  text(x=labs[x, 1:2], labels=labs[x, 3], cex=.9, font=2, col='gray18')))
# legend  (set `xpd=TRUE` to plot beyond margins!)
legend(-70,8,  legend=c("Somewhat Agree","Strongly Agree"),       fill=colors, horiz = T, bty='n', xpd=T, border = F)



# Graph 3
library(ggpubr)

  #add comment
# Adding a comment here

par(mar=c(5, 5, 5, 5) )  # this sets margins to allow long labels
#par(oma=c(0,0,0,0))
# create labels
df$label <- paste(df$group,
                  scales::percent(df$value/sum(df$value), accuracy = 1), sep = " ")

ggpubr::ggdonutchart(df, "value",
                     label = "label", # add labels
                     color = "group",fill = "group",
                     lab.pos = c("out"), #THIS PLACES DATA LABELS ON OUTSIDE OF DONUT
                     lab.adjust = 10,
                     lab.font = c(2, "plain", "black"),
                     radius = 1,
                     borders ='n',
                     palette = c("dodgerblue3", "yellow2","gray60", "slategray1","darkorange2")) +
  theme(legend.position = "none") #DELETES LEGEND
```


# Graph 4
library(tidyverse)
par(mar=c(0, 15, 8, 0))
M<-c('Blackboard Collaborate',
     'Text messages',
     'One-on-one telephone calls',
     'Online video conferencing platform',
     'Posting a written message online',
     'E-mail'
)

H<-c(41,37,33,28,20,16)

df_graph3 <- data.frame(M=M,H=H)
df_graph3 <- df_graph3 %>%
  arrange(.$H)
c <-barplot(df_graph3$H, col =c("gray82",  "gray82",  "gray82",
                                "gray82", "gray82","dodgerblue3"),
            names.arg = df_graph3$M, horiz = TRUE,
            family="Arial", border = NA,
            xlim = range(0,100), ylim = range(0, 0.08),
            axes = FALSE, width = 0.01, las=1)
text(df_graph3$H, c, labels = paste(df_graph3$H,"%"), pos = 4.5)
```


# Graph 5
library(dplyr)
library(ggplot2)
par(mar=c(0, 2, 5, 0))


df <- data.frame(
  group = c("Phone call",	
            "Text",	
            "Email",	
            "Video",	
            "Social media platforms",	
            "Other") ,
  value = c(71,64,48,13,15,12)
)
df %>%
  arrange(value) %>%
  mutate(group= forcats::fct_inorder(group)) %>%
  ggplot(aes(x=group, y=value)) +
  geom_segment( aes(x=group, xend=group, y=0, yend=value), color="dodgerblue3") +
  geom_point( color="darkorange2", size=4.2, alpha=1) +
  geom_text(aes(label = paste(value,"%")), hjust = -.3,size=3.8,family="Arial") + 
  expand_limits(y = 100)+
  theme_light() +
  coord_flip() +
  theme(
    plot.margin = margin(1, 1, 4, 1.1, "cm"),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(color = 'black', size = 12, hjust = 1),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
## ADDING COMMENT ABOUT GRAPH ##
### GRAPH 6 ###
par(mar=c(0, 15, 0, 0))

M<-c("Strongly disagree",	
     "Somewhat disagree",	
     "Neutral",
     "Somewhat agree",	
     "Strongly agree")

H<-c(18,22,16,27,15)

df_graph3 <- data.frame(M=M,H=H)
df_graph3 <- df_graph3 
c <-barplot(df_graph3$H, col =c( "slategray1", "yellow2","gray60","dodgerblue3","darkorange2"),
            names.arg = df_graph3$M, horiz = TRUE,
            family="Arial", border = NA,
            xlim = range(0,100), ylim = range(0, 0.08),
            axes = FALSE, width = 0.01, las=1)
text(df_graph3$H, c, labels = paste(df_graph3$H,"%"), pos = 4.5)




