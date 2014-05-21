#############

library (ggplot2)
library (plyr)
library(grid)


listpoli<-read.table("poli_123.txt",header=T, dec=",")

colnames(listpoli)<-c("prj",  "sbj",    "polo",	
                      "invest",	"fin",	"call")
head(listpoli)
str(listpoli)
levels(listpoli$call)<-c("1^","2^","3^")

attrsbj<-read.table("attr_sbj.txt",
                    header=T, dec=",", stringsAsFactors=F)
head(attrsbj)
str(attrsbj)

###########################################################

head(listpoli)
hist(table(listpoli$sbj), breaks=100)
table(table(listpoli$sbj))


sbjtype<-sapply(listpoli$sbj, function (x) attrsbj$type[attrsbj$sbj==x]  ) 

#  mytype<-lapply(listpoli$sbj, function (x) attrsbj[attrsbj$sbj==x,]  ) 
str(sbjtype)

lstpoli<-cbind(listpoli,sbjtype)
head(lstpoli)
str(lstpoli)
#############################


#################################################################
qplot(polo, data=listpoli, fill=call)

#poloprj<- sapply(levels(listpoli$polo), 
#       function(x) length(levels(factor(listpoli$prj[listpoli$polo==x]))) )


polosx<-ddply(listpoli, .(polo,call), summarise,
            npart= length(polo),
            numprj= length(levels(factor(prj))),
            numsbj= length(levels(factor(sbj))),
            totinv= sum(invest),
            meaninv=mean(invest),
            totfin= sum(fin),
            meanfin=mean(fin)
              )
str(polosx)
head(polosx)

###########################################################


### >>>>>> multiple plots in one graphic
# define function to create multi-plot setup (nrow, ncol)

vp.setup <- function(x,y){
    # create a new layout with grid
    grid.newpage()
    # define viewports and assign it to grid layout
    pushViewport(viewport(layout = grid.layout(x,y)))
}
# define function to easily access layout (row, col)
vp.layout <- function(x,y){
    viewport(layout.pos.row=x, layout.pos.col=y)
}
# define graphics

p1 <-qplot(polo,npart, data=polosx, fill=call, 
      geom="bar", stat="identity",  
      main=" total participations POLI",  ylab="")

p2 <-qplot(polo, numprj, data=polosx, fill=call, 
      geom="bar",stat="identity" ,  
      main=" n. projects in POLI",  ylab="")

p3 <-qplot(polo, numsbj, data=polosx, fill=call, 
      geom="bar",stat="identity",  
      main=" n. subjects in POLI" ,  ylab="")

p4<-qplot(polo,totfin/1000000, data=polosx, fill=call, 
          geom="bar", stat="identity" ,  
          main=" total funding in POLI",  ylab="Meuro")


jpeg("basic_stat1.jpg",width = 600, height = 900, units = "px")
# pdf("polo_call1.pdf",paper=c("a4"))
# par(mfrow=c(3,1)) 
# setup multi plot with grid

vp.setup(4,1)
# plot graphics into layout
print(p1, vp=vp.layout(1,1))
print(p2, vp=vp.layout(2,1))
print(p3, vp=vp.layout(3,1))
print(p4, vp=vp.layout(4,1))

dev.off()

################################################################
p1<-ggplot(polosx, aes(call,npart)) + 
    geom_boxplot(aes(fill = call), show_guide = FALSE) + 
    ggtitle("n. participations per polo per call") 

p2<-ggplot(polosx, aes(call,numprj)) + 
    geom_boxplot(aes(fill = call), show_guide = FALSE) + 
    ggtitle("n.projects per polo per call") 

p3<-ggplot(polosx, aes(call,numsbj)) + 
    geom_boxplot(aes(fill = call), show_guide = FALSE) + 
    ggtitle("n.subjects per polo per call") 

p4<-ggplot(polosx, aes(call,totfin)) + 
    geom_boxplot(aes(fill = call), show_guide = FALSE) + 
    ggtitle("tot_funding per polo per call") 

jpeg("basic_stat01.jpg",width = 600, height = 900, units = "px")
# pdf("polo_call1.pdf",paper=c("a4"))
# par(mfrow=c(3,1)) 
# setup multi plot with grid

vp.setup(4,1)
# plot graphics into layout
print(p1, vp=vp.layout(1,1))
print(p2, vp=vp.layout(2,1))
print(p3, vp=vp.layout(3,1))
print(p4, vp=vp.layout(4,1))

dev.off()

################################################################
p1<-ggplot(polosx, aes(call,npart)) + 
    geom_boxplot(aes(fill = call), show_guide = FALSE) + 
    ggtitle("n. participations per polo per call") 

p2<-ggplot(polosx, aes(call,numprj)) + 
    geom_boxplot(aes(fill = call), show_guide = FALSE) + 
    ggtitle("n.projects per polo per call") 

p3<-ggplot(polosx, aes(call,numsbj)) + 
    geom_boxplot(aes(fill = call), show_guide = FALSE) + 
    ggtitle("n.subjects per polo per call") 

p4<-ggplot(polosx, aes(call,totfin)) + 
    geom_boxplot(aes(fill = call), show_guide = FALSE) + 
    ggtitle("tot_funding per polo per call") 

jpeg("basic_stat01.jpg",width = 600, height = 900, units = "px")
# pdf("polo_call1.pdf",paper=c("a4"))
# par(mfrow=c(3,1)) 
# setup multi plot with grid

vp.setup(4,1)
# plot graphics into layout
print(p1, vp=vp.layout(1,1))
print(p2, vp=vp.layout(2,1))
print(p3, vp=vp.layout(3,1))
print(p4, vp=vp.layout(4,1))

dev.off()

################################################################
p1<-ggplot(lstpoli, aes(call,log(invest))) + 
    geom_boxplot(aes(fill = call), show_guide = FALSE) + 
    ggtitle("investment distribution per call") 

p2<-ggplot(lstpoli, aes(call,log(fin))) + 
    geom_boxplot(aes(fill = call), show_guide = FALSE) + 
    ggtitle("funding distribution per call") 


jpeg("basic_stat02.jpg",width = 480, height = 600, units = "px")
# pdf("polo_call1.pdf",paper=c("a4"))
# par(mfrow=c(3,1)) 
# setup multi plot with grid

vp.setup(2,1)
# plot graphics into layout
print(p1, vp=vp.layout(1,1))
print(p2, vp=vp.layout(2,1))


dev.off()
######################################################


ggplot(polosx, aes(numprj,totfin/1000000)) + 
    geom_point(aes(color = polo), show_guide = FALSE,
               size = 10, alpha = 1/2) + 
    geom_smooth(method=lm,   se=T) +
    facet_grid(call ~ .) +
    ggtitle("tot.funding per tot.projects") +
    ylab("Meuro") +
    geom_text(aes(label=polo), size=3) 


ggsave(file="fundsbj.jpeg", dpi=72)
###############################################

p1 <-qplot(polo,npart, data=polosx, fill=call, 
           facets= call~., show_guide = FALSE,
           geom="bar", stat="identity",  
           main=" total participations POLI",  ylab="")

p2 <-qplot(polo, numprj, data=polosx, fill=call, 
           facets= call~., show_guide = FALSE,
           geom="bar",stat="identity" ,  
           main=" n. projects in POLI",  ylab="")

p3 <-qplot(polo, numsbj, data=polosx, fill=call, 
           facets= call~., show_guide = FALSE,
           geom="bar",stat="identity",  
           main=" n. subjects in POLI" ,  ylab="")

p4<-qplot(polo,totfin/1000000, data=polosx, fill=call, 
          facets= call~., show_guide = FALSE,
          geom="bar", stat="identity" ,  
          main=" total funding in POLI",  ylab="Meuro")


jpeg("basic_stat2.jpg",width = 600, height = 900, units = "px")
# pdf("polo_call1.pdf",paper=c("a4"))
# par(mfrow=c(3,1)) 
# setup multi plot with grid

vp.setup(4,1)
# plot graphics into layout
print(p1, vp=vp.layout(1,1))
print(p2, vp=vp.layout(2,1))
print(p3, vp=vp.layout(3,1))
print(p4, vp=vp.layout(4,1))

dev.off()

p1<-qplot(polo, meanfin/1000000, data=polosx, fill=call, 
          facets= call~., show_guide = FALSE,
          geom="bar", stat="identity",  
          main=" mean funding in POLI",  ylab="Meuro")

p2<-qplot(polo, fin/1000000, data=listpoli, fill=call, 
          facets= call~., show_guide = FALSE,
          geom="boxplot",  
          main=" funding distribution in POLI",  ylab="Meuro")

jpeg("basic_stat3.jpg",width = 600, height = 900, units = "px")
# pdf("polo_call1.pdf",paper=c("a4"))
# par(mfrow=c(3,1)) 
# setup multi plot with grid

vp.setup(2,1)
# plot graphics into layout
print(p1, vp=vp.layout(1,1))
print(p2, vp=vp.layout(2,1))

dev.off()



p1<-qplot(sbjtype, data=lstpoli, fill=call, 
      main=" participation per type ",  ylab="n. participations")

p2<-qplot(sbjtype, data=lstpoli, fill=call, 
      facets= call~., show_guide = FALSE,
      main=" participation per type ",  ylab="n. participations")

jpeg("basic_stat4.jpg",width = 600, height = 900, units = "px")
# setup multi plot with grid

vp.setup(3,1)
# plot graphics into layout
print(p1, vp=vp.layout(1,1))
print(p2, vp=vp.layout(2:3,1))

dev.off()



p1<-qplot(sbjtype, fin/1000000, data=lstpoli, fill=sbjtype, 
      geom="boxplot",  show_guide = FALSE,
      main=" funding distribution for type",  ylab="Meuro")

p2<-qplot(sbjtype, fin/1000000, data=lstpoli, fill=call, 
      geom="boxplot",  
      main=" funding distribution for type",  ylab="Meuro")

p3<-qplot(sbjtype, fin/1000000, data=lstpoli, fill=call, 
      facets= call~., show_guide = FALSE,
      geom="boxplot",  
      main=" funding distribution for type",  ylab="Meuro")

jpeg("basic_stat5.jpg",width = 600, height = 900, units = "px")
# pdf("polo_call1.pdf",paper=c("a4"))
# par(mfrow=c(3,1)) 
# setup multi plot with grid

vp.setup(4,1)
# plot graphics into layout
print(p1, vp=vp.layout(1,1))
print(p2, vp=vp.layout(2,1))
print(p3, vp=vp.layout(3:4,1))

dev.off()


#######################################################
sbjsx<-ddply(lstpoli, .(sbj,call), summarise,
              type= levels(factor(sbjtype)),
              npart= length(sbj),
              numprj= length(levels(factor(prj))),
              totinv= sum(invest),
              meaninv=mean(invest),
              totfin= sum(fin),
              meanfin=mean(fin)
)
str(sbjsx)
head(sbjsx)
sbjsx$sbj[sbjsx$numprj>1]


typesx<-ddply(lstpoli, .(sbjtype,call), summarise,
             type= levels(factor(sbjtype)),
             npart= length(sbj),
             numprj= length(levels(factor(prj))),
             numsbj= length(levels(factor(sbj))),
             totinv= sum(invest),
             meaninv=mean(invest),
             totfin= sum(fin),
             meanfin=mean(fin)
)
str(typesx)
head(typesx)
sum(typesx$npart)

p1 <-qplot(type,npart, data=typesx, fill=call, 
           geom="bar", stat="identity",  
           main=" n. participation per type",  ylab="")

p2 <-qplot(type, numprj, data=typesx, fill=call, 
           geom="bar",stat="identity" ,  
           main=" n. projects per type",  ylab="")

p3 <-qplot(type, numsbj, data=typesx, fill=call, 
           geom="bar",stat="identity",  
           main=" n. subjects per type" ,  ylab="")

p4<-qplot(type,totfin/1000000, data=typesx, fill=call, 
          geom="bar", stat="identity" ,  
          main=" total funding per type",  ylab="Meuro")


jpeg("basic_stat6.jpg",width = 600, height = 900, units = "px")
# pdf("polo_call1.pdf",paper=c("a4"))
# par(mfrow=c(3,1)) 
# setup multi plot with grid

vp.setup(4,1)
# plot graphics into layout
print(p1, vp=vp.layout(1,1))
print(p2, vp=vp.layout(2,1))
print(p3, vp=vp.layout(3,1))
print(p4, vp=vp.layout(4,1))

dev.off()


p1 <-qplot(type, numprj, data=typesx, fill=call, 
           facets= call~., show_guide = FALSE,
           geom="bar",stat="identity" ,  
           main=" n. projects in type",  ylab="")

p2 <-qplot(type, numsbj, data=typesx, fill=call, 
           facets= call~., show_guide = FALSE,
           geom="bar",stat="identity",  
           main=" n. subjects in type" ,  ylab="")

p3<-qplot(type,totfin/1000000, data=typesx, fill=call, 
          facets= call~., show_guide = FALSE,
          geom="bar", stat="identity" ,  
          main=" total funding in type",  ylab="Meuro")


jpeg("basic_stat7.jpg",width = 600, height = 900, units = "px")
# pdf("polo_call1.pdf",paper=c("a4"))
# par(mfrow=c(3,1)) 
# setup multi plot with grid

vp.setup(3,1)
# plot graphics into layout
print(p1, vp=vp.layout(1,1))
print(p2, vp=vp.layout(2,1))
print(p3, vp=vp.layout(3,1))


dev.off()

##############################################################


p1<-qplot(invest,fin, data=lstpoli, color=sbjtype,
      geom="point", size=2, show_guide = FALSE,
      main=" funding/investment ratio distribution ")

p2<-qplot(sbjtype, fin/invest,  data=lstpoli, fill=sbjtype, 
    geom="boxplot", 
      main=" funding /investment ratio distribution ")


jpeg("basic_stat8.jpg",width = 600, height = 900, units = "px")
# pdf("polo_call1.pdf",paper=c("a4"))
# par(mfrow=c(3,1)) 
# setup multi plot with grid
vp.setup(2,1)
# plot graphics into layout
print(p1, vp=vp.layout(1,1))
print(p2, vp=vp.layout(2,1))

dev.off()


###################################

lstpoli_no<-lstpoli[!lstpoli$sbjtype=="OTH",]

gg<-ggplot(lstpoli_no, aes(invest/1e+5,fin/1e+5)) + 
    geom_point(aes(color = sbjtype), show_guide = F,
               size = 3, alpha = 2/3) + 
#    facet_wrap( ~ sbjtype, ncol=2) +
#   geom_smooth(method=lm,   se=F) +
    ggtitle("funding on investment distribution ")+
    xlab("invest in M???") + ylab("funding in M???") 
  

gg + facet_wrap( ~ sbjtype, ncol=2) 

ggsave(file="fund_inv.jpeg", dpi=72)

####################################

lstpoli_h<-lstpoli[lstpoli$sbjtype=="UNI",]

ggplot(lstpoli_h, aes(invest/1e+5,fin/1e+5)) + 
    geom_point(aes(color = sbjtype), show_guide = F,
               size = 3, alpha = 2/3) + 
    #    facet_wrap( ~ sbjtype, ncol=2) +
       geom_smooth(method=lm,   se=T) +
    ggtitle("University funding on investment distribution ")+
    xlab("invest in M???") + ylab("funding in M???") 

ggsave(file="fund_inv_UNI.jpeg", dpi=72)
