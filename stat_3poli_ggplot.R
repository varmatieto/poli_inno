#############

library (ggplot2)
library (plyr)
library(grid)


listpoli<-read.table("poli_123.txt",sep="", header=T, dec=",")

colnames(listpoli)<-c("prj",  "sbj",    "polo",	
                      "invest",	"fin",	"call")
head(listpoli)
str(listpoli)
levels(listpoli$call)<-c("1^","2^","3^")

attrsbj<-read.table("attr_sbj.txt",sep="", 
                    header=T, dec=",", stringsAsFactors=F)
head(attrsbj)
str(attrsbj)

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
              numsbj= length(levels(factor(polo))),
              totinv= sum(invest),
              meaninv=mean(invest),
              totfin= sum(fin),
              meanfin=mean(fin)
)
str(sbjsx)
head(sbjsx)

sbjsx<-ddply(lstpoli, .(sbj,call), summarise,
             type= levels(factor(sbjtype)),
             npart= length(sbj),
             numprj= length(levels(factor(prj))),
             numsbj= length(levels(factor(polo))),
             totinv= sum(invest),
             meaninv=mean(invest),
             totfin= sum(fin),
             meanfin=mean(fin)
)
str(sbjsx)
head(sbjsx)


p1 <-qplot(type,npart, data=sbjsx, fill=type, 
           geom="bar", stat="identity",  
           main=" total participations sbj",  ylab="")

p2 <-qplot(type, numprj, data=sbjsx, fill=type, 
           geom="bar",stat="identity" ,  
           main=" n. projects sbj",  ylab="")

p3 <-qplot(type, numsbj, data=sbjsx, fill=type, 
           geom="bar",stat="identity",  
           main=" n. subjects sbj" ,  ylab="")

p4<-qplot(type,totfin/1000000, data=sbjsx, fill=type, 
          geom="bar", stat="identity" ,  
          main=" total funding sbj",  ylab="Meuro")


jpeg("basic_stat6.jpg",width = 600, height = 900, units = "px")
# pdf("polo_call1.pdf",paper=c("a4"))
# par(mfrow=c(3,1)) 
# setup multi plot with grid

vp.setup(3,1)
# plot graphics into layout
print(p2, vp=vp.layout(1,1))
print(p3, vp=vp.layout(2,1))
print(p4, vp=vp.layout(3,1))

dev.off()


p1 <-qplot(type, numprj, data=sbjsx, fill=call, 
           facets= call~., show_guide = FALSE,
           geom="bar",stat="identity" ,  
           main=" n. projects in type",  ylab="")

p2 <-qplot(type, numsbj, data=sbjsx, fill=call, 
           facets= call~., show_guide = FALSE,
           geom="bar",stat="identity",  
           main=" n. subjects in type" ,  ylab="")

p3<-qplot(type,totfin/1000000, data=sbjsx, fill=call, 
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

p1<-qplot(type, meanfin/1000000, data=sbjsx, fill=call, 
          facets= call~., show_guide = FALSE,
          geom="bar", stat="identity",  
          main=" mean funding in sbj",  ylab="Meuro")

p2<-qplot(sbjtype, fin/1000000, data=listpoli, fill=call, 
          facets= call~., show_guide = FALSE,
          geom="boxplot",  
          main=" funding distribution in sbj",  ylab="Meuro")

jpeg("basic_stat8.jpg",width = 600, height = 900, units = "px")
# pdf("polo_call1.pdf",paper=c("a4"))
# par(mfrow=c(3,1)) 
# setup multi plot with grid

vp.setup(3,1)
# plot graphics into layout
print(p1, vp=vp.layout(1,1))
print(p2, vp=vp.layout(2:3,1))

dev.off()


str(sbjsx)
factor(sbjsx$npart)

qplot(type, log(numprj), data=sbjsx, geom="boxplot")
