#############

library (ggplot2)
library (plyr)
library(grid)


listpoli<-read.table("poli_123.txt",sep="", header=T, dec=",")

colnames(listpoli)<-c("prj",  "sbj",    "polo",	
                      "invest",	"fin",	"call")
head(listpoli)
str(listpoli)

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



p1<-qplot(polo,totinv/1000000, data=polosx, fill=call, 
      geom="bar", stat="identity",  
      main=" total investment in POLI" ,  ylab="Meuro")

p2<-qplot(polo,totinv/1000000, data=polosx, fill=call, 
         facets= call~., show_guide = FALSE,
         geom="bar", stat="identity" ,  
      main=" total investment in POLI",  ylab="Meuro")

p3<-qplot(polo, meaninv/1000000, data=polosx, fill=call, 
      facets= call~., show_guide = FALSE,
      geom="bar", stat="identity",  
      main=" mean investment in POLI",  ylab="Meuro")


jpeg("basic_stat2.jpg",width = 600, height = 900, units = "px")
# pdf("polo_call1.pdf",paper=c("a4"))
# par(mfrow=c(3,1)) 
# setup multi plot with grid

vp.setup(3,1)
# plot graphics into layout
print(p1, vp=vp.layout(1,1))
print(p2, vp=vp.layout(2,1))
print(p3, vp=vp.layout(3,1))

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

