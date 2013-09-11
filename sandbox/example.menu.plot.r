# example of menu plot

testmenu<-function(x=NULL,method=c('none','character','numeric')){
  if(!is.null(x)) method <- class(x)[1]
  
  method<-method[1]
  
  if(method=='none') 
    
    method<-menu(c('character','numeric'),title = 
      "\nMake a plot selection (or 0 to exit):\n")
  
  
  switch(method,
         invisible(return(x)),
         character = {
           print('you chose character')
         },
         numeric = {
           print('you chose numeric')
         }
  )    
  
  
}

#
# barplot 
#

x <- c(44,56,34,35,44,51,55)
nams <- LETTERS[1:7]
bp <- barplot(x,horiz=T,col="light blue",xlim=c(0,60))
text(30,-1.25,xpd=NA,"Networks",cex=1.5) # x title
text(-6,4,xpd=NA,"Research Areas",cex=1.5,srt=90) # y title
text(x,bp,x,pos=4) # place numbers to right of bars
text(0,bp,nams,cex=1.2,pos=4) # label bars right on the bars themselves 
