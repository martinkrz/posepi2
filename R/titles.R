# This is messy but I don't know how else to do it.
output$title1a = renderPrint({
  index = p1()[[2]][[1]]
  title = p1()[[3]][[1]]
  cat(titlefmt(index,title))
})
output$plot1 = renderPlot({
  p1()[[1]][[1]]
})
output$caption1 = renderPrint({ 
  cat(paste("<p>",p1()[[4]][[1]],"</p>",sep=""))
})
output$title2a <- renderPrint({
  index = p2()[[2]][[1]]
  title = p2()[[3]][[1]]
  cat(titlefmt(index,title))
})
output$title2b <- renderPrint({
  index = p2()[[2]][[2]]
  title = p2()[[3]][[2]]
  cat(titlefmt(index,title))
})
output$title2c <- renderPrint({
  index = p2()[[2]][[3]]
  title = p2()[[3]][[3]]
  cat(titlefmt(index,title))
})
output$title2d <- renderPrint({
  index = p2()[[2]][[4]]
  title = p2()[[3]][[4]]
  cat(titlefmt(index,title))
})
output$title2e <- renderPrint({
  index = p2()[[2]][[5]]
  title = p2()[[3]][[5]]
  cat(titlefmt(index,title))
})
output$title2f <- renderPrint({
  index = p2()[[2]][[6]]
  title = p2()[[3]][[6]]
  cat(titlefmt(index,title))
})
output$plot2a <- renderPlot({
  p2()[[1]][[1]]
})
output$plot2b <- renderPlot({
  p2()[[1]][[2]]
})
output$plot2c <- renderPlot({
  p2()[[1]][[3]]
})
output$plot2d <- renderPlot({
  p2()[[1]][[4]]
})
output$plot2e <- renderPlot({
  p2()[[1]][[5]]
})
output$plot2f <- renderPlot({
  p2()[[1]][[6]]
})
output$caption2a = renderPrint({ 
  cat(paste("<p>",p2()[[4]][[1]],"</p>",sep=""))
})
output$caption2b = renderPrint({ 
  cat(paste("<p>",p2()[[4]][[2]],"</p>",sep=""))
})
output$caption2c = renderPrint({ 
  cat(paste("<p>",p2()[[4]][[3]],"</p>",sep=""))
})
output$caption2d = renderPrint({ 
  cat(paste("<p>",p2()[[4]][[4]],"</p>",sep=""))
})
output$caption2e = renderPrint({ 
  cat(paste("<p>",p2()[[4]][[5]],"</p>",sep=""))
})
output$caption2f = renderPrint({ 
  cat(paste("<p>",p2()[[4]][[6]],"</p>",sep=""))
})
output$title3a <- renderPrint({
  index = p3()[[2]][[1]]
  title = p3()[[3]][[1]]
  cat(titlefmt(index,title))
})
output$title3b <- renderPrint({
  index = p3()[[2]][[2]]
  title = p3()[[3]][[2]]
  cat(titlefmt(index,title))
})
output$title3c <- renderPrint({
  index = p3()[[2]][[3]]
  title = p3()[[3]][[3]]
  cat(titlefmt(index,title))
})
output$title3d <- renderPrint({
  index = p3()[[2]][[4]]
  title = p3()[[3]][[4]]
  cat(titlefmt(index,title))
})
output$plot3a <- renderPlot({
  p3()[[1]][[1]]
})
output$plot3b <- renderPlot({
  p3()[[1]][[2]]
})
output$plot3c <- renderPlot({
  p3()[[1]][[3]]
})
output$plot3d <- renderPlot({
  p3()[[1]][[4]]
})
output$caption3a = renderPrint({ 
  cat(paste("<p>",p3()[[4]][[1]],"</p>",sep=""))
})
output$caption3b = renderPrint({ 
  cat(paste("<p>",p3()[[4]][[2]],"</p>",sep=""))
})
output$caption3c = renderPrint({ 
  cat(paste("<p>",p3()[[4]][[3]],"</p>",sep=""))
})
output$caption3d = renderPrint({ 
  cat(paste("<p>",p3()[[4]][[4]],"</p>",sep=""))
})