install.packages(c("dygraphs","plotly","xts")) 

library(dygraphs)
library(plotly)#inter-activity to any kind of plot
library(xts)#data time series
library(tidyverse)


##sec 2 dygraphs(time series visualization)

##Example 1 simple time series
Nile_ts <-as.xts(Nile)
dygraph(Nile_ts,main = "Nile River Flow")

##
"INDIA-AQI-DATA-2015-2020.CSV" %>% 
  read_csv() %>% 
  select(c(Date,NO)) %>% 
  dygraph(main = "NO timeseries") %>% 
  dyRangeSelector()


#example 2 add range selector
Nile_ts <-as.xts(Nile)
dygraph(Nile_ts,main = "Nile River Flow") %>% 
  dyRangeSelector()


#example 3
data <- cbind(mdeaths,fdeaths)
data_xts <- as.xts(data)
dygraph(data_xts,main = "Deaths in UK") %>% 
  dySeries("mdeaths",label = "male") %>% 
  dySeries("fdeaths",label = "female") %>% 
  dyOptions(colors = c("blue","red")) %>% 
  dyRangeSelector()


##EXAMPLE 4 customizing graphs

lungdeaths <- cbind(mdeaths,fdeaths)
dygraph(as.xts(lungdeaths),main = "Compairing deaths of male and female") %>% 
  dySeries("mdeaths",color = "darkblue") %>% 
  dySeries("fdeaths",color = "tomato") %>%
  dyHighlight(highlightCircleSize = 5,highlightSeriesBackgroundAlpha = 0.2) %>% 
  dyOptions(drawPoints = TRUE,pointSize = 2) %>% 
  dyLegend(show = "always")

lungdeaths <- cbind(mdeaths,fdeaths)
dygraph(as.xts(lungdeaths),main = "Compairing deaths of male and female") %>% 
  dySeries("mdeaths",color = "darkblue") %>% 
  dySeries("fdeaths",color = "tomato") %>%
  dyHighlight(highlightCircleSize = 5,highlightSeriesBackgroundAlpha = 0.2) %>% 
  dyOptions(drawPoints = TRUE,pointSize = 2) %>% 
  dyLegend(show = "auto") %>% #"auto","always","follow","never"
  dyRangeSelector()
------------------------------------
  ##section2 plotly(interactive layer on ggplot)
  
  #Ex 1 :concert ggplot to plotly
  ggplot(mtcars, aes(x=wt , y=mpg , color= factor(cyl)))+
  geom_point(size=3)+
  labs(title = "Miles per Gallon vs Weight",
       x="Weight", y="MPG") -> plot_p
ggplotly(plot_p)
  
#Ex 2:interactive bar chat
plot_ly(data = mpg,x = ~class,type = "histogram")

##Ex 3 :line chart with hover info
plot_ly(data = economics, x = ~date, y = ~unemploy, type = 'scatter',
        mode = 'lines',
        line = list(color = 'purple')) %>%
  layout(title = "US Unemployment Over Time")

  ##Ex 4:Adding tooltips  and customization
plot_ly(mtcars, x = ~mpg,y= ~hp, type = 'scatter',mode = 'markers', 
        text = ~paste("car: ",rownames(mtcars),
                      "<br>MPG:",mpg,
                      "<br>HP: ",hp),
        marker = list(size = 10)) %>% 
  layout(title = "car horsepower vs MPG")
  
  


