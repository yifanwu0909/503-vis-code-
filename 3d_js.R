setwd("C:/Users/yifan/Documents/ANLY 503/project2/")
library(ggplot2)
library(sqldf)
library(forcats)
library(stringr)


# 3d

d3 = read.csv('merged.csv')
library(threejs)
library(htmlwidgets)
js = scatterplot3js(as.numeric(d3$unemployed), 
                    as.numeric(d3$less_than_highschool), 
                    as.numeric(d3$below_poverty), 
                    color = as.factor(d3$color), 
                    axisLabels=c("Unemployed Rate","Below Poverty Rate", "Less Than High School"))
saveWidget(js, file="3d-scatter.html", selfcontained = TRUE, libdir = NULL, background = "white")
