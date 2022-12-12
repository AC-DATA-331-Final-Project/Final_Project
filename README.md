# Final_Project
## Graph 1
* Attempting to show average size for each state by month. Wanted to show growth of moths throughout the year in each state. 
* We were extremely limited by the following failures and our limited coding experience. 
* We also wanted to create this graph in a line graph, but we discuss the limitations in our failure section below.
### Failures
* Cleaning the sex column - dealing with NA and different names in the column.
* Cleaning and creating the dates column.
* Couldn’t figure out how to create line graphs for each state - partially because not every state had data for each month.
* There was to many points in the scatterplot for wing length and wing width by state. 
* States names were scattered within our graph. This made it hard to know which state was with which point.
<img src="images/Rplot02.png" alt="Girl in a jacket" width="1200" height="800">


## Graph 2
* We wanted to show a graph that would show the relationship between wing length and wing width by state.
* The goal was to display the states with the largest moth wing sizes.
* We were able to overlay a linear regression to display the relationship in the scatterplot.
* A secondary goal of this graph was to see if there were clear regions that had larger moths than others. However, the graph was inconclusive in that regard as it was to hard to decipher which region was ideal for moth size. 
### Failures
* One failure or drawback that we had with this graph was scaling it so that it doesn't become too cluttered
<img src="images/Rplot10.png" alt="Girl in a jacket" width="1200" height="750">


## Graph 3
* We wanted to show the similarities between the male and female regression lines without the outlier point in the Female graph - Alberta.
<img src="images/Rplot11.png" alt="Girl in a jacket" width="1200" height="750">


## Graph 4
* We wanted to show the states with the largest moths in square units in more clear fashion
<img src="images/Rplot09.png" alt="Girl in a jacket" width="1200" height="800">


## Graph 5
* We wanted to display moth wing size in square units over the months of the year in a line graph.
* The goal or purpose of this graph was to see if the size of moths increase over the year.
### Failures
* We were unable to determine the conclusiveness of this hypothesis because the data was not consistently from the same place during each month, so, the data may have been more biased in one direction or the other during certain months. 
<img src="images/Rplot.png" alt="Girl in a jacket" width="1600" height="800">


## Graph 6 and ttest
* Goal was to determine if male or females were larger, however it was difficult to tell by the graph, so we created a ttest to determine if the two datasets - male and female - were statistically similar.
<img src="images/Rplot04.png" alt="Girl in a jacket" width="1200" height="800">

* We concluded that the datasets were not statistically similar and that the males were larger on average than the females.
<img src="images/ttest_image.png" alt="Girl in a jacket" width="1200" height="500">
