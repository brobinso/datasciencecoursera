# Reproducible Research Recipes
# Knitr options
```r results="asis", "hide"
echo true, false
fig.height:numeric
fig.width:numeric
```
current time is `r time`
# global options (overridable)
```{r setoptions,echo=FALSE}
opts_chunk$set(echo=F,results="hide")
```
# store results of a lengthy computation in a chunk
cache=TRUE

