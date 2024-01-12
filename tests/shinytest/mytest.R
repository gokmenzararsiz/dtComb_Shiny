app <- ShinyDriver$new("../../")
app$snapshotInit("mytest")
data <- "ddimer.txt"
app$snapshot()
app$setInputs(`generalTabSetPanel` = "Data Upload")
# app$waitForShiny()

# app$uploadFile(upload = data)
app$snapshot()
app$setInputs(`generalTabSetPanel` = "Analysis")
app$snapshot()

app$setInputs(`goButton` = "click")
Sys.sleep(5)
app$snapshot()

app$setInputs(`tabGroupPlot` = "Results")
app$snapshot()
