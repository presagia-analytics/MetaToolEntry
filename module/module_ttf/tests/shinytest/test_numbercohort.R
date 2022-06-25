app <- ShinyDriver$new("../../")
app$snapshotInit("test_numbercohort")

app$setInputs(`ttf1-n_arms` = 8)
app$setInputs(`ttf1-n_arms` = 7)
app$setInputs(`ttf1-n_arms` = 6)
app$setInputs(`ttf1-n_arms` = 5)
app$setInputs(`ttf1-n_arms` = 2)
app$snapshot()
