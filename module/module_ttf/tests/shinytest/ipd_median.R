app <- ShinyDriver$new("../../")
app$snapshotInit("ipd_median")

app$setInputs(`ttf1-save_table` = "click")
app$setInputs(`ttf1-add_ipd` = "click")
app$snapshot()
