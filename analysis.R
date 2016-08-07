library(dpylr)

## Separate the records with new window

training_yes <- filter(training, new_window == "yes")
training_no <- filter(training, new_window == "no")

## Remove columns with no information (empty and NAs)

colNames <- names(training_no)
drop = c("")

for (i in seq_along(colNames)) {
  if (training_no[1,colNames[i]] == "" || is.na(training_no[1,colNames[i]])) {
    drop <- append(drop, c(colNames[i]))
  }
}

drop <- drop[2:length(drop)]
training_no <- training_no[!(colNames %in% drop)]

## Splitting the available training data into train, test and val
