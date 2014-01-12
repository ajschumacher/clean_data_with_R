
# Read in one of the data sets to start with
ell <- read.csv("ell.csv", as.is=TRUE, skip=6, check.names=FALSE,
                na.strings="s")


# Ensure the columns are what we're expecting
stopifnot(names(ell)==c("DBN", "Grade", "Year", "Category", "Number Tested",
                        "Mean Scale Score", "#", "%", "#", "%", "#", "%",
                        "#", "%", "#", "%"))
# The first time, that list can be generated with:
# cat(paste('"', paste(names(ell), collapse='", "'), '"', sep=""))


# You might notice this problem with:
# table(ell$Category)
# And you might fix it like this:
ell$Category[ell$Category=="ELLs"] <- "ELL"
# And you could then assert that it is fixed
stopifnot(all(ell$Category %in% c("ELL", "EP")))


# We want to understand patterns of missingness; here's one way
# sapply(ell, function(v){return(sum(is.na(v)))})
# table(rowSums(as.data.frame(lapply(ell[, 7:16], is.na))))
# ell[rowSums(as.data.frame(lapply(ell[, 7:16], is.na))) %in% 2:3, ]
# Then we can fix the issue; this is a very ad hoc way:
ell[c(45615, 45627), 15] <- 64
ell[c(45615, 45627), 16] <- 100
# Now we can assert that missingness occurs only as follows:
stopifnot(all(rowSums(as.data.frame(lapply(ell[, 7:16], is.na))) %in% c(0, 10)))


# Check that the percentages are consistent with the counts
# This caused some trouble during the presentation because
# of a floating point issue that I hadn't noticed. Sorry!
# You can check this:
# 3/16 - 0.188 == -0.0005
# I wish I had noticed this before - it's another great point!
stopifnot(all((round(abs(100*ell[[15]]/ell[[5]] - ell[[16]]), 3) <= 0.05) %in% c(TRUE, NA)))
stopifnot(all((round(abs(100*ell[[13]]/ell[[5]] - ell[[14]]), 3) <= 0.05) %in% c(TRUE, NA)))
stopifnot(all((round(abs(100*ell[[11]]/ell[[5]] - ell[[12]]), 3) <= 0.05) %in% c(TRUE, NA)))
stopifnot(all((round(abs(100*ell[[9]]/ell[[5]] - ell[[10]]), 3) <= 0.05) %in% c(TRUE, NA)))
stopifnot(all((round(abs(100*ell[[7]]/ell[[5]] - ell[[8]]), 3) <= 0.05) %in% c(TRUE, NA)))
# These are pretty ugly and unclear; good place to clean up some day - but the good news
# is that there aren't any obvious problems with the data in this area.

# Since the percentages are okay, drop them, retaining the counts
ell <- ell[, -seq(8, 16, by=2)]


# Check that the "3+4" column is the sum of the 3 column and the 4 column
stopifnot(all((ell[[9]] + ell[[10]] - ell[[11]]) %in% c(0, NA)))

# And drop the redundant column
ell <- ell[, -11]


# Check that scores are suppressed when there are 5 or fewer students
stopifnot(all(is.na(ell[["Mean Scale Score"]][ell[["Number Tested"]] <= 5])))


# Check that the total number tested appear in the four count columns
# where they aren't suppressed
stopifnot(all(ell[["Number Tested"]][5 < ell[["Number Tested"]]] ==
                rowSums(ell[, 7:10])[5 < ell[["Number Tested"]]]))


# Give the columns more useful names
names(ell) <- c("dbn", "grade", "year", "category", "n", "mean", "one", "two", "three", "four")


# Check that we have a unique ID with the combination of the first four columns
stopifnot(all(!duplicated(ell[, 1:4])))


# A brief digression on the failure of the suppression method used...
# e <- aggregate(ell$one, list(ell$dbn, ell$year, ell$category),
#                function(v){return(c(sum(is.na(v)), length(v)))})
# e <- subset(e, x[, 1] == 1 & x[, 2] != 1)
# head(e)
# subset(ell, dbn=="01M839" & year==2006 & category=="ELL")
# subset(ell, dbn=="22K203" & year==2011 & category=="ELL")
# rm(e)
# This demonstrates that some suppressed values are not really suppressed.


# More checks could be done here, but for now just eliminate the All Grades
# rows, which would make sense for analysis focusing on individual grades.
ell <- subset(ell, grade != "All Grades")
# Now the grade column can be made into a numeric type (without any warnings).
ell$grade <- as.integer(ell$grade)


# At this point we've done pretty much good work with the ELL data set,
# but there are many more, and we could go back and re-factor or otherwise
# apply what we've done to the other data sets. One issue is that we'll
# find different problems with different data sets, even from the same source.
# There aren't unique IDs in the gender data set, for example:
gender <- read.csv("gender.csv", as.is=TRUE, skip=6, check.names=FALSE,
                   na.strings="s")
head(gender[duplicated(gender[, 1:4]) |
            rev(duplicated(gender[nrow(gender):1, 1:4])), ])
# Probably next we would try to figure out what should be done with these
# strange extra male rows. Pull requests accepted! :)
