library(crunch)
options(crunch.api=getOption("test.api"), 
        crunch.debug=FALSE,
        crunch.email=getOption("test.user"),
        crunch.pw=getOption("test.pw"))
login()

#load("../vignettes/getting-started.RData")
load("../vignettes/economist.RData")

track.local <- ds$track
summary.track.local <- capture.output(print(track.local))
track.cats <- track.cats.before <- categories(track.local)
names(track.cats)[1:2] <- c("Right track", "Wrong track")
values(track.cats)[1:3] <- c(1, -1, 0)
is.na(track.cats) <- "Not sure"

categories(ds$track)[2:3] <- track.cats[c(3,2)]

names(variables(ds))[aliases(variables(ds)) == "track"] <- "Direction of country"

head2 <- head.of.variables <- head(names(variables(ds)), 10)
head2[6:9] <- c("Favorability of Edward Snowden", 
                               "Approval of Snowden's Leak",
                               "Support for Prosecution of Snowden",
                               "Penalty for Snowden")

save(ds, track.local, summary.track.local, track.cats, track.cats.before, head.of.variables, head2,
    file="../vignettes/variables.RData")
