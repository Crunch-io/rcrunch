library(crunch)
options(crunch.api=getOption("test.api"), 
        crunch.debug=FALSE,
        crunch.email=getOption("test.user"),
        crunch.pw=getOption("test.pw"))
login()

load("../vignettes/getting-started.RData")

track.var <- ds$track
summary.track.var <- capture.output(print(track.var))
track.cats <- categories(track.var)
names(track.cats)[1:2] <- c("Right track", "Wrong track")
values(track.cats) <- c(1, -1, 0)
is.na(track.cats) <- "Not sure"

categories(ds$track) <- track.cats[c(1,3,2)]

names(variables(ds))[aliases(variables(ds)) == "track"] <- "Direction of country"

head2 <- head.of.variables <- head(names(variables(ds)), 10)
head2[6:9] <- c("Favorability of Edward Snowden", 
                               "Approval of Snowden's Leak",
                               "Support for Prosecution of Snowden",
                               "Penalty for Snowden")
                               
save(ds, track.var, summary.track.var, track.cats, head.of.variables, head2,
    file="../vignettes/variables.RData")
                               