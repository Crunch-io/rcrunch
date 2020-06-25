library(crunch)
options(crunch.api=getOption("test.api"),
        crunch.debug=FALSE,
        crunch.email=getOption("test.user"),
        crunch.pw=getOption("test.pw"))
login()

## 1. Getting started
load("../vignettes/economist.RData")
ds <- newDataset(economist, name="Economist/YouGov Weekly Survey")
rm(economist)
dim.ds <- dim(ds)
.listDatasets <- "Economist/YouGov Weekly Survey"

message("3. Variables")
track.var <- ds$track
summary.track.var <- capture.output(print(track.var))
track.cats <- track.cats.before <- categories(track.var)
names(track.cats)[1:2] <- c("Right track", "Wrong track")
values(track.cats)[1:3] <- c(1, -1, 0)
is.na(track.cats) <- "Not sure"
categories(ds$track)[2:3] <- track.cats[c(3,2)]
names(variables(ds))[aliases(variables(ds)) == "track"] <- "Direction of country"
head2 <- head.of.variables <- head(names(variables(ds)), 10)
head2[4:7] <- names(variables(ds))[4:7] <- c("Favorability of Edward Snowden",
                                             "Approval of Snowden's Leak",
                                             "Support for Prosecution of Snowden",
                                             "Penalty for Snowden")

message("4. Make array")
start_make_array <- ds

show_imiss_b <- capture.output(print(ds$imiss_b))
ds$imiss <- makeArray(ds[grep("^imiss_", names(ds))], name="Issue importance")
show_imiss_subvars <- crunch:::showSubvariables(subvariables(ds$imiss))
show_imiss <- capture.output(print(ds$imiss))
names_imiss_subvars <- names(subvariables(ds$imiss))

newnames <- c("The economy", "Immigration",
              "The environment", "Terrorism", "Gay rights", "Education",
              "Health care", "Social security", "The budget deficit",
              "The war in Afghanistan", "Taxes", "Medicare", "Abortion")
names(subvariables(ds$imiss)) <- newnames
show_imiss_subvars2 <- crunch:::showSubvariables(subvariables(ds$imiss))
sorting <- order(names(subvariables(ds$imiss)))
subvariables(ds$imiss) <- subvariables(ds$imiss)[sorting]
show_imiss_subvars3 <- crunch:::showSubvariables(subvariables(ds$imiss))
show_boap_4 <- capture.output(print(ds$boap_4))
ds$boap <- makeMR(ds[grep("^boap_[0-9]+", names(ds))],
                  name="Approval of Obama on issues",
                  selections=c("Strongly approve", "Somewhat approve"))
show_boap_subvars <- crunch:::showSubvariables(subvariables(ds$boap))
show_boap <- c(crunch:::showCrunchVariableTitle(ds$boap),
               show_boap_subvars)
ds$boap <- undichotomize(ds$boap)
show_boap2 <- capture.output(print(ds$boap))
ds$boap <- dichotomize(ds$boap, "Strongly approve")
show_boap3 <- capture.output(print(ds$boap))

message("5. Variable order")
step0 <- ordering(ds)
step0.print <- capture.output(print(step0))
ordering(ds) <- VariableOrder(
    VariableGroup("Demos", ds[c(1, 21:37)]),
    VariableGroup("Tracking questions", ds[c(2,3, 11:20)]),
    VariableGroup("This week", ds[4:11])
)
step1 <- ordering(ds)
step1.print <- capture.output(print(step1))
names(ordering(ds))[1] <- "Demographics"
step2 <- ordering(ds)
ordering(ds) <- ordering(ds)[c(2, 3, 1)]
step3 <- ordering(ds)
ordering(ds)[["This week"]][["Snowden"]] <- ordering(ds)[["This week"]][1:4]
step4 <- ordering(ds)
step4.print <- capture.output(print(step4))

message("6. Derive")
ds$age <- 2015 - ds$birthyr
age.summary.before <- summary(ds$age)
by.summary.before <- summary(ds$birthyr)
ds$birthyr[ds$birthyr < 1945] <- 1945
age.summary.after <- summary(ds$age)
by.summary.after <- summary(ds$birthyr)
cr.expr <- 2015 - ds$birthyr
cr.log.expr <- ds$birthyr < 1945

message("7. Analyze")
tab1 <- crtabs(~ educ, data=ds)
tab2 <- crtabs(~ educ + gender, data=ds)

### Weighting
weight(ds) <- ds$weight
tab2weighted <- crtabs(~ educ + gender, data=ds)

### Complex data types
tab3 <- crtabs(~ imiss + gender, data=ds)
ds$imiss <- dichotomize(ds$imiss, c("Very Important", "Somewhat Important"))
tab3mr <- crtabs(~ imiss + gender, data=ds)
tab3subvar <- crtabs(~ imiss$imiss_f + gender, data=ds)
tab4 <- crtabs(~ imiss + educ + gender, data=ds)

### Numeric aggregations
tab5 <- crtabs(mean(age) ~ educ + gender, data=ds)
tab6 <- crtabs(min(age) ~ educ + gender, data=ds)
tab6a <- crtabs(min(age) ~ 1, data=ds)
track.cats <- categories(ds$track)
tab7 <- crtabs(mean(track) ~ educ + gender, data=ds)
tab8 <- crtabs(mean(track) ~ educ + gender, data=ds[ds$pid3 == "Democrat",])
snowdenleakapp.var <- capture.output(print(ds$snowdenleakapp))
ols1 <- lm(I(snowdenleakapp == "Strongly approve") ~ newsint2 + pid3 + gender + age, data=ds)
logit1 <- glm(I(snowdenleakapp == "Strongly approve") ~ newsint2 + pid3 + gender + age, family=binomial(link="logit"), data=ds)

message("8. Filters")
dems <- ds[ds$pid3 == "Democrat",]
dems2 <- subset(ds, ds$pid3 == "Democrat")
printdems <- capture.output(print(dems))
educ.dem.table <- table(dems$educ)
empty.filter.catalog <- filters(ds)
filters(ds)[["Young males"]] <- ds$gender == "Male" & ds$age < 30
print.young.males1 <- capture.output(print(filters(ds)[["Young males"]]))
filter.catalog.2 <- filters(ds)
is.public(filters(ds)[["Young males"]]) <- TRUE
filter.catalog.3 <- filters(ds)
filters(ds)[["Young males"]] <- ds$gender == "Male" & ds$age < 35
print.young.males2 <- capture.output(print(filters(ds)[["Young males"]]))

dim.ds.filters <- dim(ds)
exclusion(ds) <- ds$perc_skipped > 15
high_perc_skipped <- capture.output(print(exclusion(ds)))
dim.ds.excluded <- dim(ds)

message("subtotals")
sub_initial_subtotals <- subtotals(ds$manningknowledge)
subtotals(ds$manningknowledge) <- list(
    Subtotal(name = "Follows closely",
             categories = c("Somewhat closely", "Very closely"),
             after = "Somewhat closely"),
    Subtotal(name = "Doesn't Follow Closely",
             categories = c("Not very closely", "Not at all"),
             after = "Not at all"))
sub_subtotals1 <- subtotals(ds$manningknowledge)
subtotals(ds$obamaapp) <- list(
    Heading(name = "Approves",
            after = 0),
    Heading(name = "Disapprove",
            after = "Somewhat Approve"),
    Heading(name = "No Answer",
            after = "Strongly Disapprove"))
sub_headings <- subtotals(ds$obamaapp)
subtotals(ds$obamaapp) <- NULL
approve_subtotals <- list(
    Subtotal(name = "Approves",
            categories = c("Somewhat approve", "Strongly approve"),
            after = "Somewhat approve"),
    Subtotal(name = "Disapprove",
            categories = c("Somewhat disapprove", "Strongly disapprove"),
            after = "Strongly disapprove"))
subtotals(ds$snowdenleakapp) <- approve_subtotals
subtotals(ds$congapp) <- approve_subtotals
sub_snowdon <- subtotals(ds$snowdenleakapp)
sub_con <- subtotals(ds$congapp)
sub_crtab <- crtabs(~congapp + gender, ds)

save.image(file="../vignettes/vignettes.RData", version = 2)
with_consent(delete(ds)) ## cleanup
