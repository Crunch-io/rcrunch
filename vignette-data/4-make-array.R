library(crunch)
options(crunch.api=getOption("test.api"), 
        crunch.debug=FALSE,
        crunch.email=getOption("test.user"),
        crunch.pw=getOption("test.pw"))
login()

load("../vignettes/variables.RData")
start <- ds

show_imiss_b <- capture.output(print(ds$imiss_b))
ds$imiss <- makeArray(pattern="^imiss_", dataset=ds, name="Issue importance")
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

ds$boap <- makeMR(pattern="^boap_[0-9]+", dataset=ds, name="Approval of Obama on issues", selections=c("Strongly approve", "Somewhat approve"))
show_boap_subvars <- crunch:::showSubvariables(subvariables(ds$boap))
show_boap <- c(crunch:::showCrunchVariableTitle(ds$boap),
    show_boap_subvars)

ds$boap <- undichotomize(ds$boap)
show_boap2 <- capture.output(print(ds$boap))

ds$boap <- dichotomize(ds$boap, "Strongly approve")
show_boap3 <- capture.output(print(ds$boap))

save.image(file="../vignettes/array-variables.RData")

