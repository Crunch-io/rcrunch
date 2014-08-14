library(rcrunch)
options(crunch.api=getOption("test.api"), 
        crunch.debug=FALSE,
        crunch.email=getOption("test.user"),
        crunch.pw=getOption("test.pw"))
login()

load("../pkg/vignettes/economist.RData")
start <- ds <- newDataset(df, name="Economist/YouGov Weekly Survey")

show_imiss_b <- rcrunch:::showCrunchVariable(ds$imiss_b)
summary_imiss_b <- summary(ds$imiss_b)

ds$imiss <- makeArray(pattern="^imiss_", dataset=ds, name="Issue importance")
show_imiss_subvars <- rcrunch:::showSubvariables(subvariables(ds$imiss))
show_imiss <- c(rcrunch:::showCrunchVariable(ds$imiss),
    show_imiss_subvars)
names_imiss_subvars <- names(subvariables(ds$imiss))

newnames <- c("The economy", "Immigration", 
    "The environment", "Terrorism", "Gay rights", "Education", 
    "Health care", "Social security", "The budget deficit", 
    "The war in Afghanistan", "Taxes", "Medicare", "Abortion")
names(subvariables(ds$imiss)) <- newnames
show_imiss_subvars2 <- rcrunch:::showSubvariables(subvariables(ds$imiss))

sorting <- order(names(subvariables(ds$imiss)))
subvariables(ds$imiss) <- subvariables(ds$imiss)[sorting]
show_imiss_subvars3 <- rcrunch:::showSubvariables(subvariables(ds$imiss))

show_boap_4 <- rcrunch:::showCrunchVariable(ds$boap_4)
summary_boap_4 <- summary(ds$boap_4)

ds$boap <- makeMR(pattern="^boap_[0-9]+", dataset=ds, name="Approval of Obama on issues", selections=c("Strongly approve", "Somewhat approve"))
show_boap_subvars <- rcrunch:::showSubvariables(subvariables(ds$boap))
show_boap <- c(rcrunch:::showCrunchVariable(ds$boap),
    show_boap_subvars)

ds$boap <- undichotomize(ds$boap)
show_boap2 <- c(rcrunch:::showCrunchVariable(ds$boap),
    show_boap_subvars)

ds$boap <- dichotomize(ds$boap, "Strongly approve")
show_boap3 <- c(rcrunch:::showCrunchVariable(ds$boap),
    show_boap_subvars)

rm(df)
save.image(file="../pkg/vignettes/array-variables.RData")

