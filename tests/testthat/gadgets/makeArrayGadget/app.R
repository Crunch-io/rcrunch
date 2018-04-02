library(crunch)

source("../../../../inst/crunch-test.R")

with_mock_crunch({
    getEnvOf <- function(what, which=rev(sys.parents())) {
        for (frame in which)
            if (exists(what, frame=frame, inherits=FALSE))
                return(sys.frame(frame))
        return(NULL)
    }
    ds <- loadDataset("test ds")
    crunch:::.makeArrayGadget(env = getEnvOf("ds"))
})
