library(crunch)
source(system.file("crunch-test.R", package = "crunch"))

with_mock_crunch({
    getEnvOf <- function(what, which=rev(sys.parents())) {
        for (frame in which)
            if (exists(what, frame=frame, inherits=FALSE))
                return(sys.frame(frame))
        return(NULL)
    }
    print("mocked")
    ds <- crunch::loadDataset("test ds")
    crunch:::.makeArrayGadget(env = getEnvOf("ds"))
})
