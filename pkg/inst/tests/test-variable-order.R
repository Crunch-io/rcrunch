context("Variable grouping and order setting")

test_that("VariableGroup and Grouping objects can be made", {
    expect_true(inherits(VariableGroup(group="group1", entities=""),
        "VariableGroup"))
    expect_true(inherits(VariableGroup(name="group1", entities=""),
        "VariableGroup"))
    vg1 <- VariableGroup(name="group1", entities="")
    expect_true(inherits(VariableGrouping(vg1), "VariableGrouping"))
    expect_true(inherits(VariableGrouping(list(name="group1",
        entities=""), vg1), "VariableGrouping"))
})


if (!run.only.local.tests) {
    with(test.authentication, {
        with(test.dataset(df), {
            ds <- .setup
            # print(getVariableOrder(ds))
            # print(VariableGrouping(list(list(name="ungrouped", 
            # entities=vapply(ds, function (x) self(x), character(1), USE.NAMES=FALSE)))))
        skip({
        test_that("Can get VariableGrouping from dataset", {
                expect_identical(getVariableOrder(ds), 
                    VariableGrouping(list(list(name="ungrouped", 
                    entities=vapply(ds, function (x) self(x), character(1), USE.NAMES=FALSE)))))
            })
            test_that("Can make VariableGroup(ing) from Variables", {
                expect_identical(getVariableOrder(ds)[[1]], 
                    VariableGroup(name="ungrouped", entities=ds[]))
                expect_identical(getVariableOrder(ds)[[1]], 
                    VariableGroup(name="ungrouped", entities=ds))
            })
        })
        }, "Server is duplicating v1")
        
        with(test.dataset(df), {
            ds <- .setup
            test_that("Can construct VariableGroupings from variables", {
                vg <- VariableGrouping(
                    VariableGroup(name="Group 1", 
                        entities=ds[c("v1", "v3", "v5")]),
                    VariableGroup(name="Group 2.5", entities=ds["v4"]),
                    VariableGroup(name="Group 2", 
                        entities=ds[c("v6", "v2")]))
                vglist <- fromJSON(toJSON(vg))
                expect_identical(vglist, list(
                    list(group="Group 1", 
                        entities=c(self(ds$v1), self(ds$v3), self(ds$v5))),
                    list(group="Group 2.5", entities=self(ds$v4)),
                    list(group="Group 2", entities=c(self(ds$v6), self(ds$v2)))
                ))
                
                expect_identical(entities(vg[[1]]),
                    c(self(ds$v1), self(ds$v3), self(ds$v5)))
                expect_identical(entities(vg), 
                    c(self(ds$v1), self(ds$v3), self(ds$v5), self(ds$v4),
                    self(ds$v6), self(ds$v2)))
                entities(vg[[2]]) <- self(ds$v2)
                expect_identical(entities(vg[[2]]), self(ds$v2))
                expect_identical(entities(vg), 
                    c(self(ds$v1), self(ds$v3), self(ds$v5), self(ds$v2),
                    self(ds$v6)))
                entities(vg[[2]]) <- list(ds$v3)
                expect_identical(entities(vg[[2]]), self(ds$v3))
                name(vg[[2]]) <- "Group 3"
                expect_identical(names(vg), c("Group 1", "Group 3", "Group 2"))
                names(vg) <- c("G3", "G1", "G2")
                expect_identical(names(vg), c("G3", "G1", "G2"))
                vglist <- fromJSON(toJSON(vg))
                expect_identical(vglist, list(
                    list(group="G3", 
                        entities=c(self(ds$v1), self(ds$v3), self(ds$v5))),
                    list(group="G1", entities=self(ds$v3)),
                    list(group="G2", entities=c(self(ds$v6), self(ds$v2)))
                ))
            })
        })
    })
}