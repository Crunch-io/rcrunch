context("Variable grouping and order setting")

test_that("VariableGroup and Order objects can be made", {
    expect_true(inherits(VariableGroup(group="group1", entities=""),
        "VariableGroup"))
    expect_true(inherits(VariableGroup(name="group1", entities=""),
        "VariableGroup"))
    vg1 <- VariableGroup(name="group1", entities="")
    expect_true(inherits(VariableOrder(vg1), "VariableOrder"))
    expect_true(inherits(VariableOrder(list(name="group1",
        entities=""), vg1), "VariableOrder"))
})

with(fake.HTTP, {
    session_store$datasets <- DatasetCatalog(GET("api/datasets.json"))
    test.ds <- loadDataset("test ds")
    
    test_that("ordering methods on variables catalog", {
        expect_true(inherits(ordering(variables(test.ds)), "VariableOrder"))
        expect_true(inherits(ordering(test.ds), "VariableOrder"))
        expect_identical(ordering(variables(test.ds)), ordering(test.ds))
    })
})


if (!run.only.local.tests) {
    with(test.authentication, {
        with(test.dataset(df), {
            ds <- .setup
            test_that("Can get VariableOrder from dataset", {
                expect_identical(ordering(ds), 
                    VariableOrder(VariableGroup(name="ungrouped", 
                    entities=urls(active(ds@variables)))))
            })
            test_that("Can make VariableGroup/Order from Variables", {
                expect_true(setequal(entities(ordering(ds)[[1]]), 
                    entities(VariableGroup(name="ungrouped", entities=ds))))
            })
        })
        
        with(test.dataset(df), {
            ds <- .setup
            test_that("Can construct VariableOrder from variables", {
                vg <- VariableOrder(
                    VariableGroup(name="Group 1", 
                        variables=ds[c("v1", "v3", "v5")]),
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
            })
            vg <- VariableOrder(
                VariableGroup(name="Group 1", 
                    entities=ds[c("v1", "v3", "v5")]),
                VariableGroup(name="Group 2.5", variables=ds["v4"]),
                VariableGroup(name="Group 2", 
                    entities=ds[c("v6", "v2")]))
                    
            test_that("Can manipulate VariableOrder", {
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
            
            original.order <- ordering(ds)
            test_that("Can set VariableOrders", {
                expect_false(identical(vg, original.order))
                ordering(ds) <- vg
                expect_identical(ordering(ds)[-4], vg)
                    ## Prune #4 because it is the "ungrouped", which is empty
                expect_identical(ordering(refresh(ds))[-4], vg)

                ds <- refresh(ds)
                expect_false(identical(ordering(variables(ds)), original.order))
                ordering(variables(ds)) <- original.order
                expect_identical(ordering(variables(ds)), original.order)
                expect_identical(ordering(variables(refresh(ds))),
                    original.order)
            })
            
            test_that("Can manipulate VariableOrder that's part of a dataset", {
                ordering(ds) <- vg
                expect_identical(names(ordering(ds)), 
                    c("Group 1", "Group 2.5", "Group 2"))
                names(ordering(ds))[3] <- "Three"
                expect_identical(names(ordering(ds)), 
                    c("Group 1", "Group 2.5", "Three"))
                expect_identical(names(ordering(ds)), 
                    c("Group 1", "Group 2.5", "Three"))
            })
        })
    })
}