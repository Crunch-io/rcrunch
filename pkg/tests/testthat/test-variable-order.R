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
    test.ds <- loadDataset("test ds")
    
    test_that("ordering methods on variables catalog", {
        expect_true(inherits(ordering(variables(test.ds)), "VariableOrder"))
        expect_true(inherits(ordering(test.ds), "VariableOrder"))
        expect_identical(ordering(variables(test.ds)), ordering(test.ds))
    })
    
    test.ord <- ordering(test.ds)
    ents <- entities(test.ord)
    test_that("can create nested groups", {
        ord <- try(VariableOrder(
            VariableGroup(name="Group 1", entities=list(ents[1], 
                VariableGroup(name="Nested", entities=ents[2:3]),
                ents[4])),
            VariableGroup(name="Group 2", entities=ents[5])))
        expect_true(inherits(ord, "VariableOrder"))
        expect_identical(entities(ord), ents)
        
        varcat <- allVariables(test.ds)
        varcat@views$hierarchical_order <- sub("hierarchical",
            "nested-hierarchical", varcat@views$hierarchical_order)
        expect_identical(ord@value,
            VariableOrder(GET(varcat@views$hierarchical_order))@value)
            
        vglist <- fromJSON(toJSON(ord))
        expect_identical(vglist, list(groups=list(
            list(
                group="Group 1",
                entities=list(
                    ents[1],
                    list(
                        group="Nested",
                        entities=ents[2:3]
                    ),
                    ents[4]
                    )
                ),
            list(
                group="Group 2",
                entities=ents[5]
            )
        )))
    })
    
    test_that("can assign nested groups in entities", {
        ng <- list(ents[1], 
                    VariableGroup(name="Nested", entities=ents[2:3]),
                    ents[4])
        try(entities(test.ord[[1]]) <- ng)
        expect_identical(entities(test.ord[[1]], unlist=FALSE), 
            list(ents[1], ents[2:3], ents[4]))
        expect_identical(entities(test.ord[[1]]), ents[1:4])
    })
})


if (run.integration.tests) {
    with(test.authentication, {
        with(test.dataset(df), {
            test_that("Can get VariableOrder from dataset", {
                expect_identical(ordering(ds)@value, 
                    VariableOrder(VariableGroup(name="ungrouped", 
                    entities=urls(variables(ds))))@value)
            })
            test_that("Can make VariableGroup/Order from Variables", {
                expect_true(setequal(entities(ordering(ds)[[1]]), 
                    entities(VariableGroup(name="ungrouped", entities=ds))))
            })
        })
        
        with(test.dataset(df), {
            test_that("Can construct VariableOrder from variables", {
                vg <- VariableOrder(
                    VariableGroup(name="Group 1", 
                        variables=ds[c("v1", "v3", "v5")]),
                    VariableGroup(name="Group 2.5", entities=ds["v4"]),
                    VariableGroup(name="Group 2", 
                        entities=ds[c("v6", "v2")]))
                vglist <- fromJSON(toJSON(vg))
                expect_identical(vglist, list(groups=list(
                    list(group="Group 1", 
                        entities=c(self(ds$v1), self(ds$v3), self(ds$v5))),
                    list(group="Group 2.5", entities=self(ds$v4)),
                    list(group="Group 2", entities=c(self(ds$v6), self(ds$v2)))
                )))
            })
            starting.vg <- vg <- VariableOrder(
                VariableGroup(name="Group 1", 
                    entities=ds[c("v1", "v3", "v5")]),
                VariableGroup(name="Group 2.5", variables=ds["v4"]),
                VariableGroup(name="Group 2", 
                    entities=ds[c("v6", "v2")]))
                    
            test_that("Get entities from VariableOrder and Group", {
                expect_identical(entities(vg[[1]]),
                    c(self(ds$v1), self(ds$v3), self(ds$v5)))
                expect_identical(entities(vg), 
                    c(self(ds$v1), self(ds$v3), self(ds$v5), self(ds$v4),
                    self(ds$v6), self(ds$v2)))
            })
            
            try(entities(vg[[2]]) <- self(ds$v2))
            test_that("Set URLs -> entities on VariableGroup", {
                expect_identical(entities(vg[[2]]), self(ds$v2))
                expect_identical(entities(vg), 
                    c(self(ds$v1), self(ds$v3), self(ds$v5), self(ds$v2),
                    self(ds$v6)))
            })
            try(entities(vg[[2]]) <- list(ds$v3))
            test_that("Set variables -> entities on VariableGroup", {
                expect_identical(entities(vg[[2]]), self(ds$v3))
            })
            
            try(name(vg[[2]]) <- "Group 3")
            test_that("Set name on VariableGroup", {
                expect_identical(names(vg), c("Group 1", "Group 3", "Group 2"))
            })
            try(names(vg) <- c("G3", "G1", "G2"))
            test_that("Set names on VariableOrder", {
                expect_identical(names(vg), c("G3", "G1", "G2"))
            })
            
            try(vglist <- fromJSON(toJSON(vg)))
            test_that("VariableOrder to/fromJSON", {
                expect_identical(vglist, list(groups=list(
                    list(group="G3", 
                        entities=c(self(ds$v1), self(ds$v3), self(ds$v5))),
                    list(group="G1", entities=self(ds$v3)),
                    list(group="G2", entities=c(self(ds$v6), self(ds$v2)))
                )))
                
                vg[1:2] <- vg[c(2,1)]
                expect_identical(fromJSON(toJSON(vg)), list(groups=list(
                    list(group="G1", entities=self(ds$v3)),
                    list(group="G3", 
                        entities=c(self(ds$v1), self(ds$v3), self(ds$v5))),
                    list(group="G2", entities=c(self(ds$v6), self(ds$v2)))
                )))
            })
            
            original.order <- ordering(ds)
            test_that("Can set VariableOrder on dataset", {
                expect_false(identical(starting.vg, original.order))
                ordering(ds) <- starting.vg
                expect_identical(grouped(ordering(ds))@value, starting.vg@value)
                expect_identical(grouped(ordering(refresh(ds)))@value, starting.vg@value)
                expect_true(inherits(ungrouped(ordering(ds)), "VariableGroup"))
                expect_true(inherits(ungrouped(ordering(refresh(ds))),
                    "VariableGroup"))

                ds <- refresh(ds)
                expect_false(identical(ordering(variables(ds))@value,
                    original.order@value))
                ordering(variables(ds)) <- original.order
                expect_identical(ordering(variables(ds))@value,
                    original.order@value)
                expect_identical(ordering(variables(refresh(ds)))@value,
                    original.order@value)
            })
            
            test_that("Can manipulate VariableOrder that's part of a dataset", {
                ordering(ds) <- starting.vg
                expect_identical(names(ordering(ds)), 
                    c("Group 1", "Group 2.5", "Group 2", "ungrouped"))
                names(ordering(ds))[3] <- "Three"
                expect_identical(names(ordering(ds)), 
                    c("Group 1", "Group 2.5", "Three", "ungrouped"))
                expect_identical(names(grouped(ordering(ds))), 
                    c("Group 1", "Group 2.5", "Three"))
            })
            
            test_that("ordering<- validation", {
                bad.vg <- starting.vg
                entities(bad.vg[[1]]) <- c(entities(bad.vg[[1]])[-2],
                    "/not/a/variable")
                expect_error(ordering(ds) <- bad.vg, 
                    "Variable URL referenced in Order not present in catalog: /not/a/variable")
            })
        })
    })
}