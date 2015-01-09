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
    varcat <- allVariables(test.ds)
    
    test_that("ordering methods on variables catalog", {
        expect_true(inherits(ordering(variables(test.ds)), "VariableOrder"))
        expect_true(inherits(ordering(test.ds), "VariableOrder"))
        expect_identical(ordering(variables(test.ds)), ordering(test.ds))
    })
    
    test_that("relative URLs in hierarchical order", {
        vc <- varcat
        vc@views$hierarchical_order <- sub("hierarchical",
            "relative-hierarchical", vc@views$hierarchical_order)
        expect_identical(vc@order@value,
            VariableOrder(crGET(vc@views$hierarchical_order))@value)
    })
    
    test.ord <- ordering(test.ds)
    ent.urls <- urls(test.ord)
    nested.ord <- try(VariableOrder(
        VariableGroup(name="Group 1", entities=list(ent.urls[1], 
            VariableGroup(name="Nested", entities=ent.urls[2:3]),
            ent.urls[4])),
        VariableGroup(name="Group 2", entities=ent.urls[5])))
    
    test_that("Can extract group(s) by name", {
        expect_identical(nested.ord[["Group 2"]], 
            VariableGroup(name="Group 2", entities=ent.urls[5]))
        expect_identical(nested.ord$`Group 2`, 
            VariableGroup(name="Group 2", entities=ent.urls[5]))
        expect_identical(nested.ord["Group 2"], 
            VariableOrder(VariableGroup(name="Group 2", entities=ent.urls[5])))
    })
    test_that("Can create nested groups", {
        expect_true(inherits(nested.ord, "VariableOrder"))
        expect_identical(urls(nested.ord), ent.urls)
    })
    test_that("Can read nested groups from the API", {
        vc <- varcat
        vc@views$hierarchical_order <- sub("hierarchical",
            "nested-hierarchical", vc@views$hierarchical_order)
        expect_identical(nested.ord@value,
            VariableOrder(crGET(vc@views$hierarchical_order))@value)
    })
    test_that("Nested groups can also have relative urls", {
        vc <- varcat
        vc@views$hierarchical_order <- sub("hierarchical",
            "relative-and-nested-hierarchical", vc@views$hierarchical_order)
        expect_identical(nested.ord@value,
            VariableOrder(crGET(vc@views$hierarchical_order))@value)
    })
    test_that("Nested groups can serialize and deserialize", {
        vglist <- fromJSON(toJSON(nested.ord), simplifyVector=FALSE)
        expect_identical(vglist, list(groups=list(
            list(
                group="Group 1",
                entities=list(
                    ent.urls[1],
                    list(
                        group="Nested",
                        entities=as.list(ent.urls[2:3])
                    ),
                    ent.urls[4]
                    )
                ),
            list(
                group="Group 2",
                entities=as.list(ent.urls[5])
            )
        )))
    })
    
    ng <- list(ent.urls[1], 
                VariableGroup(name="Nested", entities=ent.urls[2:3]),
                ent.urls[4])
    test_that("can assign nested groups in entities", {
        to <- test.ord
        try(entities(to[[1]]) <- ng)
        expect_identical(entities(to[[1]]), ng)
        expect_identical(urls(to[[1]]), ent.urls[1:4])
        expect_identical(to[[1]][[2]], 
            VariableGroup(name="Nested", entities=ent.urls[2:3]))
        expect_identical(entities(to[[1]][[2]]), as.list(ent.urls[2:3]))
    })
    test_that("can assign group into order", {
        to <- test.ord
        try(to[[1]] <- VariableGroup(name="[[<-", entities=ng))
        expect_identical(entities(to[[1]]), ng)
        expect_identical(name(to[[1]]), "[[<-")
        expect_identical(urls(to[[1]]), ent.urls[1:4])
        expect_identical(to[[1]][[2]], 
            VariableGroup(name="Nested", entities=ent.urls[2:3]))
    })
    test_that("can assign NULL into order to remove a group", {
        no <- no2 <- no3 <- nested.ord
        no[[2]] <- NULL
        expect_identical(no, VariableOrder(
            VariableGroup(name="Group 1", entities=list(ent.urls[1], 
                VariableGroup(name="Nested", entities=ent.urls[2:3]),
                ent.urls[4]))))
        no2[["Group 2"]] <- NULL
        expect_identical(no2, VariableOrder(
            VariableGroup(name="Group 1", entities=list(ent.urls[1], 
                VariableGroup(name="Nested", entities=ent.urls[2:3]),
                ent.urls[4]))))
        no3$`Group 2` <- NULL
        expect_identical(no3, VariableOrder(
            VariableGroup(name="Group 1", entities=list(ent.urls[1], 
                VariableGroup(name="Nested", entities=ent.urls[2:3]),
                ent.urls[4]))))
    })
    test_that("can assign group into group", {
        to <- test.ord
        try(to[[1]] <- VariableGroup(name="[[<-", entities=ng))
        expect_identical(to[[1]][[1]], ent.urls[1])
        try(to[[1]][[1]] <- VariableGroup(name="Nest2",
            entities=to[[1]][[1]]))
        expect_identical(entities(to[[1]]), 
            list(VariableGroup(name="Nest2", entities=ent.urls[1]), 
                VariableGroup(name="Nested", entities=ent.urls[2:3]),
                ent.urls[4]))
        expect_identical(urls(to[[1]]), ent.urls[1:4]) 
    })
    test_that("can assign into a nested group", {
        to <- test.ord
        try(to[[1]] <- VariableGroup(name="[[<-", entities=ng))
        try(entities(to[[1]][[2]]) <- rev(entities(to[[1]][[2]])))
        expect_identical(entities(to[[1]]), 
            list(ent.urls[1], 
                VariableGroup(name="Nested", entities=ent.urls[3:2]),
                ent.urls[4]))
        expect_identical(urls(to[[1]]), ent.urls[c(1,3,2,4)])
        expect_identical(name(to[[1]]), "[[<-")
        try(name(to[[1]]) <- "Something better")
        expect_identical(name(to[[1]]), "Something better")
    })
    
    test_that("Validation on Order/Group [[<-", {
        to <- test.ord
        expect_true(inherits(to, "VariableOrder"))
        skip(expect_error(to[[1]] <- ent.urls, 
            "Cannot insert multiple variables. Perhaps you want to insert a nested group? Or maybe manipulate the entire entities vector?"),
            "Not sure what this expectation should be")
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
                vglist <- cereal(vg)
                expect_identical(vglist, list(groups=list(
                    list(group="Group 1", 
                        entities=list(self(ds$v1), self(ds$v3), self(ds$v5))),
                    list(group="Group 2.5", entities=list(self(ds$v4))),
                    list(group="Group 2", entities=list(self(ds$v6), self(ds$v2)))
                )))
            })
            starting.vg <- vg <- VariableOrder(
                VariableGroup(name="Group 1", 
                    entities=ds[c("v1", "v3", "v5")]),
                VariableGroup(name="Group 2.5", variables=ds["v4"]),
                VariableGroup(name="Group 2", 
                    entities=ds[c("v6", "v2")]))
                    
            test_that("Get urls from VariableOrder and Group", {
                expect_identical(urls(vg[[1]]),
                    c(self(ds$v1), self(ds$v3), self(ds$v5)))
                expect_identical(urls(vg), 
                    c(self(ds$v1), self(ds$v3), self(ds$v5), self(ds$v4),
                    self(ds$v6), self(ds$v2)))
            })
            
            try(entities(vg[[2]]) <- self(ds$v2))
            test_that("Set URLs -> entities on VariableGroup", {
                expect_identical(urls(vg[[2]]), self(ds$v2))
                expect_identical(urls(vg), 
                    c(self(ds$v1), self(ds$v3), self(ds$v5), self(ds$v2),
                    self(ds$v6)))
            })
            try(entities(vg[[2]]) <- list(ds$v3))
            test_that("Set variables -> entities on VariableGroup", {
                expect_identical(urls(vg[[2]]), self(ds$v3))
            })
            
            try(name(vg[[2]]) <- "Group 3")
            test_that("Set name on VariableGroup", {
                expect_identical(names(vg), c("Group 1", "Group 3", "Group 2"))
            })
            try(names(vg) <- c("G3", "G1", "G2"))
            test_that("Set names on VariableOrder", {
                expect_identical(names(vg), c("G3", "G1", "G2"))
            })
            
            try(vglist <- cereal(vg))
            test_that("VariableOrder to/fromJSON", {
                expect_identical(vglist, list(groups=list(
                    list(group="G3", 
                        entities=list(self(ds$v1), self(ds$v3), self(ds$v5))),
                    list(group="G1", entities=list(self(ds$v3))),
                    list(group="G2", entities=list(self(ds$v6), self(ds$v2)))
                )))
                
                vg[1:2] <- vg[c(2,1)]
                expect_identical(cereal(vg), list(groups=list(
                    list(group="G1", entities=list(self(ds$v3))),
                    list(group="G3", 
                        entities=list(self(ds$v1), self(ds$v3), self(ds$v5))),
                    list(group="G2", entities=list(self(ds$v6), self(ds$v2)))
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