context("Variable grouping and order setting")

test_that("VariableGroup and Order objects can be made", {
    expect_is(VariableGroup(group = "group1", entities = ""), "VariableGroup")
    expect_is(VariableGroup(name = "group1", entities = ""), "VariableGroup")
    vg1 <- VariableGroup(name = "group1", entities = "")
    expect_is(VariableOrder(vg1), "VariableOrder")
    expect_is(
        VariableOrder(list(name = "group1", entities = ""), vg1),
        "VariableOrder"
    )
})

with_mock_crunch({
    ds <- loadDataset("test ds")
    varcat <- allVariables(ds)

    test_that("ordering methods on variables catalog", {
        expect_is(ordering(variables(ds)), "VariableOrder")
        expect_is(ordering(ds), "VariableOrder")
        expect_identical(ordering(variables(ds)), ordering(ds))
    })

    test.ord <- ordering(ds)
    ent.urls <- urls(test.ord)
    varcat_url <- self(allVariables(ds))
    nested.ord <- VariableOrder(
        VariableGroup(
            name = "Group 1",
            entities = list(
                ent.urls[1],
                VariableGroup(name = "Nested", entities = ent.urls[2:4]),
                ent.urls[5]
            )
        ),
        VariableGroup(name = "Group 2", entities = ent.urls[6:7]),
        catalog_url = varcat_url
    )

    test_that("urls() on Order/Group", {
        expect_identical(urls(nested.ord), ent.urls)
        expect_identical(urls(nested.ord[["Group 1"]]), ent.urls[1:5])
        expect_identical(urls(nested.ord[["Group 1"]][["Nested"]]), ent.urls[2:4])
    })

    test_that("Validation on entities<-", {
        expect_error(
            entities(ordering(ds)) <- NULL,
            "NULL is an invalid input for entities"
        )
        expect_error(
            entities(nested.ord[[1]]) <- new.env(),
            "environment is an invalid input for entities"
        )
    })

    test_that("Warning that you should be using folders instead", {
        options(crunch.already.shown.folders.msg = NULL)
        expect_warning(
            expect_PUT(ordering(ds) <- nested.ord[2:1]),
            "Hey!"
        )
        ## Second time it doesn't warn. One nag per session
        expect_warning(
            expect_PUT(ordering(ds) <- nested.ord[2:1]),
            NA
        )
    })

    test_that("length methods", {
        expect_length(nested.ord, 2)
        expect_length(nested.ord[[1]], 3)
        expect_length(nested.ord[[2]], 2)
    })

    test_that("Can extract group(s) by name", {
        expect_identical(
            nested.ord[["Group 2"]],
            VariableGroup(name = "Group 2", entities = ent.urls[6:7])
        )
        expect_identical(
            nested.ord$`Group 2`,
            VariableGroup(name = "Group 2", entities = ent.urls[6:7])
        )
    })

    test_that("Extract with [", {
        expect_identical(
            nested.ord["Group 2"],
            VariableOrder(
                VariableGroup(name = "Group 2", entities = ent.urls[6:7]),
                catalog_url = varcat_url
            )
        )
        expect_error(
            nested.ord["NOT A GROUP"],
            "Undefined groups selected: NOT A GROUP"
        )
    })

    test_that("Extract with [[ from Group", {
        expect_identical(
            nested.ord[["Group 1"]]$Nested,
            VariableGroup(name = "Nested", entities = ent.urls[2:4])
        )
        expect_error(
            nested.ord[["Group 1"]][["NOT A GROUP"]],
            "Undefined groups selected: NOT A GROUP"
        )
    })

    test_that("Extract with [ from Group", {
        expect_identical(
            nested.ord[["Group 1"]]["Nested"],
            VariableGroup(
                name = "Group 1",
                entities = list(
                    VariableGroup(name = "Nested", entities = ent.urls[2:4])
                )
            )
        )
        expect_error(
            nested.ord[["Group 1"]]["NOT A GROUP"],
            "Undefined groups selected: NOT A GROUP"
        )
    })

    test_that("Extract with path vector", {
        expect_identical(
            nested.ord[[c("Group 1", "Nested")]],
            VariableGroup(name = "Nested", entities = ent.urls[2:4])
        )
    })
    test_that("Extract with path string", {
        expect_identical(
            nested.ord[["Group 1/Nested"]],
            VariableGroup(name = "Nested", entities = ent.urls[2:4])
        )
    })
    test_that("Extract with alternative path string", {
        with(temp.option(crunch.delimiter = "|"), {
            expect_identical(
                nested.ord[["Group 1|Nested"]],
                VariableGroup(name = "Nested", entities = ent.urls[2:4])
            )
        })
    })

    test_that("Can create nested groups", {
        expect_is(nested.ord, "VariableOrder")
        expect_identical(urls(nested.ord), ent.urls)
    })
    test_that("Nested groups can serialize and deserialize", {
        vglist <- cereal(nested.ord)
        expect_identical(vglist, list(graph = list(
            list(`Group 1` = list(
                ent.urls[1],
                list(`Nested` = as.list(ent.urls[2:4])),
                ent.urls[5]
            )),
            list(`Group 2` = as.list(ent.urls[6:7]))
        )))
    })

    ng <- list(
        ent.urls[1],
        VariableGroup(name = "Nested", entities = ent.urls[2:4]),
        ent.urls[5]
    )
    test_that("can assign nested groups in entities", {
        to <- test.ord
        try(entities(to) <- ng)
        expect_identical(entities(to), entities(ng))
        expect_identical(urls(to), ent.urls[1:5])
        expect_identical(
            to[[2]],
            VariableGroup(name = "Nested", entities = ent.urls[2:4])
        )
        expect_identical(entities(to[[2]]), as.list(ent.urls[2:4]))
    })
    test_that("can assign group into order", {
        to <- test.ord
        try(to[[1]] <- VariableGroup(name = "[[<-", entities = ng))
        expect_identical(entities(to[[1]]), ng)
        expect_identical(name(to[[1]]), "[[<-")
        expect_identical(urls(to[[1]]), ent.urls[1:5])
        expect_identical(
            to[[1]][[2]],
            VariableGroup(name = "Nested", entities = ent.urls[2:4])
        )
    })
    test_that("can assign NULL into order to remove a group", {
        no <- no2 <- no3 <- nested.ord
        no[[2]] <- NULL
        expect_identical(no, VariableOrder(
            VariableGroup(name = "Group 1", entities = list(
                ent.urls[1],
                VariableGroup(name = "Nested", entities = ent.urls[2:4]),
                ent.urls[5]
            )),
            catalog_url = varcat_url
        ))
        no2[["Group 2"]] <- NULL
        expect_identical(no2, VariableOrder(
            VariableGroup(name = "Group 1", entities = list(
                ent.urls[1],
                VariableGroup(name = "Nested", entities = ent.urls[2:4]),
                ent.urls[5]
            )),
            catalog_url = varcat_url
        ))
        no3$`Group 2` <- NULL
        expect_identical(no3, VariableOrder(
            VariableGroup(name = "Group 1", entities = list(
                ent.urls[1],
                VariableGroup(name = "Nested", entities = ent.urls[2:4]),
                ent.urls[5]
            )),
            catalog_url = varcat_url
        ))
    })
    test_that("Can assign NULL into a group to remove", {
        no <- nested.ord
        expect_identical(
            no,
            VariableOrder(
                VariableGroup(
                    name = "Group 1",
                    entities = list(
                        ent.urls[1],
                        VariableGroup(name = "Nested", entities = ent.urls[2:4]),
                        ent.urls[5]
                    ),
                ),
                VariableGroup(name = "Group 2", entities = ent.urls[6:7]),
                catalog_url = varcat_url
            )
        )
        no[[1]][[3]] <- NULL
        expect_identical(
            no,
            VariableOrder(
                VariableGroup(
                    name = "Group 1",
                    entities = list(
                        ent.urls[1],
                        VariableGroup(name = "Nested", entities = ent.urls[2:4])
                    ),
                ),
                VariableGroup(name = "Group 2", entities = ent.urls[6:7]),
                catalog_url = varcat_url
            )
        )
        no[[1]][["Nested"]][[2]] <- NULL
        expect_identical(
            no,
            VariableOrder(
                VariableGroup(
                    name = "Group 1",
                    entities = list(
                        ent.urls[1],
                        VariableGroup(name = "Nested", entities = ent.urls[c(2, 4)])
                    ),
                ),
                VariableGroup(name = "Group 2", entities = ent.urls[6:7]),
                catalog_url = varcat_url
            )
        )
        no[[1]]$Nested <- NULL
        expect_identical(
            no,
            VariableOrder(
                VariableGroup(
                    name = "Group 1",
                    entities = list(ent.urls[1]),
                ),
                VariableGroup(name = "Group 2", entities = ent.urls[6:7]),
                catalog_url = varcat_url
            )
        )
        expect_error(
            nested.ord[[2]][[-1]] <- NULL,
            "Illegal subscript"
        )
        expect_error(
            nested.ord[[2]][[c(1, 2)]] <- NULL,
            "Illegal subscript"
        )
    })

    test_that("can assign group into group by index", {
        to <- test.ord
        try(to[[1]] <- VariableGroup(name = "[[<-", entities = ng))
        expect_identical(to[[1]][[1]], ent.urls[1])
        try(to[[1]][[1]] <- VariableGroup(
            name = "Nest2",
            entities = to[[1]][[1]]
        ))
        expect_identical(
            entities(to[[1]]),
            list(
                VariableGroup(name = "Nest2", entities = ent.urls[1]),
                VariableGroup(name = "Nested", entities = ent.urls[2:4]),
                ent.urls[5]
            )
        )
        expect_identical(urls(to[[1]]), ent.urls[1:5])
    })
    test_that("can assign into a nested group", {
        to <- test.ord
        try(to[[1]] <- VariableGroup(name = "[[<-", entities = ng))
        try(entities(to[[1]][[2]]) <- rev(entities(to[[1]][[2]])))
        expect_identical(
            entities(to[[1]]),
            list(
                ent.urls[1],
                VariableGroup(name = "Nested", entities = ent.urls[c(4, 3, 2)]),
                ent.urls[5]
            )
        )
        expect_identical(urls(to[[1]]), ent.urls[c(1, 4, 3, 2, 5)])
        expect_identical(name(to[[1]]), "[[<-")
        try(name(to[[1]]) <- "Something better")
        expect_identical(name(to[[1]]), "Something better")
    })

    test_that("Assignment by new group name", {
        nested.o <- nested.ord
        nested.o[["Group 3"]] <- ds["starttime"]
        expect_identical(names(nested.o), c("Group 1", "Group 2", "Group 3"))
        expect_identical(
            entities(nested.o[["Group 3"]]),
            list(self(ds$starttime))
        )
        ## Test the "duplicates option": starttime should have been removed from
        ## Group 2
        expect_identical(
            entities(nested.o[["Group 2"]]),
            list(self(ds$catarray))
        )
    })

    test_that("Assignment by new group name with a URL", {
        nested.o <- nested.ord
        nested.o[["Group 3"]] <- self(ds$starttime)
        expect_identical(names(nested.o), c("Group 1", "Group 2", "Group 3"))
        expect_identical(
            entities(nested.o[["Group 3"]]),
            list(self(ds$starttime))
        )
        ## Test the "duplicates option": starttime should have been removed from
        ## Group 2
        expect_identical(
            entities(nested.o[["Group 2"]]),
            list(self(ds$catarray))
        )
    })

    test_that("Update group with Dataset", {
        nested.o <- nested.ord
        nested.o[["Group 2"]] <- ds[c("gender", "starttime")]
        expect_identical(
            entities(nested.o[["Group 2"]]),
            lapply(ds[c("gender", "starttime")], self)
        )
    })

    test_that("Assignment by new nested group name", {
        nested.o <- nested.ord
        nested.o[["Group 1"]][[2]][["More nesting"]] <- self(ds$gender)
        expect_identical(
            entities(nested.o[["Group 1"]]$Nested[["More nesting"]]),
            list(self(ds$gender))
        )
        ## Test duplicates option: gender should only be in "More nesting"
        expect_identical(
            nested.o[["Group 1"]]$Nested[[1]],
            self(ds$location),
            self(ds$mymrset)
        )
    })

    ds3 <- loadDataset("ECON.sav")
    test_that("Show method for VO handles relative URLs correctly", {
        expect_prints(
            ordering(ds3),
            "Gender\nBirth Year\nstarttime"
        )
    })

    test_that("VariableOrder/Group show methods", {
        expect_prints(nested.ord,
            paste("[+] Group 1",
                "    Birth Year",
                "    [+] Nested",
                "        Gender",
                "        Categorical Location",
                "        mymrset",
                "    Text variable ftw",
                "[+] Group 2",
                "    starttime",
                "    Cat Array",
                sep = "\n"
            ),
            fixed = TRUE
        )
        no <- nested.ord
        no[[3]] <- VariableGroup("Group 3", entities = list())
        expect_prints(no,
            paste("[+] Group 1",
                "    Birth Year",
                "    [+] Nested",
                "        Gender",
                "        Categorical Location",
                "        mymrset",
                "    Text variable ftw",
                "[+] Group 2",
                "    starttime",
                "    Cat Array",
                "[+] Group 3",
                "    (Empty group)",
                sep = "\n"
            ),
            fixed = TRUE
        )
    })
    test_that(paste0(
        "Printing a single group doesn't fail (though it probably should do ",
        "better than show URLs)"
    ), {
        expect_prints(nested.ord[[2]],
            paste("[+] Group 2",
                "    https://app.crunch.io/api/datasets/1/variables/starttime/",
                "    https://app.crunch.io/api/datasets/1/variables/catarray/",
                sep = "\n"
            ),
            fixed = TRUE
        )
    })

    ord <- flattenOrder(test.ord)
    test_that("Composing a VariableOrder step by step: setup (flattenOrder)", {
        expect_prints(ord,
            paste("Birth Year",
                "Gender",
                "Categorical Location",
                "mymrset",
                "Text variable ftw",
                "starttime",
                "Cat Array",
                sep = "\n"
            ),
            fixed = TRUE
        )
    })
    test_that("Composing a VariableOrder step by step: group 1 by dataset", {
        ord$Demos <<- ds[c("gender", "birthyr")]
        expect_prints(ord,
            paste("Categorical Location",
                "mymrset",
                "Text variable ftw",
                "starttime",
                "Cat Array",
                "[+] Demos",
                "    Gender",
                "    Birth Year",
                sep = "\n"
            ),
            fixed = TRUE
        )
    })
    test_that("Composing a VariableOrder step by step: group by Order subset", {
        ord$Arrays <<- ord[c(2, 5)] # ds[c("mymrset", "catarray")]
        expect_prints(ord,
            paste("Categorical Location",
                "Text variable ftw",
                "starttime",
                "[+] Demos",
                "    Gender",
                "    Birth Year",
                "[+] Arrays",
                "    mymrset",
                "    Cat Array",
                sep = "\n"
            ),
            fixed = TRUE
        )
    })
    test_that("Composing a VariableOrder step by step: nested group by dataset", {
        ord$Demos[["Others"]] <<- ds[c("birthyr", "textVar")]
        expect_prints(ord,
            paste("Categorical Location",
                "starttime",
                "[+] Demos",
                "    Gender",
                "    [+] Others",
                "        Birth Year",
                "        Text variable ftw",
                "[+] Arrays",
                "    mymrset",
                "    Cat Array",
                sep = "\n"
            ),
            fixed = TRUE
        )
    })
    test_that("Composing a VariableOrder step by step: reorder group", {
        ord$Demos <<- ord$Demos[2:1]
        expect_prints(ord,
            paste("Categorical Location",
                "starttime",
                "[+] Demos",
                "    [+] Others",
                "        Birth Year",
                "        Text variable ftw",
                "    Gender",
                "[+] Arrays",
                "    mymrset",
                "    Cat Array",
                sep = "\n"
            ),
            fixed = TRUE
        )
    })
    test_that("Composing a VariableOrder step by step: reorder order", {
        ord <<- ord[4:1]
        expect_prints(ord,
            paste("[+] Arrays",
                "    mymrset",
                "    Cat Array",
                "[+] Demos",
                "    [+] Others",
                "        Birth Year",
                "        Text variable ftw",
                "    Gender",
                "starttime",
                "Categorical Location",
                sep = "\n"
            ),
            fixed = TRUE
        )
    })
    test_that("Composing a VariableOrder step by step: nested group by Group", {
        ord$Arrays$MR <<- ord$Arrays[1]
        expect_prints(ord,
            paste("[+] Arrays",
                "    Cat Array",
                "    [+] MR",
                "        mymrset",
                "[+] Demos",
                "    [+] Others",
                "        Birth Year",
                "        Text variable ftw",
                "    Gender",
                "starttime",
                "Categorical Location",
                sep = "\n"
            ),
            fixed = TRUE
        )
    })

    test_that("Order print method follows namekey", {
        with(temp.option(crunch.namekey.variableorder = "alias"), {
            expect_prints(ord,
                paste(
                    "[+] Arrays",
                    "    catarray",
                    "    [+] MR",
                    "        mymrset",
                    "[+] Demos",
                    "    [+] Others",
                    "        birthyr",
                    "        textVar",
                    "    gender",
                    "starttime",
                    "location",
                    sep = "\n"
                ),
                fixed = TRUE
            )
        })
    })

    test_that("VariableOrder to/fromJSON", {
        expect_identical(
            cereal(ord),
            list(graph = list(
                list(Arrays = list(
                    self(ds$catarray),
                    list(MR = list(
                        self(ds$mymrset)
                    ))
                )),
                list(Demos = list(
                    list(Others = list(
                        self(ds$birthyr),
                        self(ds$textVar)
                    )),
                    self(ds$gender)
                )),
                self(ds$starttime),
                self(ds$location)
            ))
        )
    })

    test_that("flattenOrder on that composed order", {
        expect_prints(flattenOrder(ord),
            paste(
                "Cat Array",
                "mymrset",
                "Birth Year",
                "Text variable ftw",
                "Gender",
                "starttime",
                "Categorical Location",
                sep = "\n"
            ),
            fixed = TRUE
        )
    })

    test_that("copyOrder returns the order of target as a VariableOrder", {
        ds_again <- loadDataset("test ds")
        expect_silent(new_order <- copyOrder(ds, ds_again))
        expect_is(new_order, "VariableOrder")
        expect_identical(entities(ordering(ds)), entities(new_order))
    })

    test_that("copyOrder input validation", {
        expect_error(
            copyOrder(ds, "foo"),
            "Both source and target must be Crunch datasets."
        )
    })
})


with_test_authentication({
    ds <- newDataset(df)
    test_that("Can get VariableOrder from dataset", {
        expect_true(setequal(
            unlist(entities(ordering(ds))),
            urls(allVariables(ds))
        ))
    })
    test_that("Can construct VariableOrder from variables", {
        # TODO: probably covered by unit tests
        vg <- VariableOrder(
            VariableGroup(
                name = "Group 1",
                variables = ds[c("v1", "v3", "v5")]
            ),
            VariableGroup(name = "Group 2.5", entities = ds["v4"]),
            VariableGroup(
                name = "Group 2",
                entities = ds[c("v6", "v2")]
            )
        )
        vglist <- cereal(vg)
        expect_identical(vglist, list(graph = list(
            list(`Group 1` = list(self(ds$v1), self(ds$v3), self(ds$v5))),
            list(`Group 2.5` = list(self(ds$v4))),
            list(`Group 2` = list(self(ds$v6), self(ds$v2)))
        )))
    })
    starting.vg <- vg <- VariableOrder(
        VariableGroup(
            name = "Group 1",
            entities = ds[c("v1", "v3", "v5")]
        ),
        VariableGroup(name = "Group 2.5", variables = ds["v4"]),
        VariableGroup(
            name = "Group 2",
            entities = ds[c("v6", "v2")]
        )
    )

    try(entities(vg[[2]]) <- self(ds$v2))
    test_that("Set URLs -> entities on VariableGroup", {
        # TODO: move to unit test
        expect_identical(urls(vg[[2]]), self(ds$v2))
        expect_identical(
            urls(vg),
            c(
                self(ds$v1), self(ds$v3), self(ds$v5), self(ds$v2),
                self(ds$v6)
            )
        )
    })
    try(entities(vg[[2]]) <- list(ds$v3))
    test_that("Set variables -> entities on VariableGroup", {
        # TODO: move to unit test
        expect_identical(urls(vg[[2]]), self(ds$v3))
    })

    try(name(vg[[2]]) <- "Group 3")
    test_that("Set name on VariableGroup", {
        # TODO: move to unit test
        expect_identical(names(vg), c("Group 1", "Group 3", "Group 2"))
    })
    try(names(vg) <- c("G3", "G1", "G2"))
    test_that("Set names on VariableOrder", {
        # TODO: move to unit test
        expect_identical(names(vg), c("G3", "G1", "G2"))
    })

    original.order <- ordering(ds)
    test_that("Can set VariableOrder on dataset", {
        expect_false(identical(starting.vg, original.order))
        ordering(ds) <- starting.vg
        expect_identical(
            entities(grouped(ordering(ds))),
            entities(starting.vg)
        )
        expect_identical(
            entities(grouped(ordering(refresh(ds)))),
            entities(starting.vg)
        )
        expect_is(ungrouped(ordering(ds)), "VariableGroup")
        expect_is(ungrouped(ordering(refresh(ds))), "VariableGroup")
        expect_identical(
            names(ordering(ds)),
            c("Group 1", "Group 2.5", "Group 2")
        )

        ## Test that can reorder groups
        ordering(ds) <- starting.vg[c(2, 1, 3)]
        expect_identical(
            entities(grouped(ordering(ds))),
            entities(starting.vg[c(2, 1, 3)])
        )
        expect_identical(
            names(ordering(ds)),
            c("Group 2.5", "Group 1", "Group 2")
        )
        expect_identical(
            names(ordering(refresh(ds))),
            c("Group 2.5", "Group 1", "Group 2")
        )

        ds <- refresh(ds)
        expect_false(identical(
            entities(ordering(variables(ds))),
            entities(original.order)
        ))
        ordering(variables(ds)) <- original.order
        expect_identical(
            entities(ordering(variables(ds))),
            entities(original.order)
        )
        expect_identical(
            entities(ordering(variables(refresh(ds)))),
            entities(original.order)
        )
    })

    test_that("A partial order results in 'ungrouped' variables", {
        ordering(ds) <- starting.vg[1:2]
        expect_is(grouped(ordering(ds)), "VariableOrder")
        expect_identical(
            entities(grouped(ordering(ds))),
            entities(starting.vg[1:2])
        )
        expect_is(ungrouped(ordering(ds)), "VariableGroup")
        expect_true(setequal(
            unlist(entities(ungrouped(ordering(ds)))),
            c(self(ds$v6), self(ds$v2))
        ))
    })

    test_that("grouped and ungrouped within a group", {
        nesting <- VariableGroup("Nest", self(ds$v3))
        ordering(ds) <- starting.vg
        ordering(ds)[["Group 1"]][[2]] <- nesting
        ## Update fixture with duplicates=TRUE, as it should be found
        ## after setting on a duplicates=TRUE order
        expect_identical(
            grouped(ordering(ds)[["Group 1"]]),
            VariableGroup("Group 1", list(nesting))
        )
        expect_identical(
            ungrouped(ordering(ds)[["Group 1"]]),
            VariableGroup("ungrouped", list(self(ds$v1), self(ds$v5)))
        )
    })

    test_that("Can manipulate VariableOrder that's part of a dataset", {
        ordering(ds) <- starting.vg
        expect_identical(
            names(ordering(ds)),
            c("Group 1", "Group 2.5", "Group 2")
        )
        names(ordering(ds))[3] <- "Three"
        expect_identical(
            names(ordering(ds)),
            c("Group 1", "Group 2.5", "Three")
        )
        expect_identical(
            names(grouped(ordering(ds))),
            c("Group 1", "Group 2.5", "Three")
        )
    })

    test_that("ordering<- validation", {
        # TODO: move to unit test
        bad.vg <- starting.vg
        entities(bad.vg[[1]]) <- c(
            entities(bad.vg[[1]])[-2],
            "/not/a/variable" # nolint
        )
        expect_error(
            ordering(ds) <- bad.vg,
            "Variable URL referenced in Order not present in catalog: /not/a/variable"
        )
    })

    test_that("Creating VariableOrder with named list doesn't break", {
        bad.vg <- do.call(VariableOrder, c(sapply(names(starting.vg),
            function(i) starting.vg[[i]],
            simplify = FALSE
        )))
        ## The list of entities is named because sapply default is
        ## USE.NAMES=TRUE, but the VariableOrder constructor should
        ## handle this
        ordering(ds) <- bad.vg
        expect_identical(ordering(ds)@graph, starting.vg@graph)
    })

    test_that("copyOrder copies across datasets with simple order", {
        ds_fork <- forkDataset(ds)
        old_order <- ordering(ds_fork)
        new_order <- VariableOrder(
            self(ds$v1), self(ds$v2), self(ds$v5),
            self(ds$v6), self(ds$v3), self(ds$v4)
        )
        new_order_fork <- VariableOrder(
            self(ds_fork$v1), self(ds_fork$v2),
            self(ds_fork$v5), self(ds_fork$v6),
            self(ds_fork$v3), self(ds_fork$v4)
        )
        ordering(ds) <- new_order

        # test that ds has the new order
        expect_identical(entities(ordering(ds)), entities(new_order))
        # test that ds_fork has the old order still
        expect_identical(entities(ordering(ds_fork)), entities(old_order))
        expect_false(identical(entities(ordering(ds_fork)), entities(new_order_fork)))

        # copy order, and check that ds_fork has the new order.
        expect_silent(copied_order <- copyOrder(ds, ds_fork))
        ordering(ds_fork) <- copied_order
        expect_identical(entities(ordering(ds_fork)), entities(new_order_fork))
    })

    test_that("copyOrder copies across datasets with simple(-ish) order (and one nesting)", {
        ds_fork <- forkDataset(ds)
        old_order <- ordering(ds_fork)
        new_order <- VariableOrder(
            self(ds$v1), self(ds$v2), self(ds$v5),
            self(ds$v6), VariableGroup(
                "Group A",
                list(self(ds$v4), self(ds$v3))
            )
        )
        new_order_fork <- VariableOrder(
            self(ds_fork$v1), self(ds_fork$v2),
            self(ds_fork$v5), self(ds_fork$v6),
            VariableGroup(
                "Group A",
                list(self(ds_fork$v4), self(ds_fork$v3))
            )
        )
        ordering(ds) <- new_order

        # test that ds has the new order
        expect_identical(entities(ordering(ds)), entities(new_order))
        # test that ds_fork has the old order still
        expect_identical(entities(ordering(ds_fork)), entities(old_order))
        expect_false(identical(entities(ordering(ds_fork)), entities(new_order_fork)))

        # copy order, and check that ds_fork has the new order.
        expect_silent(copied_order <- copyOrder(ds, ds_fork))
        ordering(ds_fork) <- copied_order
        expect_identical(entities(ordering(ds_fork)), entities(new_order_fork))
    })


    test_that("copyOrder copies across datasets with nested hierarchical order", {
        ds_fork <- forkDataset(ds)
        old_order <- ordering(ds_fork)
        new_order <- VariableOrder(
            VariableGroup("Group 1", list(
                self(ds$v1), self(ds$v2),
                VariableGroup("Group 1.5", list(self(ds$v5), self(ds$v6)))
            )),
            VariableGroup("Group 2", list(self(ds$v4), self(ds$v3)))
        )
        new_order_fork <- VariableOrder(
            VariableGroup("Group 1", list(
                self(ds_fork$v1), self(ds_fork$v2),
                VariableGroup("Group 1.5", list(self(ds_fork$v5), self(ds_fork$v6)))
            )),
            VariableGroup("Group 2", list(self(ds_fork$v4), self(ds_fork$v3)))
        )
        ordering(ds) <- new_order

        # test that ds has the new order
        expect_identical(entities(ordering(ds)), entities(new_order))
        # test that ds_fork has the old order still
        expect_identical(entities(ordering(ds_fork)), entities(old_order))
        expect_false(identical(entities(ordering(ds_fork)), entities(new_order_fork)))

        # copy order, and check that ds_fork has the new order.
        expect_silent(copied_order <- copyOrder(ds, ds_fork))
        ordering(ds_fork) <- copied_order
        expect_identical(entities(ordering(ds_fork)), entities(new_order_fork))
    })

    test_that("copyOrder copies across disparate datasets", {
        # setup an alternative dataset that has some overlap with ds
        df_alt <- df
        df_alt$v12 <- df_alt$v1
        df_alt$v1 <- NULL
        df_alt$v2 <- NULL
        df_alt$new_var <- 1
        df_alt$new_var2 <- letters[20:1]
        ds_alt <- newDataset(df_alt)

        old_order <- ordering(ds_alt)
        new_order <- VariableOrder(
            self(ds$v1), self(ds$v2), self(ds$v5),
            self(ds$v6), VariableGroup(
                "Group A",
                list(self(ds$v4), self(ds$v3))
            )
        )
        new_order_alt <- VariableOrder(
            self(ds_alt$v5), self(ds_alt$v6),
            VariableGroup(
                "Group A",
                list(self(ds_alt$v4), self(ds_alt$v3))
            ),
            # the following variables do not overlap with ds,
            # and therefor will be appended to the end,
            # but their order will not be garuanteed
            self(ds_alt$v12), self(ds_alt$new_var), self(ds_alt$new_var2)
        )
        ordering(ds) <- new_order

        # test that ds has the new order
        expect_identical(entities(ordering(ds)), entities(new_order))
        # test that ds_alt has the old order still
        expect_identical(entities(ordering(ds_alt)), entities(old_order))
        expect_false(identical(entities(ordering(ds_alt)), entities(new_order_alt)))

        # copy order, and check that ds_alt has the new order.
        expect_silent(copied_order <- copyOrder(ds, ds_alt))
        ordering(ds_alt) <- copied_order
        # ignore the last three variables because their order was not specified
        expect_identical(
            entities(ordering(ds_alt))[-c(4, 5, 6)],
            entities(new_order_alt)[-c(4, 5, 6)]
        )
    })
})
