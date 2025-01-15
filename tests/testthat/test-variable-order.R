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
    ds <- cachedLoadDataset("test ds")
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
        expect_error(
            ordering(ds) <- nested.ord[2:1],
            "Hey!"
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
        with(temp.option(crunch = list(crunch.delimiter = "|")), {
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

    ds3 <- cachedLoadDataset("ECON.sav")
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
        with(temp.option(crunch = list(crunch.namekey.variableorder = "alias")), {
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

    test_that("copyOrder has been removed", {
        ds_again <- cachedLoadDataset("test ds")
        expect_error(
            new_order <- copyOrder(ds, ds_again),
            "There's a new way to copy ordering and folders: `copyFolders`!"
        )
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
})
