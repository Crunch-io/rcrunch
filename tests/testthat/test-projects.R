context("Projects")

with_mock_HTTP({
    projects <- session()$projects
    test_that("Getting projects catalog", {
        expect_true(inherits(projects, "ProjectCatalog"))
        expect_identical(length(projects), 2L)
        expect_identical(names(projects), c("Project One", "Project Two"))
    })

    aproject <- projects[["Project One"]]
    test_that("Getting project from catalog", {
        expect_true(inherits(projects[[1]], "CrunchProject"))
        expect_true(inherits(projects$`Project One`, "CrunchProject"))
        expect_true(inherits(projects[["Project One"]], "CrunchProject"))
        expect_true(is.null(projects$`Beta Project`))
    })

    test_that("Project attributes", {
        expect_identical(name(aproject), "Project One")
    })

    test_that("Simple project creation by assignment", {
        expect_error(projects[["A new project"]] <- list(),
            'POST /api/projects.json {"name":"A new project"}',
            fixed=TRUE)
        expect_error(projects$`A new project` <- list(),
            'POST /api/projects.json {"name":"A new project"}',
            fixed=TRUE)
    })

    test_that("Project editing", {
        expect_error(names(projects)[2] <- "New name",
            paste('PATCH /api/projects.json',
            '{"/api/projects/project2.json":{"name":"New name"}}'),
            fixed=TRUE)
        expect_error(name(projects[[2]]) <- "New name",
            paste('PATCH /api/projects.json',
            '{"/api/projects/project2.json":{"name":"New name"}}'),
            fixed=TRUE)
    })

    test_that("Project deletion", {
        expect_error(delete(projects[[1]], confirm=TRUE),
            "Must confirm deleting project")
        with(consent(), expect_error(delete(projects[[1]], confirm=TRUE),
            "DELETE /api/projects/project1.json"))
    })

    m <- members(aproject)
    test_that("Project members catalog", {
        expect_true(inherits(m, "MemberCatalog"))
        expect_identical(names(m), c("Fake User", "Roger User"))
    })

    test_that("Add members by members<-", {
        expect_error(members(aproject) <- c("new.user@crunch.io", "foo@example.co"),
            'PATCH /api/projects/project1/members.json {"new.user@crunch.io":{},"foo@example.co":{}}',
            fixed=TRUE)
    })

    test_that("Add members doesn't re-add if already a member", {
        expect_error(members(aproject) <- c("new.user@crunch.io", "roger.user@example.com"),
            'PATCH /api/projects/project1/members.json {"new.user@crunch.io":{}}',
            fixed=TRUE)
    })

    test_that("Remove members by <- NULL", {
        expect_error(members(aproject)[["roger.user@example.com"]] <- NULL,
            'PATCH /api/projects/project1/members.json {"roger.user@example.com":null}',
            fixed=TRUE)
    })

    test_that("is.editor on member catalog", {
        expect_identical(is.editor(m), c(TRUE, FALSE))
    })

    test_that("is.editor<- on member catalog", {
        expect_error(is.editor(m) <- c(TRUE, TRUE),
            paste('PATCH /api/projects/project1/members.json',
                '{"/api/users/user2.json":{"permissions":{"edit":true}}}'),
            fixed=TRUE)
        expect_error(is.editor(m[2]) <- TRUE,
            paste('PATCH /api/projects/project1/members.json',
                '{"/api/users/user2.json":{"permissions":{"edit":true}}}'),
            fixed=TRUE)
        expect_error(is.editor(m[2]) <- FALSE,
            NA) ## No change, so no PATCH request made
    })

    d <- datasets(aproject)
    test_that("Project datasets catalog", {
        expect_true(inherits(d, "DatasetCatalog"))
        expect_identical(names(d), "ECON.sav")
    })

    do <- ordering(d)
    test_that("Project datasets order", {
        expect_true(inherits(do, "DatasetOrder"))
        expect_identical(do@graph, list(DatasetGroup("Group 1", "/api/datasets/dataset3.json")))
    })

    test_that("Add datasets to project by <- a dataset (which transfers ownership)", {
        ds <- loadDataset("test ds")
        expect_error(datasets(aproject) <- ds,
            'PATCH /api/datasets/dataset1.json {"owner":"/api/projects/project1.json"}',
            fixed=TRUE)
    })

    test_that("Organize datasets", {
        expect_identical(DatasetOrder(DatasetGroup("new group", datasets(aproject))),
            DatasetOrder(DatasetGroup("new group", "/api/datasets/dataset3.json")))
        expect_error(ordering(datasets(aproject)) <- DatasetOrder(DatasetGroup("new group",
            datasets(aproject))),
            'PUT /api/projects/project1/datasets/order.json {"graph":[{"new group":["/api/datasets/dataset3.json"]}]}',
            fixed=TRUE)
        nested.ord <- DatasetOrder("/api/datasets/dataset3.json",
            DatasetGroup("new group",
                list(DatasetGroup("nested", "/api/datasets/dataset3.json"))))
        expect_error(ordering(datasets(aproject)) <- nested.ord,
            'PUT /api/projects/project1/datasets/order.json {"graph":["/api/datasets/dataset3.json",{"new group":[{"nested":["/api/datasets/dataset3.json"]}]}]}',
            fixed=TRUE)
    })
})

if (run.integration.tests) {
    with(test.authentication, {
        projects <- session()$projects
        ucat <- getAccountUserCatalog()
        my.name <- names(ucat)[urls(ucat) == userURL()]
        my.email <- emails(ucat)[urls(ucat) == userURL()]

        nprojects.0 <- length(projects)
        test_that("Can get project catalog", {
            expect_true(inherits(projects, "ProjectCatalog"))
        })

        name.of.project1 <- now()
        test_that("Can create a project", {
            expect_false(name.of.project1 %in% names(projects))
            projects[[name.of.project1]] <- list()
            expect_true(name.of.project1 %in% names(projects))
            expect_true(length(projects) == nprojects.0 + 1L)
            expect_true(inherits(projects[[name.of.project1]], "CrunchProject"))
            expect_identical(length(members(projects[[name.of.project1]])), 1L)
            expect_identical(names(members(projects[[name.of.project1]])),
                my.name)
        })

        projects <- refresh(projects)
        pj <- projects[[name.of.project1]]
        p_url <- self(pj)
        name2 <- paste(name.of.project1, "revised")
        test_that("Can rename a project by name<-", {
            expect_identical(self(projects[[name.of.project1]]),
                p_url)
            expect_identical(projects[[name2]], NULL)
            name(projects[[name.of.project1]]) <- name2
            expect_identical(projects[[name.of.project1]],
                NULL)
            expect_identical(self(projects[[name2]]), p_url)
        })

        name3 <- paste(name2, "FINAL")
        test_that("Can rename a project with names<-", {
            expect_false(name3 %in% names(projects))
            names(projects)[urls(projects) == p_url] <- name3
            expect_true(name3 %in% names(projects))
            expect_identical(self(projects[[name3]]), p_url)
        })

        test_that("Get and set project icon", {
            ico <- icon(pj)
            # expect_true(nchar(ico) > 0) ## Unskip after #119305641 ships
            icon(pj) <- "empty.png"
            print(icon(pj))
            expect_false(icon(pj) == "empty.png")
            expect_true(endsWith(icon(pj), ".png"))
            expect_false(identical(icon(pj), ico))
        })


        test_that("Can delete a project by URL", {
            projects <- refresh(projects)
            expect_true(p_url %in% urls(projects))
            try(crDELETE(p_url))
            expect_false(p_url %in% urls(refresh(projects)))
        })

        test_that("Can create a project with members", {
            skip("TODO")
            skip_on_jenkins("Jenkins user needs more permissions")
            projects <- refresh(projects)
            nprojects.2 <- length(projects)
            name.of.project2 <- now()
            expect_false(name.of.project2 %in% names(projects))
            with(cleanup(testUser()), as="u", {
                projects[[name.of.project2]] <- list(members=email(u))
                expect_true(name.of.project2 %in% names(projects))
                with(cleanup(projects[[name.of.project2]]), as="tp", {
                    expect_true(length(projects) == nprojects.2 + 1L)
                    expect_true(setequal(names(members(tp)),
                        c(name(u), my.name)))
                })
            })
        })

        test_that("Can add members to a project (and then set as an editor)", {
            skip_on_jenkins("Jenkins user needs more permissions")
            with(cleanup(testProject()), as="tp", {
                with(cleanup(testUser()), as="u", {
                    expect_identical(names(members(tp)),
                        my.name)
                    members(tp) <- email(u)
                    expect_true(setequal(names(members(tp)),
                        c(name(u), my.name)))

                    expect_identical(is.editor(members(tp)),
                        c(TRUE, FALSE))
                    is.editor(members(tp)[email(u)]) <- TRUE
                    expect_identical(is.editor(members(tp)),
                        c(TRUE, TRUE))
                })
            })
        })

        test_that("Can remove members from a project", {
            skip_on_jenkins("Jenkins user needs more permissions")
            with(cleanup(testProject()), as="tp", {
                with(cleanup(testUser()), as="u", {
                    expect_identical(names(members(tp)),
                        my.name)
                    members(tp) <- email(u)
                    expect_true(setequal(names(members(tp)),
                        c(name(u), my.name)))
                    try(members(tp)[[email(u)]] <- NULL)
                    expect_identical(names(members(tp)),
                        my.name)
                })
            })
        })

        with(test.dataset(), {
            with(cleanup(testProject()), as="tp", {
                test_that("Can add datasets to project", {
                    expect_true(inherits(tp, "CrunchProject"))
                    expect_identical(length(datasets(tp)), 0L)
                    datasets(tp) <- ds
                    expect_identical(names(datasets(tp)), name(ds))
                    expect_identical(owner(refresh(ds)), self(tp))
                })
                ds2 <- loadDataset(datasets(tp)[[1]])
                test_that("Can load a dataset from a project", {
                    expect_true(is.dataset(ds2))
                    expect_identical(self(ds2), self(ds))
                })
                test_that("Can organize datasets", {
                    expect_identical(as.list(urls(datasets(tp))),
                        entities(ordering(datasets(tp))))
                    ordering(datasets(tp)) <- DatasetOrder(DatasetGroup("A group of one",
                        list(ds)))
                    expect_identical(ordering(datasets(tp))@graph[[1]],
                        DatasetGroup(name="A group of one", entities=self(ds)))
                })
                with(test.dataset(), as="ds3", {
                    ord2 <- DatasetOrder(DatasetGroup("A group of two",
                        c(self(ds), self(ds3))))
                    test_that("Have to add dataset to project before organizing it", {
                        expect_error(ordering(datasets(tp)) <- ord2,
                            "Dataset URL referenced in Order not present in catalog")
                        expect_identical(ordering(datasets(tp))@graph[[1]],
                            DatasetGroup(name="A group of one", entities=self(ds)))
                    })
                    owner(ds3) <- tp
                    tp <- refresh(tp)
                    test_that("Can reorganize datasets", {
                        ordering(datasets(tp)) <- ord2
                        expect_identical(ordering(datasets(tp))@graph[[1]],
                            DatasetGroup(name="A group of two",
                            entities=c(self(ds), self(ds3))))
                        expect_output(ordering(datasets(tp)),
                            paste("[+] A group of two",
                                  paste0("    ", name(ds)),
                                  paste0("    ", name(ds3)),
                                  sep="\n"),
                            fixed=TRUE)
                    })
                    ord3 <- DatasetOrder(DatasetGroup("G1", self(ds3)),
                        DatasetGroup("G2", self(ds)))
                    ord3.list <- list(DatasetGroup("G1", self(ds3)),
                        DatasetGroup("G2", self(ds)))
                    ord3.alt <- DatasetOrder(
                        DatasetGroup("G1", datasets(tp)[names(datasets(tp)) == name(ds3)]),
                        DatasetGroup("G2", datasets(tp)[names(datasets(tp)) == name(ds)]))
                    test_that("Can re-reorganize", {
                        expect_identical(ord3, ord3.alt)
                        ordering(datasets(tp)) <- ord3
                        expect_identical(ordering(datasets(tp))@graph,
                            ord3.list)
                        expect_identical(ordering(datasets(refresh(tp)))@graph,
                            ord3.list)
                    })

                    test_that("Can create a Group by assigning by name", {
                        ordering(datasets(tp))[["New group three"]] <- self(ds)
                        expect_output(ordering(datasets(tp)),
                            paste("[+] G1",
                                  paste0("    ", name(ds3)),
                                  "[+] G2",
                                  "    (Empty group)",
                                  "[+] New group three",
                                  paste0("    ", name(ds)),
                                  sep="\n"),
                            fixed=TRUE)
                    })
                })

                test_that("Can rename a dataset in a project", {
                    newname <- paste(name(ds2), "edited")
                    name(ds2) <- newname
                    expect_identical(name(ds2), newname)
                    expect_identical(name(refresh(ds2)), newname)
                    expect_identical(name(datasets(refresh(tp))[[1]]),
                        newname)
                })

                test_that("Can privatize a dataset belonging to a project", {
                    expect_identical(owner(ds2), self(tp))
                    owner(ds2) <- me()
                    expect_identical(owner(ds2), self(me()))
                })
            })
        })
    })
}
