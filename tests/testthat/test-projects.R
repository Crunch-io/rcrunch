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
        p_url <- self(projects[[name.of.project1]])
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
            with(test.user(), {
                ucat <- getUserCatalog()
                u.email <- emails(ucat)[urls(ucat) == u]
                u.name <- names(ucat)[urls(ucat) == u]
                projects[[name.of.project2]] <- list(members=u.email)
                expect_true(name.of.project2 %in% names(projects))
                with(cleanup(projects[[name.of.project2]]), as="tp", {
                    expect_true(length(projects) == nprojects.2 + 1L)
                    expect_true(setequal(names(members(tp)),
                        c(u.name, my.name)))
                })
            })
        })

        test_that("Can add members to a project", {
            skip_on_jenkins("Jenkins user needs more permissions")
            with(cleanup(testProject()), as="tp", {
                with(test.user(), {
                    ucat <- getUserCatalog()
                    u.email <- emails(ucat)[urls(ucat) == u]
                    u.name <- names(ucat)[urls(ucat) == u]
                    expect_identical(names(members(tp)),
                        my.name)
                    members(tp) <- u.email
                    expect_true(setequal(names(members(tp)),
                        c(u.name, my.name)))
                })
            })
        })

        test_that("Can remove members from a project", {
            skip_on_jenkins("Jenkins user needs more permissions")
            with(cleanup(testProject()), as="tp", {
                with(test.user(), {
                    ucat <- getUserCatalog()
                    u.email <- emails(ucat)[urls(ucat) == u]
                    u.name <- names(ucat)[urls(ucat) == u]
                    expect_identical(names(members(tp)),
                        my.name)
                    members(tp) <- u.email
                    expect_true(setequal(names(members(tp)),
                        c(u.name, my.name)))
                    try(members(tp)[[u.email]] <- NULL)
                    expect_identical(names(members(tp)),
                        my.name)
                })
            })
        })

        with(test.dataset(df), {
            with(cleanup(testProject()), as="tp", {
                test_that("Can add datasets to project", {
                    expect_true(inherits(tp, "CrunchProject"))
                    expect_identical(length(datasets(tp)), 0L)
                    datasets(tp) <- ds
                    expect_identical(names(datasets(tp)), name(ds))
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
                test_that("Can rename a dataset in a project", {
                    newname <- paste(name(ds2), "edited")
                    name(ds2) <- newname
                    expect_identical(name(ds2), newname)
                    expect_identical(name(refresh(ds2)), newname)
                    expect_identical(name(datasets(refresh(tp))[[1]]),
                        newname)
                })
            })
        })
    })
}
