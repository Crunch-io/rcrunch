context("Projects")

with_mock_crunch({
    projects <- session()$projects
    test_that("Getting projects catalog", {
        expect_is(projects, "ProjectFolder")
        expect_length(projects, 2)
        expect_identical(names(projects), c("Project One", "Project Three"))
    })

    test_that("Getting project from catalog", {
        expect_true(is.project(projects[[1]]))
        expect_true(is.project(projects$`Project One`))
        expect_true(is.project(projects[["Project One"]]))
        expect_null(projects$`Beta Project`)
    })

    aproject <- projects[["Project One"]]
    test_that("Project attributes", {
        expect_identical(name(aproject), "Project One")
    })

    creation <- '{"element":"shoji:entity","body":{"name":"A new project"}}'
    test_that("Simple project creation by assignment", {
        expect_POST(
            projects[["A new project"]] <- list(),
            "https://app.crunch.io/api/projects/",
            creation
        )
        expect_POST(
            projects$`A new project` <- list(),
            "https://app.crunch.io/api/projects/",
            creation
        )
    })

    test_that("Project creation with newProject", {
        expect_POST(
            newProject("A new project"),
            "https://app.crunch.io/api/projects/",
            creation
        )
        with_POST("https://app.crunch.io/api/projects/project1/", {
            ## Mock the return of that creation
            pro <- newProject("This is being ignored")
            expect_true(is.project(pro))
            expect_identical(name(pro), "Project One")
            ## Now also check that the PATCH to add members happens
            expect_PATCH(
                newProject("A new project", members = "new.user@crunch.io"),
                "https://app.crunch.io/api/projects/project1/members/",
                '{"new.user@crunch.io":{}}'
            )
        })
    })

    test_that("Project editing", {
        expect_PATCH(
            names(projects)[2] <- "New name",
            "https://app.crunch.io/api/projects/",
            '{"element":"shoji:catalog","index":',
            '{"https://app.crunch.io/api/projects/project3/":{"name":"New name"}}}'
        )
        expect_PATCH(
            name(projects[[2]]) <- "New name",
            "https://app.crunch.io/api/projects/project3/",
            '{"name":"New name"}'
        )
    })

    test_that("Project deletion", {
        expect_error(
            delete(projects[[1]]),
            "Must confirm deleting folder"
        )
        with(consent(), {
            expect_DELETE(delete(projects[[1]]), "https://app.crunch.io/api/projects/project1/")
        })
    })

    m <- members(aproject)
    test_that("Project members catalog", {
        expect_is(m, "MemberCatalog")
        expect_identical(names(m), c("Fake User", "Roger User"))
        expect_identical(emails(m), c("fake.user@example.com", "roger.user@example.com"))
        expect_identical(
            name(m[["roger.user@example.com"]]),
            "Roger User"
        )
        expect_null(m[["NOTAUSER@example.com"]])
        expect_identical(
            names(m["roger.user@example.com"]),
            "Roger User"
        )
        expect_error(
            m["NOTAUSER@example.com"],
            "Undefined elements selected: NOTAUSER@example.com"
        )
    })

    test_that("Add members by members<-", {
        expect_PATCH(
            members(aproject) <- c("new.user@crunch.io", "foo@example.co"),
            "https://app.crunch.io/api/projects/project1/members/",
            '{"new.user@crunch.io":{},"foo@example.co":{}}'
        )
    })

    test_that("Add members doesn't re-add if already a member", {
        expect_PATCH(
            members(aproject) <- c("new.user@crunch.io", "roger.user@example.com"),
            "https://app.crunch.io/api/projects/project1/members/",
            '{"new.user@crunch.io":{}}'
        )
    })

    test_that("Remove members by <- NULL", {
        expect_PATCH(
            members(aproject)[["roger.user@example.com"]] <- NULL,
            "https://app.crunch.io/api/projects/project1/members/",
            '{"roger.user@example.com":null}'
        )
    })

    test_that("is.editor on member catalog", {
        expect_identical(is.editor(m), c(TRUE, FALSE))
    })

    test_that("is.editor<- on member catalog", {
        expect_PATCH(
            is.editor(m) <- c(TRUE, TRUE),
            "https://app.crunch.io/api/projects/project1/members/",
            '{"https://app.crunch.io/api/users/user2/":{"permissions":{"edit":true}}}'
        )
        expect_PATCH(
            is.editor(m[2]) <- TRUE,
            "https://app.crunch.io/api/projects/project1/members/",
            '{"https://app.crunch.io/api/users/user2/":{"permissions":{"edit":true}}}'
        )
        expect_no_request(is.editor(m[2]) <- FALSE) ## No change, so no PATCH request made
    })

    test_that("Print method for MemberCatalog", {
        expect_prints(
            m,
            get_output(data.frame(
                name = c("Fake User", "Roger User"),
                email = c("fake.user@example.com", "roger.user@example.com"),
                is.editor = c(TRUE, FALSE)
            ))
        )
    })

    d <- datasets(aproject)
    test_that("datasets() filters a project folder", {
        expect_identical(
            names(d),
            c("an archived dataset", "test ds", "streaming no messages")
        )
    })

    test_that("Can loadDataset from a project dataset catalog", {
        ds <- loadDataset("test ds", project = aproject)
        expect_is(ds, "CrunchDataset")
        expect_identical(name(ds), "test ds")
        expect_identical(
            loadDataset("test ds", project = "Project One"),
            ds
        )
    })

    test_that("loadDataset project arg error handling", {
        expect_error(
            loadDataset("foo", project = 12),
            "project must be a `CrunchProject` object, a URL, or a path to a project from the root"
        )
        expect_error(
            loadDataset("foo", project = "Not a project"),
            paste0(
                'Could not get project Not a project ',
                'because "Not a project" is not a folder'
            )
        )
    })

    test_that("Project datasets order", {
        expect_deprecated(do <- ordering(d))
        expect_is(do, "DatasetOrder")
        expect_identical(
            do@graph,
            list(DatasetGroup("Group 1", "https://app.crunch.io/api/datasets/3/"))
        )
        expect_prints(do,
            paste("[+] Group 1", "    ECON.sav", sep = "\n"),
            fixed = TRUE
        )
    })

    test_that("Add datasets to project by <- a dataset (which calls mv)", {
        ds <- cachedLoadDataset("ECON.sav")
        expect_PATCH(
            datasets(aproject) <- ds,
            "https://app.crunch.io/api/projects/project1/"
        )
    })
    test_that("Add datasets to project by <- does nothing if already present", {
        ds <- cachedLoadDataset("test ds")
        expect_no_request(datasets(aproject) <- ds)
    })

    test_that("Organize datasets is gone!", {
        expect_error(
            ordering(aproject) <- DatasetOrder(DatasetGroup(
                "new group",
                datasets(aproject)
            )),
            "Hi there!"
        )
    })
})

with_test_authentication({
    myprojects <- projects()
    my.name <- name(me())
    my.email <- email(me())

    nprojects.0 <- length(myprojects)
    test_that("Can get project catalog", {
        expect_is(myprojects, "ProjectFolder")
    })

    name.of.project1 <- now()
    test_that("Can create a project", {
        expect_false(name.of.project1 %in% names(myprojects))
        myprojects[[name.of.project1]] <- list()
        expect_true(name.of.project1 %in% names(myprojects))
        expect_true(length(myprojects) == nprojects.0 + 1L)
        expect_true(is.project(myprojects[[name.of.project1]]))
        expect_length(members(myprojects[[name.of.project1]]), 1)
        expect_identical(
            names(members(myprojects[[name.of.project1]])),
            my.name
        )
    })

    myprojects <- refresh(myprojects)
    pj <- myprojects[[name.of.project1]]
    p_url <- self(pj)
    name2 <- paste(name.of.project1, "revised")
    test_that("Can rename a project by name<-", {
        expect_identical(
            self(myprojects[[name.of.project1]]),
            p_url
        )
        expect_null(myprojects[[name2]])
        name(myprojects[[name.of.project1]]) <- name2
        expect_null(myprojects[[name.of.project1]])
        expect_identical(self(myprojects[[name2]]), p_url)
    })

    name3 <- paste(name2, "FINAL")
    test_that("Can rename a project with names<-", {
        expect_false(name3 %in% names(myprojects))
        names(myprojects)[urls(myprojects) == p_url] <- name3
        expect_true(name3 %in% names(myprojects))
        expect_identical(self(myprojects[[name3]]), p_url)
    })

    test_that("Can delete a project by URL", {
        myprojects <- refresh(myprojects)
        expect_true(p_url %in% urls(myprojects))
        try(crDELETE(p_url))
        expect_false(p_url %in% urls(refresh(myprojects)))
    })

    test_that("Can create a project with members", {
        skip("TODO")
        skip_on_jenkins("Jenkins user needs more permissions")
        myprojects <- refresh(myprojects)
        nprojects.2 <- length(myprojects)
        name.of.project2 <- now()
        expect_false(name.of.project2 %in% names(myprojects))
        u <- testUser()
        myprojects[[name.of.project2]] <- list(members = email(u))
        expect_true(name.of.project2 %in% names(myprojects))
        expect_true(length(myprojects) == nprojects.2 + 1L)
        expect_true(setequal(
            names(members(myprojects[[name.of.project2]])),
            c(name(u), my.name)
        ))
    })

    test_that("Can add members to a project (and then set as an editor)", {
        skip_on_jenkins("Jenkins user needs more permissions")
        tp <- newProject(name = now())
        u <- testUser()
        expect_identical(names(members(tp)), my.name)
        members(tp) <- email(u)
        expect_true(setequal(
            names(members(tp)),
            c(name(u), my.name)
        ))
        expect_identical(
            is.editor(members(tp)[email(u)]),
            c(FALSE)
        )
        is.editor(members(tp)[email(u)]) <- TRUE
        expect_identical(
            is.editor(members(tp)[email(u)]),
            c(TRUE)
        )
    })

    test_that("Can remove members from a project", {
        skip_on_jenkins("Jenkins user needs more permissions")
        tp <- newProject(name = now())
        u <- testUser()
        expect_identical(names(members(tp)), my.name)
        members(tp) <- email(u)
        expect_true(setequal(names(members(tp)), c(name(u), my.name)))
        try(members(tp)[[email(u)]] <- NULL)
        expect_identical(names(members(tp)), my.name)
    })

    ds <- createDataset(name = now())
    tp <- newProject(name = now())
    test_that("Can add datasets to project", {
        expect_true(is.project(tp))
        expect_length(datasets(tp), 0)
        datasets(tp) <- ds
        expect_identical(names(datasets(tp)), name(ds))
        expect_identical(owner(refresh(ds)), self(tp))
    })
    ds2 <- loadDataset(name(ds), project = name(tp))
    test_that("Can load a dataset from a project", {
        expect_true(is.dataset(ds2))
        expect_identical(self(ds2), self(ds))
    })

    test_that("Can rename a dataset in a project", {
        newname <- paste(name(ds2), "edited")
        name(ds2) <- newname
        expect_identical(name(ds2), newname)
        expect_identical(name(refresh(ds2)), newname)
        expect_identical(name(datasets(refresh(tp))[[1]]), newname)
    })

    test_that("Can privatize a dataset belonging to a project", {
        expect_identical(owner(ds2), self(tp))
        owner(ds2) <- me()
        expect_identical(owner(ds2), self(me()))
    })
})
