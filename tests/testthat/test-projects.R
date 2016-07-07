context("Projects")

with_mock_HTTP({
    projects <- session()$projects
    test_that("Getting projects catalog", {
        expect_is(projects, "ProjectCatalog")
        expect_length(projects, 2)
        expect_identical(names(projects), c("Project One", "Project Two"))
    })

    aproject <- projects[["Project One"]]
    test_that("Getting project from catalog", {
        expect_is(projects[[1]], "CrunchProject")
        expect_is(projects$`Project One`, "CrunchProject")
        expect_is(projects[["Project One"]], "CrunchProject")
        expect_null(projects$`Beta Project`)
    })

    test_that("Project attributes", {
        expect_identical(name(aproject), "Project One")
    })

    test_that("Simple project creation by assignment", {
        expect_POST(projects[["A new project"]] <- list(),
            '/api/projects.json',
            '{"name":"A new project"}')
        expect_POST(projects$`A new project` <- list(),
            '/api/projects.json',
            '{"name":"A new project"}')
    })

    test_that("Project editing", {
        expect_PATCH(names(projects)[2] <- "New name",
            '/api/projects.json',
            '{"/api/projects/project2.json":{"name":"New name"}}')
        expect_PATCH(name(projects[[2]]) <- "New name",
            '/api/projects.json',
            '{"/api/projects/project2.json":{"name":"New name"}}')
    })

    test_that("Project deletion", {
        expect_error(delete(projects[[1]], confirm=TRUE),
            "Must confirm deleting project")
        with(consent(), expect_DELETE(delete(projects[[1]], confirm=TRUE),
            "/api/projects/project1.json"))
    })

    m <- members(aproject)
    test_that("Project members catalog", {
        expect_is(m, "MemberCatalog")
        expect_identical(names(m), c("Fake User", "Roger User"))
        expect_identical(emails(m), c("fake.user@example.com", "roger.user@example.com"))
    })

    test_that("Add members by members<-", {
        expect_PATCH(members(aproject) <- c("new.user@crunch.io", "foo@example.co"),
            '/api/projects/project1/members.json',
            '{"new.user@crunch.io":{},"foo@example.co":{}}')
    })

    test_that("Add members doesn't re-add if already a member", {
        expect_PATCH(members(aproject) <- c("new.user@crunch.io", "roger.user@example.com"),
            '/api/projects/project1/members.json',
            '{"new.user@crunch.io":{}}')
    })

    test_that("Remove members by <- NULL", {
        expect_PATCH(members(aproject)[["roger.user@example.com"]] <- NULL,
            '/api/projects/project1/members.json',
            '{"roger.user@example.com":null}')
    })

    test_that("is.editor on member catalog", {
        expect_identical(is.editor(m), c(TRUE, FALSE))
    })

    test_that("is.editor<- on member catalog", {
        expect_PATCH(is.editor(m) <- c(TRUE, TRUE),
            '/api/projects/project1/members.json',
            '{"/api/users/user2.json":{"permissions":{"edit":true}}}')
        expect_PATCH(is.editor(m[2]) <- TRUE,
            '/api/projects/project1/members.json',
            '{"/api/users/user2.json":{"permissions":{"edit":true}}}')
        expect_no_request(is.editor(m[2]) <- FALSE) ## No change, so no PATCH request made
    })

    test_that("Print method for MemberCatalog", {
        expect_output(m,
            paste(capture.output(print(data.frame(
                name=c("Fake User", "Roger User"),
                email=c("fake.user@example.com", "roger.user@example.com"),
                is.editor=c(TRUE, FALSE)
            ))), collapse="\n")
        )
    })

    d <- datasets(aproject)
    test_that("Project datasets catalog", {
        expect_is(d, "DatasetCatalog")
        expect_identical(names(d), "ECON.sav")
    })

    do <- ordering(d)
    test_that("Project datasets order", {
        expect_is(do, "DatasetOrder")
        expect_identical(do@graph, list(DatasetGroup("Group 1", "/api/datasets/dataset3.json")))
    })

    test_that("Add datasets to project by <- a dataset (which transfers ownership)", {
        ds <- loadDataset("test ds")
        expect_PATCH(datasets(aproject) <- ds,
            '/api/datasets/dataset1.json',
            '{"owner":"/api/projects/project1.json"}')
    })

    test_that("Organize datasets", {
        expect_identical(DatasetOrder(DatasetGroup("new group", datasets(aproject))),
            DatasetOrder(DatasetGroup("new group", "/api/datasets/dataset3.json")))
        expect_PUT(ordering(datasets(aproject)) <- DatasetOrder(DatasetGroup("new group",
            datasets(aproject))),
            '/api/projects/project1/datasets/order.json',
            '{"graph":[{"new group":["/api/datasets/dataset3.json"]}]}')
        nested.ord <- DatasetOrder("/api/datasets/dataset3.json",
            DatasetGroup("new group",
                list(DatasetGroup("nested", "/api/datasets/dataset3.json"))))
        expect_PUT(ordering(datasets(aproject)) <- nested.ord,
            '/api/projects/project1/datasets/order.json',
            '{"graph":["/api/datasets/dataset3.json",',
            '{"new group":[{"nested":["/api/datasets/dataset3.json"]}]}]}')
    })
})

with_test_authentication({
    projects <- session()$projects
    my.name <- name(me())
    my.email <- email(me())

    nprojects.0 <- length(projects)
    test_that("Can get project catalog", {
        expect_is(projects, "ProjectCatalog")
    })

    name.of.project1 <- now()
    test_that("Can create a project", {
        expect_false(name.of.project1 %in% names(projects))
        projects[[name.of.project1]] <- list()
        expect_true(name.of.project1 %in% names(projects))
        expect_true(length(projects) == nprojects.0 + 1L)
        expect_is(projects[[name.of.project1]], "CrunchProject")
        expect_length(members(projects[[name.of.project1]]), 1)
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
        expect_null(projects[[name2]])
        name(projects[[name.of.project1]]) <- name2
        expect_null(projects[[name.of.project1]])
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
        u <- testUser()
        projects[[name.of.project2]] <- list(members=email(u))
        expect_true(name.of.project2 %in% names(projects))
        expect_true(length(projects) == nprojects.2 + 1L)
        expect_true(setequal(names(members(projects[[name.of.project2]])),
            c(name(u), my.name)))
    })

    test_that("Can add members to a project (and then set as an editor)", {
        skip_on_jenkins("Jenkins user needs more permissions")
        tp <- testProject()
        u <- testUser()
        expect_identical(names(members(tp)), my.name)
        members(tp) <- email(u)
        expect_true(setequal(names(members(tp)),
            c(name(u), my.name)))
        expect_identical(is.editor(members(tp)),
            c(TRUE, FALSE))
        is.editor(members(tp)[email(u)]) <- TRUE
        expect_identical(is.editor(members(tp)),
            c(TRUE, TRUE))
    })

    test_that("Can remove members from a project", {
        skip_on_jenkins("Jenkins user needs more permissions")
        tp <- testProject()
        u <- testUser()
        expect_identical(names(members(tp)), my.name)
        members(tp) <- email(u)
        expect_true(setequal(names(members(tp)), c(name(u), my.name)))
        try(members(tp)[[email(u)]] <- NULL)
        expect_identical(names(members(tp)), my.name)
    })

    ds <- crunch::createDataset()
    tp <- testProject()
    test_that("Can add datasets to project", {
        expect_is(tp, "CrunchProject")
        expect_length(datasets(tp), 0)
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
    ds3 <- crunch::createDataset()
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
        expect_identical(ordering(datasets(tp))@graph, ord3.list)
        expect_identical(ordering(datasets(refresh(tp)))@graph, ord3.list)
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
