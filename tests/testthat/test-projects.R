context("Projects")

with_mock_HTTP({
    projects <- session()$projects
    test_that("Getting projects catalog", {
        expect_true(inherits(projects, "ProjectCatalog"))
        expect_identical(length(projects), 1L)
        expect_identical(names(projects), "Project One")
    })

    test_that("Getting project from catalog", {
        expect_true(inherits(projects[[1]], "CrunchProject"))
        expect_true(inherits(projects$`Project One`, "CrunchProject"))
        expect_true(inherits(projects[["Project One"]], "CrunchProject"))
        expect_true(is.null(projects$`Beta Project`))
    })

    test_that("Project attributes", {
        aproject <- projects[[1]]
        expect_identical(name(aproject), "Project One")
        m <- members(aproject)
        expect_true(inherits(m, "MemberCatalog"))
        expect_identical(names(m), c("Fake User", "Roger User"))
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

        t2 <- projects
        name.of.project1 <- now()
        test_that("Can create a project", {
            expect_false(name.of.project1 %in% names(t2))
            t2[[name.of.project1]] <- list()
            expect_true(name.of.project1 %in% names(t2))
            expect_true(length(t2) == nprojects.0 + 1L)
            expect_true(inherits(t2[[name.of.project1]], "CrunchProject"))
            expect_identical(length(members(t2[[name.of.project1]])), 1L)
            expect_identical(names(members(t2[[name.of.project1]])),
                my.name)
        })

        test_that("Can delete a project by URL", {
            t2 <- refresh(t2)
            expect_true(name.of.project1 %in% names(t2))
            try(crDELETE(self(t2[[name.of.project1]])))
            expect_false(name.of.project1 %in% names(refresh(t2)))
        })

        test_that("delete method for project (requires confirmation)", {
            ## Setup
            t2 <- refresh(t2)
            nprojects.2 <- length(t2)
            name.of.project2 <- now()
            expect_false(name.of.project2 %in% names(t2))
            t2[[name.of.project2]] <- list()
            expect_true(name.of.project2 %in% names(t2))
            expect_true(length(t2) == nprojects.2 + 1L)

            expect_error(delete(t2[[name.of.project2]], confirm=TRUE),
                "Must confirm deleting project")
            expect_true(name.of.project2 %in% names(t2))
            expect_true(length(t2) == nprojects.2 + 1L)

            ## Cleanup
            try(delete(t2[[name.of.project2]]))
            expect_false(name.of.project2 %in% names(getProjects()))
        })

        test_that("Can create a project with members", {
            skip_on_jenkins("Jenkins user needs more permissions")
            t2 <- refresh(t2)
            nprojects.2 <- length(t2)
            name.of.project2 <- now()
            expect_false(name.of.project2 %in% names(t2))
            with(test.user(), {
                ucat <- getUserCatalog()
                u.email <- emails(ucat)[urls(ucat) == u]
                u.name <- names(ucat)[urls(ucat) == u]
                t2[[name.of.project2]] <- list(members=u.email)
                expect_true(name.of.project2 %in% names(t2))
                expect_true(length(t2) == nprojects.2 + 1L)
                this.project <- t2[[name.of.project2]]
                expect_true(setequal(names(members(this.project)),
                    c(u.name, my.name)))
            })
            try(crDELETE(self(refresh(t2)[[name.of.project2]])))
        })

        test_that("Can add members to a project", {
            skip_on_jenkins("Jenkins user needs more permissions")
            t2 <- refresh(projects)
            name.of.project3 <- now()
            expect_false(name.of.project3 %in% names(t2))
            with(test.user(), {
                ucat <- getUserCatalog()
                u.email <- emails(ucat)[urls(ucat) == u]
                u.name <- names(ucat)[urls(ucat) == u]
                t2[[name.of.project3]] <- list()
                this.project <- t2[[name.of.project3]]
                expect_identical(names(members(this.project)),
                    my.name)
                members(this.project) <- u.email
                expect_true(setequal(names(members(this.project)),
                    c(u.name, my.name)))
            })
            try(crDELETE(self(refresh(t2)[[name.of.project3]])))
        })

        test_that("Can remove members from a project", {
            skip_on_jenkins("Jenkins user needs more permissions")
            t2 <- refresh(projects)
            name.of.project4 <- now()
            expect_false(name.of.project4 %in% names(t2))
            with(test.user(), {
                ucat <- getUserCatalog()
                u.email <- emails(ucat)[urls(ucat) == u]
                u.name <- names(ucat)[urls(ucat) == u]
                t2[[name.of.project4]] <- list()
                this.project <- t2[[name.of.project4]]
                expect_identical(names(members(this.project)),
                    my.name)
                members(this.project) <- u.email
                expect_true(setequal(names(members(this.project)),
                    c(u.name, my.name)))
                try(members(this.project)[[u.email]] <- NULL)
                expect_identical(names(members(this.project)),
                    my.name)
            })
            try(crDELETE(self(refresh(t2)[[name.of.project4]])))
        })

        test_that("Can add datasets to projects", {

        })
    })
}
