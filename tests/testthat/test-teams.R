context("Teams")

with_mock_crunch({
    teams <- getTeams()
    test_that("Getting teams catalog", {
        expect_is(teams, "TeamCatalog")
        expect_length(teams, 1)
        expect_identical(names(teams), "Alpha Team")
    })

    test_that("Getting team entity", {
        expect_is(teams[[1]], "CrunchTeam")
        expect_is(teams$`Alpha Team`, "CrunchTeam")
        expect_is(teams[["Alpha Team"]], "CrunchTeam")
        expect_null(teams$`Beta Team`)
    })

    ateam <- teams[[1]]
    test_that("Team entity attributes", {
        expect_is(ateam, "CrunchTeam")
        expect_identical(name(ateam), "Alpha Team")
    })
    test_that("Team members", {
        m <- members(ateam)
        expect_is(m, "MemberCatalog")
        expect_identical(names(m), c("Fake User", "Roger User"))
    })

    test_that("Team deletion", {
        expect_error(delete(ateam), "Must confirm")
        expect_DELETE(with_consent(delete(ateam)), self(ateam))
    })
})

with_test_authentication({
    ucat <- getAccountUserCatalog()
    my.name <- names(ucat)[urls(ucat) == userURL()]
    my.email <- emails(ucat)[urls(ucat) == userURL()]

    teams <- try(getTeams())
    nteams.0 <- length(teams)
    test_that("Can get team catalog", {
        expect_is(teams, "TeamCatalog")
    })

    t2 <- teams
    name.of.team1 <- now()
    test_that("Can create a team", {
        expect_false(name.of.team1 %in% names(t2))
        expect_warning(t2[[name.of.team1]] <- list(),
            "Teams are deprecated. Use projects instead.")
        expect_true(name.of.team1 %in% names(t2))
        expect_true(length(t2) == nteams.0 + 1L)
        expect_is(t2[[name.of.team1]], "CrunchTeam")
        expect_length(members(t2[[name.of.team1]]), 1)
        expect_identical(names(members(t2[[name.of.team1]])), my.name)
    })

    test_that("delete method for team (requires confirmation)", {
        ## Setup
        t2 <- refresh(t2)
        nteams.2 <- length(t2)
        name.of.team2 <- now()
        expect_false(name.of.team2 %in% names(t2))
        expect_warning(t2[[name.of.team2]] <- list(),
            "Teams are deprecated. Use projects instead.")
        expect_true(name.of.team2 %in% names(t2))
        expect_true(length(t2) == nteams.2 + 1L)

        expect_error(delete(t2[[name.of.team2]]),
            "Must confirm deleting team")
        expect_true(name.of.team2 %in% names(t2))
        expect_true(length(t2) == nteams.2 + 1L)

        ## Cleanup
        with_consent(delete(t2[[name.of.team2]]))
        expect_false(name.of.team2 %in% names(getTeams()))
    })

    test_that("Can create a team with members", {
        skip_on_jenkins("Jenkins user needs more permissions")
        t2 <- refresh(t2)
        nteams.2 <- length(t2)
        name.of.team2 <- now()
        expect_false(name.of.team2 %in% names(t2))
        u <- testUser()
        expect_warning(t2[[name.of.team2]] <- list(members=email(u)),
            "Teams are deprecated. Use projects instead.")
        expect_true(name.of.team2 %in% names(t2))
        expect_true(length(t2) == nteams.2 + 1L)
        this.team <- t2[[name.of.team2]]
        expect_true(setequal(names(members(this.team)), c(name(u), my.name)))
    })

    test_that("Can add members to a team", {
        skip_on_jenkins("Jenkins user needs more permissions")
        t2 <- refresh(teams)
        name.of.team3 <- now()
        expect_false(name.of.team3 %in% names(t2))
        u <- testUser()
        expect_warning(t2[[name.of.team3]] <- list(),
            "Teams are deprecated. Use projects instead.")
        this.team <- t2[[name.of.team3]]
        expect_identical(names(members(this.team)), my.name)
        members(this.team) <- email(u)
        expect_true(setequal(names(members(this.team)), c(name(u), my.name)))
    })

    test_that("Can remove members from a team", {
        skip_on_jenkins("Jenkins user needs more permissions")
        t2 <- refresh(teams)
        name.of.team4 <- now()
        expect_false(name.of.team4 %in% names(t2))
        u <- testUser()
        expect_warning(t2[[name.of.team4]] <- list(),
            "Teams are deprecated. Use projects instead.")
        this.team <- t2[[name.of.team4]]
        expect_identical(names(members(this.team)), my.name)
        members(this.team) <- email(u)
        expect_true(setequal(names(members(this.team)), c(name(u), my.name)))
        try(members(this.team)[[email(u)]] <- NULL)
        expect_identical(names(members(this.team)), my.name)
    })
})
