context("Teams")

with(fake.HTTP, {
    test_that("Getting teams catalog", {
        teams <- try(getTeams())
        expect_true(inherits(teams, "TeamCatalog"))
        expect_identical(length(teams), 1L)
        expect_identical(names(teams), "Alpha Team")
    })
    
    test_that("Getting team entity", {
        teams <- try(getTeams())
        expect_true(inherits(teams[[1]], "CrunchTeam"))
        expect_true(inherits(teams$`Alpha Team`, "CrunchTeam"))
        expect_true(inherits(teams[["Alpha Team"]], "CrunchTeam"))
        expect_true(is.null(teams$`Beta Team`))
    })
    
    test_that("Team entity attributes", { 
        ateam <- try(getTeams()[[1]])
        expect_identical(name(ateam), "Alpha Team")
        m <- try(members(ateam))
        expect_true(inherits(m, "MemberCatalog"))
        expect_identical(names(m), c("Fake User", "Roger User"))
    })
})