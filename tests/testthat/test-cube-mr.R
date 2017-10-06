context("Multiple response cube specialness")

alldims <- list(
    pdl_gender=c("Male", "Female"),
    attitudes_recoded_klima_2=c("Climate change is the biggest threat to civilisation",
        "Electric cars are the future of the motor industry",
        "I always make an effort to recycle",
        "I always make sure I turn lights off when I leave a room",
        "I don't care what my carbon footprint is",
        "I don't mind paying more for products that are good for the environment")
)

mr_x_cat_wt <- loadCube("cubes/selected-crosstab-4.json")

test_that("properties of a cube with for as_selected x cat: it looks 2D", {
    expect_equal(dim(mr_x_cat_wt), c(6L, 3L))
    expect_equal(names(dimnames(mr_x_cat_wt)),
        c("attitudes_recoded_klima_2", "pdl_gender"))
    expect_equal(aliases(variables(mr_x_cat_wt)),
        c("attitudes_recoded_klima_2", "pdl_gender"))
    expect_equal(names(Categories(data=index(variables(mr_x_cat_wt))[[2]]$categories)),
        c("Male", "Female", "No Data"))
    skip("TODO: variables should return a catalog you can [[ from; also categories() method should check the tuple before loading entity")
    expect_equal(names(categories(variables(mr_x_cat_wt)[[2]])),
        c("Male", "Female", "No Data"))
})

test_that("as.array on cube with as_selected x cat", {
    expect_equal(as.array(mr_x_cat_wt),
        cubify(
                9928.20954289002, 11524.821237084192,
                9588.843313998908, 9801.254016136965,
                11697.435357575358, 13095.670425525452,
                9782.8995547749, 10531.918128023966,
                4417.596222134318, 3448.380316269752,
                6179.175512581436, 6490.427474934746,
            dims=alldims[c("attitudes_recoded_klima_2", "pdl_gender")]))
})

test_that("margin.table for as_selected x cat", {
    expect_equal(margin.table(mr_x_cat_wt, 1),
        cubify(
                21453.03077997421,
                19390.097330135875,
                24793.105783100807,
                20314.817682798865,
                7865.976538404069,
                12669.602987516182,
            dims=alldims["attitudes_recoded_klima_2"]))
    expect_equal(margin.table(mr_x_cat_wt, 2),
        cubify(14566.261567907562, 15607.301233922663,
                 14456.513325488017, 15450.609903833058,
                 14415.136475733132, 15405.898678070093,
                 11485.661204663904, 11912.588886491172,
                 11664.69933815247, 12110.196347286023,
                 11547.413553551738, 11961.575582997419,
            dims=alldims[c("attitudes_recoded_klima_2", "pdl_gender")]))
    expect_error(margin.table(mr_x_cat_wt, 3),
        "Margin 3 exceeds Cube's number of dimensions (2)", fixed=TRUE)
    ## NULL margin is the same shape as margin 1 here but is sum of selected + not
    ## Note: not based on expectation from JS tests
    expect_equal(margin.table(mr_x_cat_wt),
        cubify(
                30173.5628018302,
                29907.1232293211,
                29821.0351538032,
                23398.2500911551,
                23774.8956854385,
                23508.9891365492,
            dims=alldims["attitudes_recoded_klima_2"]))
})

test_that("prop.table(row) for as_selected x cat", {
    expect_equal(prop.table(mr_x_cat_wt, 1),
        cubify(
                0.4627882020361299, 0.5372117979638701,
                0.4945227014975337, 0.5054772985024663,
                0.47180193800279874, 0.5281980619972013,
                0.481564723224583, 0.5184352767754171,
                0.5616081106479636, 0.4383918893520365,
                0.48771658580541166, 0.5122834141945883,
            dims=alldims[c("attitudes_recoded_klima_2", "pdl_gender")]))
})
test_that("prop.table(col) for as_selected x cat", {
    expect_equal(prop.table(mr_x_cat_wt, 2),
        cubify(
                0.6815894041587091, 0.7384249886863752,
                0.6632887957217867, 0.6343603312193796,
                0.8114689290154947, 0.8500426167391849,
                0.8517489224566737, 0.8840998567462627,
                0.3787149667617584, 0.28475015741941767,
                0.535113381358101, 0.5426064007955989,
            dims=alldims[c("attitudes_recoded_klima_2", "pdl_gender")]))
})
test_that("prop.table(cell) for as_selected x cat", {
    ## Note: not based on expectation from JS tests
    expect_equal(prop.table(mr_x_cat_wt),
        cubify(
                0.329036700375595, 0.381950958618156,
                0.320620717695708,  0.327723062528721,
                0.392254504152701, 0.439142047148397,
                0.418103897371069,  0.450115632023491,
                0.185809278853744, 0.14504292098248,
                0.262843097025161,  0.27608279697761,
            dims=alldims[c("attitudes_recoded_klima_2", "pdl_gender")]))
})

test_that("as_selected_margins adds margins in the right cases", {
    selecteds <- c(FALSE, TRUE, FALSE)
    expect_equal(as_selected_margins(1, selecteds), 1)
    expect_equal(as_selected_margins(2, selecteds), c(1, 3))
    expect_equal(as_selected_margins(NULL, selecteds), 1)
    nothing_selected <- c(FALSE, FALSE)
    expect_equal(as_selected_margins(1, nothing_selected), 1)
    expect_equal(as_selected_margins(2, nothing_selected), 2)
    expect_equal(as_selected_margins(NULL, nothing_selected), NULL)
})

test_that("as_selected_margins with before=FALSE", {
    selecteds <- c(FALSE, TRUE, FALSE)
    expect_equal(as_selected_margins(1, selecteds, before=FALSE), 1)
    expect_equal(as_selected_margins(2, selecteds, before=FALSE), c(1, 2))
    expect_equal(as_selected_margins(NULL, selecteds, before=FALSE), 1)
    nothing_selected <- c(FALSE, FALSE)
    expect_equal(as_selected_margins(1, nothing_selected, before=FALSE), 1)
    expect_equal(as_selected_margins(2, nothing_selected, before=FALSE), 2)
    expect_equal(as_selected_margins(NULL, nothing_selected, before=FALSE), NULL)
})

with_test_authentication({
    ds <- newDatasetFromFixture("apidocs")
    test_that("as_selected in cube query", {
        cub <- as.array(crtabs(~ as_selected(allpets), data=ds))
        expect_identical(dimnames(cub),
            list(
                allpets=c("Cat", "Dog", "Bird")
            ))
    })
})
