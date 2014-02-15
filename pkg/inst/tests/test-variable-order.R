context("Variable grouping and order setting")

test_that("VariableGroup and Grouping objects can be made", {
    expect_true(inherits(VariableGroup(group="group1", entities=""),
        "VariableGroup"))
    expect_true(inherits(VariableGroup(name="group1", entities=""),
        "VariableGroup"))
    vg1 <- VariableGroup(name="group1", entities="")
    expect_true(inherits(VariableGrouping(list(vg1)), "VariableGrouping"))
    expect_true(inherits(VariableGrouping(list(list(name="group1",
        entities=""))), "VariableGrouping"))
})

skip({
if (!run.only.local.tests) {
    with(test.authentication, {
        with(test.dataset(df), {
            ds <- .setup
            # print(getVariableOrder(ds))
            # print(VariableGrouping(list(list(name="ungrouped", 
            # entities=vapply(ds, function (x) self(x), character(1), USE.NAMES=FALSE)))))
            test_that("Can get VariableGrouping from dataset", {
                expect_identical(getVariableOrder(ds), 
                    VariableGrouping(list(list(name="ungrouped", 
                    entities=vapply(ds, function (x) self(x), character(1), USE.NAMES=FALSE)))))
            })
            test_that("Can make VariableGroup(ing) from Variables", {
                expect_identical(getVariableOrder(ds)[[1]], 
                    VariableGrouping(name="ungrouped", entities=ds[]))
                expect_identical(getVariableOrder(ds)[[1]], 
                    VariableGrouping(name="ungrouped", entities=ds))
            })
        })
    })
}
})