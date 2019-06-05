context("CrunchCube computation speed alarms")
# We don't need to stress test CRAN
skip_on_cran()

# These benchmark alarms are to check if we have introduced slow downs in cube
# processing

# load a multi dimensional cube so there are many cells to calculate
cube <- loadCube("cubes/cat-x-cat-x-cat.json")

# Make two subtotals for each dimension
transforms(cube) <- TransformsList(
    pasta = Transforms(
        insertions = Insertions(
            Subtotal(
                name = "long",
                categories = c("Bucatini", "Chitarra", "Fileja"),
                position = "top"
            ),
            Subtotal(
                name = "short",
                categories = c("Boccoli", "Orecchiette", "Quadrefiore"),
                position = "bottom"
            )
        )
    ),
    food_groups = Transforms(
        insertions = Insertions(
            Subtotal(
                name = "plant-based",
                categories = c("Vegetables", "Fruit", "Grain"),
                position = "top"
            ),
            Subtotal(
                name = "animal-based",
                categories = c("Meat"),
                position = "bottom"
            )
        )
    ),
    offal = Transforms(
        insertions = Insertions(
            Subtotal(
                name = "circulatory",
                categories = c("Heart", "Kidney", "Liver", "Thymus", "Pancreas"),
                position = "top"
            ),
            Subtotal(
                name = "non-circulatory",
                categories = c("Snout", "Lung", "Tongue"),
                position = "bottom"
            )
        )
    )
)

# run 10 iterations so that some small hiccup doesn't falsly alarm
with_trans <- sapply(seq_len(10), function(i) {
    system.time({
        applyTransforms(cube)
    })
})
without_trans <- sapply(seq_len(10), function(i) {
    system.time({
        applyTransforms(noTransforms(cube))
    })
})

# We test both the ratio of no transforms to transforms as well as the absolute
# processing time. If the ratio fails, this might be transient. If both the
# ratio and the absolute time fail there is probably a regression to cube
# computations
test_that("applyTransforms is no slower than 10 times with noTransforms", {
    expect_lt(median(with_trans["elapsed", ]) / median(without_trans["elapsed", ]), 10)
})

test_that("applying transforms takes no longer than 1 second", {
    expect_lt(median(with_trans["elapsed", ]), 1)
})

# TODO: add a cube with a large number of categories benchmark as well
