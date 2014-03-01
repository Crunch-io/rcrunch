context("Mock!")

with(fake.HTTP, {
    ds <- as.dataset(GET("datasets/dataset1.json"))
    expect_true(is.dataset(ds))
    expect_true(is.Categorical(ds$gender))
    expect_true(is.Numeric(ds$birthyr))
})