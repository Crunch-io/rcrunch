# Even though this is an integration test it goes outside opf
# with_mock_crunch because that function is called in gadgets/makeArrayGadget/app.R
test_that("makeArrayGadget passes shiny tests", {
    cat(getwd())
    skip_on_cran()
    shinytest::expect_pass(
        shinytest::testApp("gadgets/makeArrayGadget", compareImages = FALSE))
})