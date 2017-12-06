context("Variable transformations")

insrts_list <- list(list(anchor = 6, name = "Low",
                         `function` = "subtotal", args = c(1, 2)),
                    list(anchor = 7, name = "High",
                         `function` = "subtotal", args = c(9, 10)))
insrts <- Insertions(data=insrts_list)

test_that("Can make a transforms object from Insertions or lists", {
    expect_true(is.Insertions(insrts))
    expect_true(is.list(insrts_list) & !is.Insertions(insrts_list))
    trans <- Transforms(insertions = insrts)

    expect_equal(trans, Transforms(insertions = insrts_list))
    expect_equal(trans[["insertions"]], insrts)
})

test_that("Transforms validation", {
    expect_error(Transforms(foo = 'bar'),
                 paste("Transforms must have at least one of",
                       serialPaste(dQuote(c("insertions", "categories", "elements")), "or")))
    expect_error(Transforms(insertions = "foo"),
                 paste0("invalid class ", dQuote("Insertions"),
                        " object: Invalid insertions: 1 element is not a Crunch Insertion object\\."))
    expect_error(Transforms(categories = "foo"),
                 paste0("invalid class ", dQuote("Categories"),
                        " object: Invalid categories: 1 element is not a Crunch category object\\."))
})

cats_list <- mapply(function (i, n, m) list(id = i, name = n, missing = m),
                    i = c(1:10), n = LETTERS[c(1:10)],
                    m = c(rep(FALSE, 6), TRUE, TRUE, FALSE, FALSE),
                    SIMPLIFY = FALSE)

cats <- Categories(data = cats_list)

insrts_list <- list(list(anchor = 0, name = "First one",
                         `function` = "subtotal", args = c(3, 4)),
                    list(anchor = 999, name = "Last one",
                         `function` = "subtotal", args = c(5, 6)),
                    list(anchor = 10, name = "High",
                         `function` = "subtotal", args = c(9, 10)),
                    list(anchor = 2, name = "Low",
                         `function` = "subtotal", args = c(1, 2)),
                    list(anchor = 8, name = "missing anchor",
                         `function` = "subtotal", args = c(2, 3)),
                    list(anchor = 4, name = "missing categories",
                         `function` = "subtotal", args = c(7, 8)))
insrts <- Insertions(data=insrts_list)

test_that("findInsertPosition", {
    expect_equal(findInsertPosition(insrts[["First one"]], cats), 0)
    expect_equal(findInsertPosition(insrts[["Last one"]], cats), Inf)
    expect_equal(findInsertPosition(insrts[["High"]], cats), 10)
    expect_equal(findInsertPosition(insrts[["Low"]], cats), 2)
    expect_equal(findInsertPosition(insrts[["missing anchor"]], cats), Inf)
    expect_equal(findInsertPosition(insrts[["missing categories"]], cats), 4)
})


test_that("collateCats places at beginning", {
    new_cats <- collateCats(insrts["First one"], cats)
    expect_equivalent(new_cats[c(2:11)], cats)
    expect_equivalent(new_cats[1], insrts["First one"])
})

test_that("collateCats places at end", {
    new_cats <- collateCats(insrts["Last one"], cats)
    expect_equivalent(new_cats[c(1:10)], cats)
    expect_equivalent(new_cats[11], insrts["Last one"])
})

test_that("collateCats places at end if the id is improbably high", {
    new_cats <- collateCats(insrts["High"], cats)
    expect_equivalent(new_cats[c(1:10)], cats)
    expect_equivalent(new_cats[11], insrts["High"])
})

test_that("collateCats places after index 2", {
    new_cats <- collateCats(insrts["Low"], cats)
    expect_equivalent(new_cats[c(1,2,4:11)], cats)
    expect_equivalent(new_cats[3], insrts["Low"])
})

test_that("collateCats places a missing anchor at end", {
    new_cats <- collateCats(insrts["missing anchor"], cats)
    expect_equivalent(new_cats[c(1:10)], cats)
    expect_equivalent(new_cats[11], insrts["missing anchor"])
})

test_that("collateCats places at an anchor even if the combo categories are na", {
    new_cats <- collateCats(insrts["missing categories"], cats)
    expect_equivalent(new_cats[c(1:4,6:11)], cats)
    expect_equivalent(new_cats[5], insrts["missing categories"])
})

test_that("collateCats works all together", {
    new_cats <- collateCats(insrts, cats)
    expect_length(new_cats, 16)
    expect_equivalent(new_cats[c(2, 3, 5, 6, 8, 9, 10, 11, 12, 13)], cats)
    expect_equivalent(new_cats[c(1, 15, 14, 4, 16, 7)], insrts)
    # indices for new_cats to name map
    # 1  - First one
    # 15 - Last one
    # 14 - High
    # 4  - Low
    # 16 - missing anchor
    # 7  - missing categories
})

insrt_heads <- Insertions(data=list(list(name = "Subtitle", anchor = 0)))
# turn into subclassed insertions

test_that("Converting insertions to subtypes works", {
    insrts_subtyped <- subtypeInsertions(insrts)
    insrt_heads_subtyped  <- subtypeInsertions(insrt_heads)
    
    # the lengths are the same, we aren't missing anything
    expect_equal(length(insrts), length(insrts_subtyped))
    expect_equal(length(insrt_heads), length(insrt_heads_subtyped))
    
    # the types are correct
    expect_true(is.Subtotal(insrts_subtyped[[1]]))
    expect_true(is.Heading(insrt_heads_subtyped[[1]]))
    expect_true(is.category(cats[[1]])) # we need cats for collation later
    
    # we can check the full object and get back a vector of logicals
    expect_true(all(is.Subtotals(insrts_subtyped)))
    expect_true(all(is.Headings(insrt_heads_subtyped)))
    expect_true(all(unlist(lapply(cats, is.category))))
    
    # we can re-subtype with no harm
    expect_true(all(is.Subtotals(subtypeInsertions(insrts_subtyped))))
    expect_true(all(is.Headings(subtypeInsertions(insrt_heads_subtyped))))

    # collate and check
    collated <- collateCats(c(insrts_subtyped, insrt_heads_subtyped), cats)
    expect_true(all(is.Subtotals(collated[c(2, 5, 8, 15, 16, 17)])))
    expect_true(all(is.Headings(collated[c(1)])))
    expect_true(all(unlist(lapply(collated[c(3, 4, 6, 7, 9, 10, 11, 12, 13, 14)],
                           is.category))))
    
    expect_error(subtypeInsertion("foo"), "Must provide an object of type Insertion")
    expect_error(subtypeInsertions("foo"), "Must provide an object of type Insertions")
    })

with_mock_crunch({
    ds <- loadDataset("test ds")

    test_that("Can get transform", {
        expect_equivalent(transforms(ds$location),
                     Transforms(insertions = list(
                         Subtotal(name = "London+Scotland", after = 3,
                              categories = c(1, 2))),
                                categories = NULL,
                                elements = NULL)
                     )

        loc_ary <- array(c(7, 10, 17), dim = 3,
                         dimnames = list(c("London", "Scotland",
                                           "London+Scotland")))
        expect_output(expect_equivalent(showTransforms(ds$location), loc_ary))
    })
    
    test_that("Can set transform (with Insertions)", {
        expect_null(transforms(ds$gender))
        trans_insert <- Transforms(insertions = Insertions(
            Insertion(anchor = 3, name = "Male+Female",
                      `function` = "subtotal", args = c(1, 2))))
        expect_PATCH(transforms(ds$gender) <- trans_insert,
            'https://app.crunch.io/api/datasets/1/variables/gender/',
            '{"element":"shoji:entity","body":{"view":{"transform":{',
            '"insertions":[{"anchor":3,"name":"Male+Female","function"',
            ':"subtotal","args":[1,2]}]}}}}')
    })

    test_that("Can set transform (with Subtotal)", {
        trans_insert <- Transforms(insertions = Insertions(
            Subtotal(after = 3, name = "Male+Female",
                      categories = c(1, 2))))
        expect_PATCH(transforms(ds$gender) <- trans_insert,
                     'https://app.crunch.io/api/datasets/1/variables/gender/',
                     '{"element":"shoji:entity","body":{"view":{"transform":{',
                     '"insertions":[{"anchor":3,"name":"Male+Female","function"',
                     ':"subtotal","args":[1,2]}]}}}}')
    })
    
    test_that("Can set transform (with Heading)", {
        trans_insert <- Transforms(insertions = Insertions(
            Heading(after = 3, name = "Male+Female")))
        expect_PATCH(transforms(ds$gender) <- trans_insert,
                     'https://app.crunch.io/api/datasets/1/variables/gender/',
                     '{"element":"shoji:entity","body":{"view":{"transform":{',
                     '"insertions":[{"anchor":3,"name":"Male+Female"}]}}}}')
    })
    
    
    
    test_that("Can delete transform", {
        expect_PATCH(transforms(ds$location) <- NULL,
            'https://app.crunch.io/api/datasets/1/variables/location/',
            '{"element":"shoji:entity","body":{"view":{"transform":{}}}}')
    })

    test_that("Non-combine insertions are ignored", {
        loc_var <- ds$location
        trns <- transforms(loc_var)
        trns[['insertions']][[1]] <- Insertion(name = "London+Scotland",
                                               anchor = 3, args = c(1,2),
                                               `function` = "foobar")
        loc_ary <- c(7, 10, NA)
        names(loc_ary) <- c("London", "Scotland", "London+Scotland")
        expect_warning(expect_equivalent(calcTransforms(table(loc_var), trns,
                                        categories(loc_var)[!is.na(categories(loc_var))]), loc_ary),
                       paste0("Transform functions other than subtotal are ",
                              "not supported. Applying only subtotals and ",
                              "ignoring foobar"))
    })

    test_that("Transform respects anchors", {
        loc_var <- ds$location
        trns <- transforms(loc_var)
        trns[['insertions']][[1]][['after']] <- 1

        loc_ary <- c(7, 17, 10, NA)
        names(loc_ary) <- c("London", "London+Scotland", "Scotland", "No Data")
        expect_equivalent(calcTransforms(table(loc_var), trns,
                                        categories(loc_var)),
                          loc_ary)
    })

    test_that("Transform works without a function (that is, with a heading)", {
        loc_var <- ds$location
        trns <- transforms(loc_var)
        trns[['insertions']][[1]] <- Heading(name = "London+Scotland", after = 3)

        loc_ary <- c(7, 10, NA, NA)
        names(loc_ary) <- c("London", "Scotland", "London+Scotland", "No Data")
        expect_equivalent(calcTransforms(table(loc_var), trns,
                                        categories(loc_var)),
                          loc_ary)
    })

    test_that("calcTransform rejects nd arrays", {
        ary2d <- array(c(1, 2, 3, 4, 5, 6), dim = c(2, 3),
                         dimnames = list("foo1" = c("bar", "baz"),
                                         "foo2" = c("bar", "baz", "qux")))

        expect_error(calcTransforms(ary2d, Transforms(insertions=insrts),
                                   Categories()),
                     paste0("Calculating varaible transforms is not ",
                            "implemented for dimensions greater than 1."))
    })
})

with_test_authentication({
    ds <- newDataset(df)

    test_that("Can get and set transforms", {
        trans <- Transforms(insertions = list(
            Subtotal(after = 3, name = "B+C",
                 categories = c(1, 2))))
        expect_null(transforms(ds$v4))
        transforms(ds$v4) <- trans
        trans_resp <- trans
        trans_resp["categories"] <- list(NULL)
        trans_resp["elements"] <- list(NULL)
        expect_json_equivalent(transforms(ds$v4), trans_resp)

        v4_ary <- array(c(10, 10, 20), dim = 3,
                        dimnames = list(c("B", "C", "B+C")))
        expect_output(expect_equivalent(showTransforms(ds$v4), v4_ary))
    })

    test_that("Can remove transforms", {
        transforms(ds$v4) <- NULL
        v4_notrans <- array(c(10, 10), dim = 2,
                            dimnames = list(c("B", "C")))

        expect_null(transforms(ds$v4))
        expect_output(expect_equivalent(showTransforms(ds$v4), v4_notrans))
    })
})
