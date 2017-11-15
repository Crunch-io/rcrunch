context("z-scores")

##########################################
## fixutres from crunch cube
##########################################
mr_x_cat <- loadCube(test_path("cubes/mr-by-cat-profiles-stats-weighted.json"))
mr_x_cat_unweighted <- loadCube(test_path("cubes/mr-by-cat-profiles-stats.json"))
mr_x_cat_dims <- list(pdl_gender = c("maennlich", "weiblich"),
                      pdl_holidays_taken_12_months =
                          c("ein Mal", "zwei Mal", "drei Mal", "vier Mal",
                            "fuenf Mal oder mehr", "kein Mal - ich war in den letzten zwoelf Monaten nicht im Urlaub",
                            "weiss nicht", "keine Angabe"))

gender_x_ideology <- loadCube(test_path("cubes/econ-gender-x-ideology-weighted.json"))

test_that("z-scores are calculated by default with the deprecated method)", {
    expect_equal(z.table(gender_x_ideology, 1), zScoresDep(gender_x_ideology, 1))
    expect_equal(z.table(gender_x_ideology, 2), zScoresDep(gender_x_ideology, 2))
})

test_that("z-scores can calculate to row margins - crunch-cube fixture (axis 1)", {
    # values from crunch-cube tests
    out <- cubify(c(-0.6950547, -0.524633, -1.454419, 1.0896893, 3.2737502, -2.0051536,
                    0.7375224, 0.556688, 1.5432837, -1.1562691, -3.4737757, 2.1276681),
                  dims = list(Gender = c("Male", "Female"),
                                  RespondentIdeology =
                                      c("Very liberal", "Liberal", "Moderate",
                                        "Conservative", "Very Conservative",
                                        "Not sure")))
    expect_equal(zScoresDep(gender_x_ideology, 1), out)
})

test_that("z-scores can calculate to column margins - crunch-cube fixture (axis 0)", {
    # values from crunch-cube tests
    out <- cubify(c(-0.7316249, -0.5418986, -1.4985995, 1.1212007, 3.4848471, -2.265434,
                    0.7316249, 0.5418986, 1.4985995, -1.1212007, -3.4848471, 2.265434),
                 dims = list(Gender = c("Male", "Female"),
                                 RespondentIdeology =
                                     c("Very liberal", "Liberal", "Moderate",
                                       "Conservative", "Very Conservative",
                                       "Not sure")))
    expect_equal(zScoresDep(gender_x_ideology, 2), out)
})


test_that("z-scores can calculate to row margins - crunch cube fixture (axis 1)", {
    out <- cubify(c(-1.35700098973668, 3.39819222765456, 3.47632774910236,
                    1.39986424142017, 2.33910237706402, -6.92590429515317,
                    -0.237453687452224, 0.736452470486666, 1.3528312160513,
                    -3.38775031004662, -3.4656457377556, -1.39556275813377,
                    -2.33191481595459, 6.90462247318263, 0.236724043078395,
                    -0.734189509622821), dims = mr_x_cat_dims)
    expect_equivalent(zScoresDep(mr_x_cat, 1), out)
})

test_that("p-values can calculate to columns margins - crunch cube fixture (axis 0)", {
    out <- cubify(c(-1.33325107235154, 3.4170985193131, 3.56231261682056,
                      1.42672343792323, 2.41444184160409, -6.85140362038577,
                      -0.220890470186746, 0.722988145330955,
                      1.33325107235152, -3.41709851931311, -3.56231261682057,
                      -1.42672343792324, -2.41444184160409, 6.85140362038576,
                      0.220890470186742, -0.72298814533095),
                  dims = mr_x_cat_dims)
    expect_equivalent(zScoresDep(mr_x_cat, 2), out)
})

test_that("p-values can calculate to row margins - crunch-cube fixture (axis 1)", {
    out <- cubify(c(-1.74780896e-01, 6.78327392e-04, 5.08330650e-04,
                    1.61553976e-01, 1.93301357e-02,  -4.33209024e-12,
                    -8.12304844e-01, 4.61455383e-01,
                    1.76109558e-01, -7.04683839e-04, -5.28959758e-04,
                    -1.62846204e-01, -1.97051729e-02, 5.03375119e-12,
                    8.12870881e-01, -4.62833246e-01),
                  dims = mr_x_cat_dims)
    expect_equivalent(p.values(mr_x_cat, 1), abs(out))
})

test_that("z-scores can calculate to column margins - crunch-cube fixture (axis 0)", {
    out <- cubify(c(-1.82449424e-01, 6.32923709e-04, 3.67602277e-04,
                    1.53659627e-01, 1.57593387e-02, -7.31281702e-12,
                    -8.25177718e-01, 4.69687167e-01,
                    1.82449424e-01, -6.32923709e-04, -3.67602277e-04,
                    -1.53659627e-01, -1.57593387e-02, 7.31281702e-12,
                    8.25177718e-01, -4.69687167e-01), dims = mr_x_cat_dims)
    expect_equivalent(p.values(mr_x_cat, 2), abs(out))
})



##########################################
## fixutres from whaam
##########################################
# whaam fixtures
admit_by_dept_unweighted <- loadCube(test_path("cubes/admit-by-dept-unweighted.json"))
admit_by_gender_weighted <- loadCube(test_path("cubes/admit-by-gender-weighted.json"))

test_that("z-scores can calculate to row margins - whaam fixture", {
    out <- cubify(c(17.3006739150679, 12.1555052876046, -2.61883165552036,
                    -3.12585957287982, -7.73178794867428, -23.9433203846143,
                    -17.2790610621901, -12.1403200324679, 2.6155600821955,
                     3.12195459533981, 7.72212901884083, 23.9134092110139),
                 dims = list(Admit = c("Admitted", "Rejected"),
                                 Dept = c("A", "B", "C", "D", "E", "F")))
    expect_equal(zScoresDep(admit_by_dept_unweighted, 1), out)
})

test_that("z-scores can calculate to row margins - whaam fixture", {
    out <- cubify(c(9.80281743121017, -9.80281743121017,
                    -9.71107624617507, 9.71107624617506),
                 dims = list(Admit = c("Admitted", "Rejected"),
                                 Gender = c("Male", "Female")))
    expect_equal(zScoresDep(admit_by_gender_weighted, 1), out)
})

test_that("z-scores can calculate to column margins - whaam fixture", {
    out <- cubify(c(18.7216214725448, 13.3291986335621, -2.67980030430232,
                    -3.19261047229265, -8.09694682104735, -32.0139892315214,
                    -18.7216214725448, -13.3291986335621, 2.67980030430231,
                    3.19261047229265, 8.09694682104735, 32.0139892315214),
                 dims = list(Admit = c("Admitted", "Rejected"),
                                 Dept = c("A", "B", "C", "D", "E", "F")))
    expect_equal(zScoresDep(admit_by_dept_unweighted, 2), out)
})

test_that("z-scores can calculate to column margins - whaam fixture", {
    out <- cubify(c(9.75089877074671, -9.72361434000118,
                    -9.75089877074672, 9.72361434000117),
                 dims = list(Admit = c("Admitted", "Rejected"),
                                 Gender = c("Male", "Female")))
    expect_equal(zScoresDep(admit_by_gender_weighted, 2), out)
})

## p-values unweighted
test_that("z-scores can calculate to row margins (unweighted) - whaam fixture (pvalues calc)", {
    out <- cubify(c(-0.013867151154616897, 0.0009563833962253909,
                  0.0019487904594506222, 0.12924606532399752,
                  0.21918270942711926, -0.0000019841338991799518,
                  -0.7213747305278584, 0.7405824255456315,
                  0.013871483015614139, -0.0009568966033475235,
                  -0.0019497184647323529, -0.12926350905959105,
                  -0.21920369941309148, 0.0000019862619564658957,
                  0.7213868916373223, -0.7405938153040896),
                  dims = mr_x_cat_dims)
    expect_equal(p.values(mr_x_cat_unweighted, 1), abs(out))
})

test_that("z-scores can calculate to columns margins (unweighted) - whaam fixture (pvalues calc)", {
    out <- cubify(c(-0.013861902224157063, 0.0009542912810769355,
                    0.0019299337340947798, 0.12876189755225131,
                    0.21852890322144702, -0.0000019638715096359505,
                    -0.7195376396263351, 0.7396284032210236,
                    0.013861902224157951, -0.0009542912810769355,
                    -0.0019299337340947798, -0.12876189755224998,
                    -0.21852890322144547, 0.0000019638715096359505,
                    0.7195376396263378, -0.7396284032210221),
                  dims = mr_x_cat_dims)
    expect_equivalent(p.values(mr_x_cat_unweighted, 2), abs(out))
})

## p-values weighted
test_that("z-scores can calculate to row margins - whaam fixture (pvalues calc)", {
    out <- cubify(c(-0.17478090412248748, 0.0006783274445936183,
                      0.00050833069186762, 0.16155398180455305,
                      0.019330134162242496, -4.332090242087361e-12,
                      -0.812304843861103, 0.46145538258239,
                      # js pnorm is bad, and shows up in differences at 8 digits
                      # the values below are from whaam for posterity
                      # -0.8123048793015557, 0.46145535855338116,
                    0.17610956603574945, -0.0007046838930806487,
                      -0.0005289598016751107, -0.16284620991880927,
                      -0.019705171316439563, 5.03375119365046e-12,
                      0.812870881353903, -0.462833246125983
                      # js pnorm is bad, and shows up in differences at 8 digits
                      # the values below are from whaam for posterity
                      # 0.812870916762612, -0.4628332216372071
                      ), dims = mr_x_cat_dims)
    expect_equivalent(p.values(mr_x_cat, 1), abs(out))
})

test_that("z-scores can calculate to columns margins - whaam fixture (pvalues calc)", {
    out <- cubify(c(-0.1824494335182465, 0.0006329237585269709,
                    0.0003676023086403024, 0.15365963167452468,
                    0.015759337556626285, -7.312817018600981e-12,
                    -0.825177717845072, 0.469687167401825,
                    # js pnorm is bad, and shows up in differences at 8 digits
                    # the values below are from whaam for posterity
                    # -0.8251777514259946, 0.4696871406486176,
                    0.18244943351825338, -0.0006329237585269709,
                    -0.0003676023086403024, -0.15365963167452334,
                    -0.015759337556626285, 7.312817018600981e-12,
                    0.825177717845075, -0.469687167401828
                    # js pnorm is bad, and shows up in differences at 8 digits
                    # the values below are from whaam for posterity
                    #0.8251777514259975, -0.46968714064862027
                    ), dims = mr_x_cat_dims)
    expect_equivalent(p.values(mr_x_cat, 2), abs(out))
})
