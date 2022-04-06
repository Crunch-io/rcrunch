crunch::set_crunch_opts(
    "crunch.api" = crunch:::get_crunch_opt("old.crunch.api"),
    "old.crunch.api" = crunch:::get_crunch_opt("crunch.api")
)

crunch_test_teardown_check()
