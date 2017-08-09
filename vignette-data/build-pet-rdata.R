set.seed(999)

pet_types <- c("Cat", "Dog", "Bird", "Lizard")
pet_weights <- c(0.35, 0.3, 0.2, 0.15)

constrained_replicate <- function(slots, reps, cats, weights) {
    replicate(reps, {
        ans <- sample(cats, slots, prob = weights)
        # NA some subset of the later answers.
        not_ans <- sample(c(1:length(ans)), 1, prob = c(0.1, 0.45, 0.45))
        if (not_ans < length(ans)) {
            ans[c({not_ans}:length(ans))] <- NA
        }
        ans
    })
}

pets <- constrained_replicate(3, 250, pet_types, pet_weights)
pets <- as.data.frame(t(pets))
names(pets) <- c("Pet1", "Pet2", "Pet3")

# add agreement
agreement <- c("Strongly Agree", "Agree", "Neither Agree Nor Disagree", "Disagree", "Strongly Disagree")
pets[!is.na(pets$Pet1), "Opinion1"]  <- factor(sample(agreement, nrow(pets[!is.na(pets$Pet1), ]), replace = TRUE))
pets[!is.na(pets$Pet2), "Opinion2"] <- factor(sample(agreement, nrow(pets[!is.na(pets$Pet2), ]), replace = TRUE))
pets[!is.na(pets$Pet3), "Opinion3"] <- factor(sample(agreement, nrow(pets[!is.na(pets$Pet3), ]), replace = TRUE))

# add Days_having_Pet
pets[!is.na(pets$Pet1), "Days_having_Pet1"]  <- sample(c(150:{365*5}), nrow(pets[!is.na(pets$Pet1), ]), replace = TRUE)
pets[!is.na(pets$Pet2), "Days_having_Pet2"] <- sample(c(150:{365*5}), nrow(pets[!is.na(pets$Pet2), ]), replace = TRUE)
pets[!is.na(pets$Pet3), "Days_having_Pet3"] <- sample(c(150:{365*5}), nrow(pets[!is.na(pets$Pet3), ]), replace = TRUE)

save(pets, file="../vignettes/pets.RData")
