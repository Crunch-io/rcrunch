# table <- function (..., exclude = if (useNA == "no") c(NA, NaN), 
#                     useNA = c("no", "ifany", "always"), dnn = list.names(...),
#                     deparse.level = 1) {
#     if (is.variable(..1)) {
#         return(CategoricalVariable.table(...))
#     } else {
#         m <- match.call(expand.dots=FALSE)
#         m[[1L]] <- as.name("table")
#         return(eval(m, parent.frame(), enclos=baseenv()))
#     }   
# }
NULL