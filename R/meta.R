tag <- function(type, expr, comment = NULL) {
	stopifnot(type %in% c("result", "data"))
	cachedir <- cache()
	eval(substitute(expr), parent.frame())
	setConfig("expr.tag", type)
}
