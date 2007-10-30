sha1 <- function(object, skip = 0L) {
	## Setting 'skip = 14' gives us the same results as
	## 'digest(object, "sha1")'	
	bytes <- serialize(object, NULL)
	.Call("sha1_object", bytes, as.integer(skip))
}
