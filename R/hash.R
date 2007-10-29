sha1 <- function(object) {
	r <- serialize(object, NULL)
	.Call("hash_object", r)
}
