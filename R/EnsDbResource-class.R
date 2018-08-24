### =========================================================================
### EnsDb objects
### -------------------------------------------------------------------------
###

setClass("EnsDbResource", contains="AnnotationHubResource")

setMethod(".get1", "EnsDbResource",
    function(x, ..., force=FALSE)
{
    .require("ensembldb")
    ensembldb::EnsDb(cache(getHub(x), force=force))
})
