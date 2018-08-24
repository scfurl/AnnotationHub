setClass("mzRpwizResource", contains="AnnotationHubResource")
setMethod(".get1", "mzRpwizResource",
    function(x, ..., force=FALSE)
{
    .require("mzR")
    yy <- cache(getHub(x), force=force)
    mzR::openMSfile(yy, backend = "Ramp")
})

setClass("mzRidentResource", contains="AnnotationHubResource")
setMethod(".get1", "mzRidentResource",
    function(x, ..., force=FALSE)
{
    .require("mzR")
    yy <- cache(getHub(x), force=force)
    mzR::openIDfile(yy)
})

setClass("MSnSetResource", contains="RdaResource")
setMethod(".get1", "MSnSetResource",
    function(x, ..., force=FALSE)
{
    .require("MSnbase")
    callNextMethod(x, ..., force=force)
})

setClass("AAStringSetResource", contains="AnnotationHubResource")
setMethod(".get1", "AAStringSetResource",
     function(x, ..., force=FALSE)
{
    .require("Biostrings")
    yy <- cache(getHub(x), force=force)
    Biostrings::readAAStringSet(yy)
})
