## THis file contains methods for all BED files

setClass("BEDFileResource", contains="AnnotationHubResource")

setMethod(".get1", "BEDFileResource",
    function(x, ..., force=FALSE)
{
    .require("rtracklayer")
    yy <- getHub(x)
    dat <- rtracklayer::BEDFile(cache(yy, force=force))
    gr <- rtracklayer::import(dat, format="bed", genome=yy$genome, ...)
    .tidyGRanges(x, gr)
})

setClass("UCSCBroadPeakResource", contains="BEDFileResource")

setMethod(".get1", "UCSCBroadPeakResource",
    function(x, ..., force=FALSE)
{
    broadPeaksmcols <- c(signalValue="numeric",
        pValue="numeric", qValue="numeric")
    callNextMethod(x, extraCols=broadPeaksmcols, force=force)
})

setClass("UCSCNarrowPeakResource", contains="BEDFileResource")

setMethod(".get1", "UCSCNarrowPeakResource",
    function(x, ..., force=FALSE)
{
    narrowPeaksmcols <- c(
        signalValue="numeric", pValue="numeric",
        qValue="numeric", peak="numeric")
    callNextMethod(x, extraCols=narrowPeaksmcols, force=force)
})

setClass("UCSCBEDRnaElementsResource", contains="BEDFileResource")

setMethod(".get1", "UCSCBEDRnaElementsResource",
    function(x, ..., force=FALSE)
{
    mcols <- c(name="character", score="integer",
        strand="character",
        level="numeric", signif="numeric", score2="numeric")
    callNextMethod(x, extraCols=mcols, force=force)
})

setClass("UCSCGappedPeakResource", contains="BEDFileResource")

setMethod(".get1", "UCSCGappedPeakResource",
    function(x, ..., force=FALSE)
{
    gappedPeakmcols <- c(signalValue="numeric",
        pValue="numeric", qValue="numeric")
    callNextMethod(x, extraCols=gappedPeakmcols, force=force)
})
