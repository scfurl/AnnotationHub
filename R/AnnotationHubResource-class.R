### =========================================================================
### AnnotationHubResource objects
### -------------------------------------------------------------------------
###

setClass("AnnotationHubResource", representation(hub="Hub"))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###

setMethod("hubCache", "AnnotationHubResource",
    function(x) hubCache(x@hub)
)

setMethod("hubUrl", "AnnotationHubResource",
    function(x) hubUrl(x@hub)
)

setMethod("getHub", "AnnotationHubResource",
    function(x) x@hub
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
###  Show
###

setMethod("show", "AnnotationHubResource",
    function(object)
{
    cat("class:", class(object), "\n")
})

setGeneric(".get1", function(x, ..., force=FALSE) {
    stopifnot(is(x, "AnnotationHubResource"), length(x) == 1L)
    standardGeneric(".get1")
})

setMethod(".get1", "AnnotationHubResource",
    function(x, ..., force=FALSE)
{
    msg <- sprintf("no '.get1' method defined for object
        of class %s, consider defining your own.",
        sQuote(class(x)))
    stop(paste(strwrap(msg), collapse="\n"))
})

##
## implementations
##

## FaFile

setClass("FaFileResource", contains="AnnotationHubResource")

setMethod(".get1", "FaFileResource",
    function(x, ..., force=FALSE)
{
    .require("Rsamtools")
    fa <- cache(getHub(x), force=force)
    Rsamtools::FaFile(file=fa[1],index=fa[2])
})

## Rds / RDS

## Michael's AHCytoData is the only package (I think) that uses RDS.
## Added Rds to be compatible with Rda naming scheme.
setClass("RdsResource", contains="AnnotationHubResource")
setMethod(".get1", "RdsResource", function(x, ..., force=FALSE) readRDS(cache(getHub(x), force=force)))

setClass("RDSResource", contains="RdsResource")
setMethod(".get1", "RDSResource", function(x, ..., force=FALSE) callNextMethod(x, force=force, ...))

## Rda

setClass("RdaResource", contains="AnnotationHubResource")
setMethod(".get1", "RdaResource",
    function(x, ..., force=FALSE)
{
    get(load(cache(getHub(x), force=force)))
})

setClass("data.frameResource", contains="RdaResource")

setClass("GRangesResource", contains="RdaResource")
setMethod(".get1", "GRangesResource",
    function(x, ..., force=FALSE)
{
    .require("GenomicRanges")
    gr <- callNextMethod(x, force=force, ...)
    .tidyGRanges(x, gr)
})

setClass("VCFResource", contains="RdaResource")
setMethod(".get1", "VCFResource",
    function(x, ..., force=FALSE)
{
    .require("VariantAnnotation")
    callNextMethod(x, force=force, ...)
})

## UCSC chain file
setClass("ChainFileResource", contains="AnnotationHubResource")

## trace(AnnotationHub:::.get1, tracer=browser, signature ="ChainFileResource")
setMethod(".get1", "ChainFileResource",
    function(x, ..., force=FALSE)
{
    .require("rtracklayer")
    .require("GenomeInfoDb")
    chain <- cache(getHub(x), force=force)
    tf <- .gunzip(chain, tempfile())
    tf <- rtracklayer::import.chain(tf)
    tf[GenomeInfoDb::sortSeqlevels(names(tf))]
})

setClass("TwoBitFileResource", contains="AnnotationHubResource")

setMethod(".get1", "TwoBitFileResource",
    function(x, ..., force=FALSE)
{
    .require("rtracklayer")
    bit <- cache(getHub(x), force=force)
    rtracklayer::TwoBitFile(bit)
})

setClass("GTFFileResource", contains="AnnotationHubResource")

setMethod(".get1", "GTFFileResource",
    function(x, ..., force=FALSE)
{
    message("Importing File into R ..")
    .require("rtracklayer")
    .require("GenomeInfoDb")
    yy <- getHub(x)
    gtf <- rtracklayer::import(cache(yy, force=force), format="gtf", genome=yy$genome, ...)
    .tidyGRanges(x, gtf)
})

setClass("GFF3FileResource", contains="AnnotationHubResource")

setMethod(".get1", "GFF3FileResource",
    function(x, ..., force=FALSE)
{
    .require("rtracklayer")
    yy <- getHub(x)
    rtracklayer::import(cache(yy, force=force), format="GFF", ...)
})

setClass("BigWigFileResource", contains="AnnotationHubResource")

setMethod(".get1", "BigWigFileResource",
    function(x, ..., force=FALSE)
{
    .require("rtracklayer")
    er <- cache(getHub(x), force=force)
    rtracklayer::BigWigFile(er)
})

setClass("dbSNPVCFFileResource", contains="AnnotationHubResource")

setMethod(".get1", "dbSNPVCFFileResource",
    function(x, ..., force=FALSE)
{
    .require("VariantAnnotation")
    withCallingHandlers({
        ## retrieve the resource
        er <- cache(getHub(x), force=force)
    }, warning=function(w) {
        if (grepl("^Failed to parse headers:", conditionMessage(w))[1])
            ## warning() something different, or...
            invokeRestart("muffleWarning")
    })
    VariantAnnotation::VcfFile(file=er[1],index=er[2])
})
## SQLiteFile

setClass("SQLiteFileResource", contains="AnnotationHubResource")

setMethod(".get1", "SQLiteFileResource",
    function(x, ..., force=FALSE)
{
    AnnotationDbi::loadDb(cache(getHub(x), force=force))
})

## GRASP2 SQLiteFile

setClass("GRASPResource", contains="SQLiteFileResource")

setMethod(".get1", "GRASPResource",
    function(x, ..., force=FALSE)
{
    RSQLite::dbConnect(RSQLite::SQLite(), cache(getHub(x), force=force),
        flags=RSQLite::SQLITE_RO)
})

setClass("ZipResource", contains="AnnotationHubResource")

setMethod(".get1", "ZipResource",
    function(x, filenames, ..., force=FALSE)
{
    zip <- cache(getHub(x), force=force)
    for (fl in filenames)
        unzip(zip, fl, exdir=tempdir())
    file.path(tempdir(), filenames)
})

setClass("ChEAResource", contains="ZipResource")

setMethod(".get1", "ChEAResource",
    function(x, ..., force=FALSE)
{
    fl <- callNextMethod(x, filenames="chea-background.csv", force=force)
    read.csv(fl, header=FALSE, stringsAsFactors=FALSE,
        col.names=c("s.no","TranscriptionFactor", "TranscriptionFactor-PubmedID",
        "TranscriptionFactorTarget", "PubmedID", "Experiment", "CellType",
        "Species","DateAdded"))
})

setClass("BioPaxResource", contains="RdaResource")

setMethod(".get1", "BioPaxResource",
    function(x, ..., force=FALSE)
{
    .require("rBiopaxParser")
    callNextMethod(x, ..., force=force)
})

setClass("PazarResource", contains="AnnotationHubResource")

setMethod(".get1", "PazarResource",
    function(x, ..., force=FALSE)
{
    .require("GenomicRanges")
    er <- cache(getHub(x), force=force)
    colClasses <-
        setNames(c(rep("character", 6), rep("integer", 2),
                   rep("factor", 2), "character", "NULL"),
                 c("PazarTFID","EnsemblTFAccession", "TFName",
                   "PazarGeneID", "EnsemblGeneAccession", "Chr", "GeneStart",
                   "GeneEnd", "Species", "ProjectName","PMID",
                   "AnalysisMethod"))
    dat <- read.delim(er, header=FALSE, col.names=names(colClasses),
                      na.strings="-", colClasses=colClasses)
    if (!anyNA(dat[["GeneStart"]])) {
        dat <- GenomicRanges::makeGRangesFromDataFrame(dat,
                                                       keep.extra.columns=TRUE)
        dat <- .tidyGRanges(x, dat)
    }
    dat
})


setClass("CSVtoGrangesResource", contains="AnnotationHubResource")

setMethod(".get1", "CSVtoGrangesResource",
   function(x, ..., force=FALSE)
{
    .require("GenomicRanges")
    yy <- getHub(x)
    dat <- read.csv(cache(yy, force=force), header=TRUE, stringsAsFactors=FALSE)
    dat <- dat[,!(names(dat) %in% "width")]
    gr <- GenomicRanges::makeGRangesFromDataFrame(dat, keep.extra.columns=TRUE)
    .tidyGRanges(x, gr)
})

setClass("ExpressionSetResource", contains="RdaResource")

setMethod(".get1", "ExpressionSetResource",
    function(x, ..., force=FALSE)
{
    .require("Biobase")
    callNextMethod(x, ..., force=force)
})

# GDS
setClass("GDSResource", contains="AnnotationHubResource")

setMethod(".get1", "GDSResource",
    function(x, ..., force=FALSE)
{
    .require("gdsfmt")
    yy <- cache(getHub(x), force=force)
    dat <- gdsfmt::openfn.gds(yy)
})

## H5FileResource
setClass("H5FileResource", contains = "AnnotationHubResource")

setMethod(".get1", "H5FileResource",
    function(x, ..., force=FALSE)
{
    .require("rhdf5")
    cache(getHub(x), force=force)
})
