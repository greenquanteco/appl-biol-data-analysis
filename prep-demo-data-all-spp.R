# install.packages("ritis")
library(ritis)

# data directory on onedrive
data.dir <- "C:/Users/ngreen62/OneDrive - Kennesaw State University/biol 7410/data"


## Unzipped the resulting download into 
top.dir <- "K:/_neon/NEON_count-small-mammals"

# need to download all sampling occasions as "expanded",
# to get vouchers

folds <- dir(top.dir)
folds <- folds[grep("RELEASE-20", folds)]
n.folds <- length(folds)

# which columns do we want from basic dataset?
use.cols1 <- c("domainID", "siteID", "plotID",
               "trapCoordinate",
               "decimalLatitude",
               "decimalLongitude", "elevation",
               "nlcdClass", "collectDate", "tagID",
               "scientificName", "sex", "lifeStage",
               "pregnancyStatus", "testes",
               "totalLength", "tailLength",
               "hindfootLength", "earLength",
               "weight", "tickNumber")  

# mlist for measured specimens
# spplist for species at each location
#   (includes extra species not in basic dataset)
spplist <- mlist <- vector("list", length=n.folds)
i <- 1


for(i in 1:n.folds){
    ifold <- paste(top.dir, folds[i], sep="/")
    ifile <- dir(ifold)
    
    # basic file--has individual captures
    ifile <- ifile[grep("_pertrapnight", ifile)]
    ix <- read.csv(paste(ifold, ifile, sep="/"))
    
    # only need captures
    ix <- ix[which(ix$trapStatus == "5 - capture"),]
    ispp <- character(0)
    
    # get basic captures, if any
    if(nrow(ix) > 0){
        ix <- ix[,use.cols1]
        mlist[[i]] <- ix
        # vector of species
        ispp <- sort(unique(ix$scientificName))
    }
    
    # get vouchers, if any
    # voucher file doesn't have any measurement data,
    # so just inspect to get unique species
    vfile <- dir(ifold)
    vfile <- vfile[grep("_voucher", vfile)]
    if(length(vfile) > 0){
        vx <- read.csv(paste(ifold, vfile, sep="/"))
        if(nrow(vx) > 0){
            # unique species in vouchers ## will have to remove nonmammals
            vspp <- sort(unique(vx$scientificName))  
            if(length(vspp) > 0){
                # combine with species in basic data
                ispp <- sort(union(ispp, vspp))  
            }#if any vspp
        }# if any rows in vx
        rm(vx)
    }#if vfile exists
    
    # save species to species list
    spplist[[i]] <- ispp
    
    rm(ix)
    rm(ispp)
    
    cat("folder", i, "of", n.folds, "done!\n")
    flush.console()
}

# compile measurement data
res <- do.call(rbind, mlist)
nrow(res)

# how many blank tags?
flag <- which(res$tagID == "") 
length(flag)
res[flag,]
table(res$siteID[which(res$scientificName=="Dipodomys spectabilis")])
# retain that individual for species richness purposes; only time
# this species caught at stie ONAQ
# make a fake tag number
length(which(res$flag=="NEON.MAM.D15.999999"))
res$tagID[flag] <- "NEON.MAM.D15.999999"

## "NEON.MAM" same for every tag, carries no information
utags <- sort(unique(res$tagID))
ntags <- length(utags)
tag.names <- strsplit(utags, "[.]")
table(sapply(tag.names, "[", 1))
table(sapply(tag.names, "[", 2))
length(which(is.na(sapply(tag.names, "[", 1))))

res$tagID <- gsub("NEON.MAM.", "", res$tagID)
nrow(res)

# do all tag IDs include domain?
## yes
table(sapply(strsplit(res$tagID, "[.]"), "[", 1))
sum(table(sapply(strsplit(res$tagID, "[.]"), "[", 1)))==nrow(res)

# tag parts
tagparts <- strsplit(res$tagID, "[.]")
res$utag <- paste(sapply(tagparts, "[", 1),
                  res$siteID,
                  sapply(tagparts, "[", 2),
                  sep="_")

# order by tag, then date
res <- res[order(res$utag, res$collectDate),]


# how many tags occur at >1 site?
tagdf <- aggregate(siteID~utag, data=res, function(x){length(unique(x))})
nrow(tagdf)
# none!
table(tagdf$siteID)
rev(sort(table(res$scientificName)))
rev(sort(table(res$tickNumber)))

# fix ticks
# assume blank is 0, because otherwise would have been
# recorded as "no count performed
res$tickNumber[which(res$tickNumber == "")] <- 0
res$tickNumber[which(res$tickNumber == "No count performed")] <- NA
rev(sort(table(res$tickNumber)))

## first filter down to species, which will
## reduce missing measurement problems
matrix(sort(unique(res$scientificName)))

use.spp <- c(
    "Alexandromys oeconomus",
    "Ammospermophilus harrisii",
    "Ammospermophilus leucurus",
    "Baiomys taylori",
    "Blarina brevicauda",
    "Blarina carolinensis",
    "Blarina hylophaga",
    "Callospermophilus lateralis",
    "Chaetodipus baileyi",
    "Chaetodipus californicus",
    "Chaetodipus eremicus",
    "Chaetodipus hispidus",
    "Chaetodipus intermedius",
    "Chaetodipus penicillatus",
    "Condylura cristata",
    "Cryptotis parva",
    "Dicrostonyx groenlandicus",
    "Didelphis virginiana",
    "Dipodomys heermanni",
    "Dipodomys merriami",
    "Dipodomys microps",
    "Dipodomys ordii",
    "Dipodomys spectabilis",
    "Glaucomys sabrinus",
    "Glaucomys volans",
    "Ictidomys tridecemlineatus",
    "Ictidomys tridecemlineatus monticola",
    "Lemmiscus curtatus",
    "Lemmus trimucronatus",
    "Lepus americanus",
    "Lepus californicus",
    "Microtus californicus",
    "Microtus longicaudus",
    "Microtus miurus",
    "Microtus montanus",
    "Microtus ochrogaster",
    "Microtus oregoni",
    "Microtus pennsylvanicus",
    "Microtus pinetorum",
    "Microtus xanthognathus",
    "Mus musculus",
    "Myodes gapperi",
    "Myodes rutilus",
    "Napaeozapus insignis",
    "Neotoma albigula",
    "Neotoma cinerea",
    "Neotoma devia",
    "Neotoma floridana",
    "Neotoma lepida",
    "Neotoma macrotis",
    "Neotoma mexicana",
    "Neotoma micropus",
    "Neurotrichus gibbsii",
    "Ochotona princeps",
    "Ochrotomys nuttalli",
    "Onychomys arenicola",
    "Onychomys leucogaster",
    "Onychomys torridus",
    "Oryzomys palustris",
    "Perognathus amplus",
    "Perognathus fasciatus",
    "Perognathus flavescens",
    "Perognathus flavus",
    "Perognathus inornatus",
    "Perognathus longimembris",
    "Perognathus parvus",
    "Peromyscus attwateri",
    "Peromyscus boylii",
    "Peromyscus californicus",
    "Peromyscus crinitus",
    "Peromyscus eremicus",
    "Peromyscus gossypinus",
    "Peromyscus keeni",
    "Peromyscus leucopus",
    "Peromyscus maniculatus",
    "Peromyscus merriami",
    "Peromyscus nasutus",
    "Peromyscus polionotus",
    "Peromyscus truei",
    "Phenacomys intermedius",
    "Podomys floridanus",
    "Rattus norvegicus",
    "Rattus rattus",
    "Reithrodontomys fulvescens",
    "Reithrodontomys humulis",
    "Reithrodontomys megalotis",
    "Reithrodontomys montanus",
    "Sciurus carolinensis",
    "Sigmodon arizonae",
    "Sigmodon hispidus",
    "Sigmodon hispidus eremicus",
    "Sigmodon ochrognathus",
    "Sorex arcticus",
    "Sorex cinereus",
    "Sorex dispar",
    "Sorex fumeus",
    "Sorex haydeni",
    "Sorex hoyi",
    "Sorex longirostris",
    "Sorex merriami",
    "Sorex monticolus",
    "Sorex nanus",
    "Sorex ornatus",
    "Sorex palustris",
    "Sorex preblei",
    "Sorex trowbridgii",
    "Sorex tundrensis",
    "Sorex ugyunak",
    "Sorex vagrans",
    "Spermophilus armatus",
    "Spermophilus beecheyi",
    "Spermophilus franklinii",
    "Spermophilus parryii",
    "Spermophilus richardsonii",
    "Spermophilus spilosoma",
    "Spermophilus tereticaudus",
    "Spermophilus variegatus",
    "Sylvilagus audubonii",
    "Sylvilagus floridanus",
    "Sylvilagus nuttallii",
    "Synaptomys cooperi",
    "Tamias alpinus",
    "Tamias amoenus",
    "Tamias dorsalis",
    "Tamias merriami",
    "Tamias minimus",
    "Tamias quadrimaculatus",
    "Tamias quadrivittatus",
    "Tamias rufus",
    "Tamias speciosus",
    "Tamias striatus",
    "Tamias townsendii",
    "Tamias umbrinus",
    "Tamiasciurus douglasii",
    "Tamiasciurus hudsonicus",
    "Thomomys talpoides",
    "Zapus hudsonius",
    "Zapus princeps",
    "Zapus trinotatus"
)

res <- res[which(res$scientificName %in% use.spp),]
rev(sort(table(res$scientificName)))
length(use.spp)

# only want adults, juvenile, subadult
# lose 7 no data, 5062 unknown
sort(table(res$lifeStage))
use.stages <- c("juvenile", "subadult", "adult")
res <- res[which(res$lifeStage %in% use.stages),]
sort(table(res$lifeStage))

# fix sex
sort(table(res$sex))
res$sex[which(res$sex == "U")] <- "unknown"
res$sex[which(res$sex == "F")] <- "female"
res$sex[which(res$sex == "FALSE")] <- "female"
res$sex[which(res$sex == "")] <- "unknown"
res$sex[which(res$sex == "M")] <- "male"
sort(table(res$sex))

# find individuals with >1 sex
fr.sex <- ftable(sex~utag, data=res)
fr.sex <- as.data.frame(as.matrix(fr.sex))
fr.sex$check <- apply(fr.sex, 1, function(x){length(which(x>0))})
table(fr.sex$check)
fr.sex[fr.sex$check==3,]
fr.sex[fr.sex$check==2,]

# male and female is a problem. 
# (male and unknown) and (female and unknown) is resolvable
fr.sex$everfemale <- as.character(1*(fr.sex$female>0))
fr.sex$evermale <- as.character(1*(fr.sex$male>0))
fr.sex$everunknown <- as.character(1*(fr.sex$unknown>0))
fr.sex$checkstring <- do.call(paste0, fr.sex[,5:7])
fr.sex$tag <- rownames(fr.sex)
res$sexcheck <- fr.sex$checkstring[match(res$utag, fr.sex$tag)]
ft.sexcheck <- ftable(sex~sexcheck, data=res)
ft.sexcheck

ftable(pregnancyStatus~sexcheck, data=res)

ftable(testes~sexcheck, data=res)

ftable(testes~pregnancyStatus, data=res)

sort(table(res$testes))

# combine sex and reproductive indicators
rc <- fr.sex[,c("tag", "checkstring")]
rownames(rc) <- NULL
all(rc$tag %in% res$utag)
all(res$utag %in% rc$tag)

prr <- tapply(res$pregnancyStatus, res$utag, function(x){any(x=="pregnant")})
rc$everpreg <- as.numeric(1*prr[rc$tag])

tes <- tapply(res$testes, res$utag, function(x){any(x=="scrotal")})
rc$everscrot <- as.numeric(1*tes[rc$tag])

length(which(is.na(rc$everpreg)))
length(which(is.na(rc$everscrot)))
# flag NA with 9
rc$everpreg[which(is.na(rc$everpreg))] <- 9
rc$everscrot[which(is.na(rc$everscrot))] <- 9
table(rc$everpreg)
table(rc$everscrot)
# should both be 0
length(which(is.na(rc$everpreg)))
length(which(is.na(rc$everscrot)))
# combine to single string
rc$code5 <- do.call(paste0, rc[,-1])
matrix(sort(unique(rc$code5)),ncol=1)
## first 3 are sex
## 4 is ever pregnant
## 5 is ever scrotal
# "00100" = unknown
# "00101" = male
# "00109" = unknown
# "00190" = unknown
# "00199" = unknown
# "01000" = male
# "01001" = male
# "01090" = male
# "01091" = male
# "01100" = male
# "01101" = male
# "10000" = female
# "10009" = female
# "10010" = female
# "10019" = female
# "10090" = female
# "10100" = female
# "10110" = female
# "11000" = unknown
# "11001" = male
# "11010" = female
# "11011" = unknown
# "11100" = unknown
# "11101" = male
# "11110" = female
# "11111" = unknown

# Lookup table for inferred sex
sex.lookup <- c(
    "00100" = "unknown",
    "00101" = "male",
    "00109" = "unknown",
    "00190" = "unknown",
    "00199" = "unknown",
    "01000" = "male",
    "01001" = "male",
    "01090" = "male",
    "01091" = "male",
    "01100" = "male",
    "01101" = "male",
    "10000" = "female",
    "10009" = "female",
    "10010" = "female",
    "10019" = "female",
    "10090" = "female",
    "10100" = "female",
    "10110" = "female",
    "11000" = "unknown",
    "11001" = "male",
    "11010" = "female",
    "11011" = "unknown",
    "11100" = "unknown",
    "11101" = "male",
    "11110" = "female",
    "11111" = "unknown"
)

length(sex.lookup)
length(unique(rc$code5))
setdiff(names(sex.lookup), unique(rc$code5))

# Assign inferred sex
rc$checksex <- unname(sex.lookup[as.character(rc$code5)])

# each column should have 1 entry per row at most
ftable(checksex~code5, data=rc)

# put inferred sex into res to compare
res$checksex <- rc$checksex[match(res$utag, rc$tag)]
res$code5 <- rc$code5[match(res$utag, rc$tag)]

# and compare
## no direct contradictions
ftable(checksex~sex, data=res)
#         checksex female  male unknown
# sex                                  
# female            63919     0    2323
# male                  0 71788    1795
# unknown             325   341    4622

# quick checks
res$testes[which(res$testes=="")] <- "unknown"
res$pregnancyStatus[which(res$pregnancyStatus=="")] <- "unknown"

table(res$pregnancyStatus)
table(res$testes)
table(res$sex)

# looks like trouble, but probably means that
# they had contradictory field reproductive statuses
ftable(checksex~pregnancyStatus, data=res)
ftable(checksex~testes, data=res)

# Individuals recorded as both pregnant and scrotal
both <- rc$tag[
    substr(rc$code5, 4, 4) == "1" &
        substr(rc$code5, 5, 5) == "1"
]

length(both)


# Examine all capture records for these individuals
res.both <- subset(res, utag %in% both)[
    , c("utag", "sex", "pregnancyStatus", "testes", "checksex", "code5")
]
nrow(res.both)
table(res.both$code5)

# shows that the 77 individuals assigned 
# male and female at different times
# caused the seeming contradiction above
res.check <- res[-which(res$utag %in% both),]
ftable(checksex~pregnancyStatus, data=res.check)
ftable(checksex~testes, data=res.check)

table(res$testes)
table(res$pregnancyStatus)

# Identify individuals with contradictory reproductive evidence
bad.tags <- rc$tag[rc$code5 %in% c("11011", "11111")]

# Flag all records from these individuals
res$donotuse <- res$utag %in% bad.tags

# Flag contradictory individuals in both reproductive variables
res$pregnancyStatus[res$donotuse] <- "donotuse"
res$testes[res$donotuse] <- "donotuse"

# Assign reproductive status based on inferred sex
res$pregnancyStatus[
    !res$donotuse &
        res$checksex == "male" &
        res$pregnancyStatus == "unknown"
] <- "nonpregnant"

res$testes[
    !res$donotuse &
        res$checksex == "female" &
        res$testes == "unknown"
] <- "nonscrotal"

table(res$testes)
table(res$pregnancyStatus)
res$pregnancyStatus[which(res$pregnancyStatus=="not")] <- "nonpregnant"
table(res$pregnancyStatus)

# everything is consistent
ftable(checksex~pregnancyStatus, data=res)
ftable(checksex~testes, data=res)

res$donotuse <- NULL

# produce two datasets: capture level, and individual level
## capture level has individuals with species, sex, tagID,
# mass, and reproductive status

## individual level has species, tagID, sex, and all measurements
## excludes pregnant individuals

ncaps <- nrow(res)
cap.cols <- c("domainID", "siteID", "plotID", "trapCoordinate",
              "collectDate", "utag", "lifeStage",
              "pregnancyStatus", "testes", "tickNumber",
              "weight")
cap <- res[,cap.cols]

ind.cols <- c("utag", "scientificName", "checksex", 
              "totalLength", "tailLength", "hindfootLength",
              "earLength", "weight")
ind <- res[,ind.cols]

##################################################

# rebuild taxa data table
tax <- data.frame(dbin=sort(unique(ind$scientificName)))
tax$dgen <- sapply(strsplit(tax$dbin, " "), "[", 1)
tax$dspp <- sapply(strsplit(tax$dbin, " "), "[", 2)
tax$tsn <- NA
tax$valid <- NA
for(i in 1:nrow(tax)){
    ires <- search_scientific(tax$dbin[i])
    if(nrow(ires) == 0) next
    flag <- which(ires$combinedName == tax$dbin[i])
    itsn <- ires$tsn[flag]
    tax$tsn[i] <- itsn
    valid <- as.data.frame(usage(itsn))$taxonUsageRating[1]
    tax$valid[i] <- valid
    cat("taxon", i, "of", nrow(tax), "complete!\n")
    flush.console()
}
tax.start <- tax

tax <- tax.start

# genus to use for interpolation
# dgen is correct except for the species
# changed below
tax$igen <- tax$dgen

# fix the invalid names and missing TSN
which(is.na(tax$tsn))

# probably Microtus oeconomus; use ITIS's naming convention
tax$tsn[1] <- "180298"
tax$valid[1] <- "valid"
tax$igen[1] <- "Microtus"

tax[which(tax$valid == "invalid"),]

#4           Blarina brevicauda      Blarina   brevicauda 179967 invalid      Blarina
flag <- which(tax$dbin == "Blarina brevicauda")
tax$tsn[flag] <- "1229285"
tax$dspp[flag] <- "brevicaudus"


#15             Cryptotis parva    Cryptotis        parva 179971 invalid    Cryptotis
flag <- which(tax$dbin == "Cryptotis parva")
tax$tsn[flag] <- "1229257"
tax$dspp[flag] <- "parvus"

#85  Sigmodon hispidus eremicus     Sigmodon     hispidus 973436 invalid     Sigmodon
flag <- which(tax$dbin == "Sigmodon hispidus eremicus")
tax$tsn[flag] <- "180349"
tax$dspp[flag] <- "hispidus"

#103       Spermophilus armatus Spermophilus      armatus 180147 invalid Spermophilus
flag <- which(tax$dbin == "Spermophilus armatus")
tax$tsn[flag] <- "930314"
tax$igen[flag] <- "Spermophilus"

#104      Spermophilus beecheyi Spermophilus     beecheyi 180148 invalid Spermophilus
flag <- which(tax$dbin == "Spermophilus beecheyi")
tax$tsn[flag] <- "930301"
tax$igen[flag] <- "Spermophilus"

#105    Spermophilus franklinii Spermophilus   franklinii 180153 invalid Spermophilus
flag <- which(tax$dbin == "Spermophilus franklinii")
tax$tsn[flag] <- "930309"
tax$igen[flag] <- "Spermophilus"

#106       Spermophilus parryii Spermophilus      parryii 180146 invalid Spermophilus
flag <- which(tax$dbin == "Spermophilus parryii")
tax$tsn[flag] <- "930321"
tax$igen[flag] <- "Spermophilus"

#107  Spermophilus richardsonii Spermophilus richardsonii 180157 invalid Spermophilus
flag <- which(tax$dbin == "Spermophilus richardsonii")
tax$tsn[flag] <- "930322"
tax$igen[flag] <- "Spermophilus"

#108     Spermophilus spilosoma Spermophilus    spilosoma 180159 invalid Spermophilus
flag <- which(tax$dbin == "Spermophilus spilosoma")
tax$tsn[flag] <- "930312"
tax$igen[flag] <- "Spermophilus"

#109  Spermophilus tereticaudus Spermophilus tereticaudus 180160 invalid Spermophilus
flag <- which(tax$dbin == "Spermophilus tereticaudus")
tax$tsn[flag] <- "930313"
tax$igen[flag] <- "Spermophilus"

#110    Spermophilus variegatus Spermophilus   variegatus 180163 invalid Spermophilus
flag <- which(tax$dbin == "Spermophilus variegatus")
tax$tsn[flag] <- "930302"
tax$igen[flag] <- "Spermophilus"

## need to fix chipmunk taxonomy
# "Tamias" was recently split, not yet reflected in ITIS
tax$igen[which(tax$dgen == "Tamias")] <- "Neotamias"
tax$igen[which(tax$dbin == "Tamias striatus")] <- "Tamias"
# combine them all for morphological interpolation
tax$igen[which(tax$igen=="Neotamias")] <- "Tamias"
tax$igen[which(tax$igen=="Eutamias")] <- "Tamias"

# combine all the ground squirrels formerly in Spermophilus
# for measurement imputation only
tax$igen[which(tax$igen %in% c("Spermophilus", "Urocitellus", "Ictidomys",
                               "Otospermophilus", "Callospermophilus",
                               "Xerospermophilus", "Poliocitellus",
                               "Notocitellus"))] <- "Spermophilus"

# fill in other taxonomic levels
tax$order <- NA
tax$suborder <- NA
tax$superfamily <- NA
tax$family <- NA
tax$subfamily <- NA
tax$genus <- NA
tax$species <- NA

ntax <- nrow(tax)
for(i in 1:ntax){
    itsn <- tax$tsn[i]
    ih <- as.data.frame(hierarchy_full(itsn))
    tax$order[i] <- ih$taxonname[which(ih$rankname == "Order")]
    if("Suborder" %in% ih$rankname){
        tax$suborder[i] <- ih$taxonname[which(ih$rankname == "Suborder")]
    }
    if("Superfamily" %in% ih$rankname){
        tax$superfamily[i] <- ih$taxonname[which(ih$rankname == "Superfamily")]
    }
    tax$family[i] <- ih$taxonname[which(ih$rankname == "Family")]
    if("Subfamily" %in% ih$rankname){
        tax$subfamily[i] <- ih$taxonname[which(ih$rankname == "Subfamily")]
    }
    tax$genus[i] <- ih$taxonname[which(ih$rankname == "Genus")]
    tax$species[i] <- ih$taxonname[which(ih$rankname == "Species")]
    cat("taxon", i, "of", ntax, "complete!\n")
    flush.console()
}

# make abbreviations: first 3 of genus, then species, then numbers as needed
tax$spp <- sapply(strsplit(tax$species, " "), "[", 2)
tax$abb <- paste0(
    substr(tax$genus, 1, 3),
    substr(tax$spp, 1, 3)
)
abb.table <- table(tax$abb)
problem.abbs <- names(abb.table)[which(abb.table > 1)]
tax[which(tax$abb %in% problem.abbs),]

# Ictidomys tridecemlineatus and Ictidomys tridecemlineatus monticola are okay

# Perognathus flavescens and Perognathus flavus need fixing to 1 and 2
tax$abb[which(tax$dbin == "Perognathus flavescens")] <- "Perfla1"
tax$abb[which(tax$dbin == "Perognathus flavus")] <- "Perfla2"

# Sigmodon hispidus and Sigmodon hispidus eremicus are okay

# Tamias quadrimaculatus and Tamias quadrivittatus need fixing to 1 and 2
tax$abb[which(tax$dbin == "Tamias quadrimaculatus")] <- "Tamqua1"
tax$abb[which(tax$dbin == "Tamias quadrivittatus")] <- "Tamqua2"

# reclassify Alexandromys oeconomus as Microtus oeconomus
taxcheck <- unique(tax[,c("dbin", "species", "abb")])

# put abb in ind
ind$abb <- tax$abb[match(ind$scientificName, tax$dbin)]

# put valid name in ind
ind$species <- tax$species[match(ind$abb, tax$abb)]

###############

# mean measurements for each individual
mag <- aggregate.data.frame(ind[,meas.cols],
                            by=list(ind$utag),
                            mean, na.rm=TRUE)
names(mag)[1] <- "utag"
mag[2:6] <- lapply(mag[2:6], function(x){replace(x, is.nan(x), NA)})
head(mag)

# replace 0s with NA
summary(mag[,meas.cols])
mag$weight[which(mag$weight == 0)] <- NA

# Add species and genus from the individual data
mag$abb <- ind$abb[match(mag$utag, ind$utag)]
mag$igen <- ind$igen[match(mag$utag, ind$utag)]

# Measurement variables
meas.cols <- c("hindfootLength", "earLength", "tailLength",
               "totalLength", "weight")

# Count missing measurements
mag$missing <- rowSums(is.na(mag[, meas.cols]))

head(mag)
table(mag$missing)

# Preserve original measurements
mag.imp <- mag

# Track what was imputed and which group supplied the model
mag.imp$imputedVariable <- rep(NA_character_, nrow(mag.imp))
mag.imp$imputationLevel <- rep(NA_character_, nrow(mag.imp))

# Original complete observations for training
train <- mag[mag$missing == 0, ]

for(v in meas.cols) {
    
    predictors <- setdiff(meas.cols, v)
    
    form <- reformulate(
        paste0("log(", predictors, ")"),
        response = paste0("log(", v, ")")
    )
    
    # Individuals missing only this measurement
    targets <- which(mag$missing == 1 & is.na(mag[[v]]))
    
    for (sp in unique(mag$abb[targets])) {
        
        rows <- targets[which(mag$abb[targets] == sp)]
        dx <- train[which(train$abb == sp), ]
        
        if (nrow(dx) < 20){
            next
        } else{
            fit <- lm(form, data = dx)
            mag.imp[[v]][rows] <- exp(predict(fit, newdata = mag[rows, ]))
            mag.imp$imputedVariable[rows] <- v
            mag.imp$imputationLevel[rows] <- "species"
        }
    }
}#v



# Number of successful imputations
table(mag.imp$imputationLevel, useNA = "ifany")

# Which measurements were imputed?
table(mag.imp$imputedVariable, useNA = "ifany")

table(mag$missing)

# Missing measurements before and after
table(
    before = mag$missing,
    after = rowSums(is.na(mag.imp[, meas.cols]))
)

# Identify imputed values that are zero or negative
for (v in meas.cols) {
    bad <- which(mag.imp$imputedVariable == v &
                     !is.na(mag.imp$imputedVariable) &
                     mag.imp[[v]] <= 0)
    
    if (length(bad) > 0) {
        cat(v, ":", length(bad), "nonpositive predictions\n")
    }
}

####################################################

# do another pass at genus level

# Original complete observations for training
train <- mag[which(mag$missing == 0), ]

for (v in meas.cols) {
    
    predictors <- setdiff(meas.cols, v)
    
    form <- reformulate(
        paste0("log(", predictors, ")"),
        response = paste0("log(", v, ")")
    )
    
    # Individuals still missing exactly one measurement
    targets <- which(mag$missing == 1 &
                         is.na(mag.imp[[v]]))
    
    for (g in unique(na.omit(mag$igen[targets]))) {
        
        rows <- targets[which(mag$igen[targets] == g)]
        dx <- train[which(train$igen == g), ]
        
        # Require at least 20 complete individuals
        if (nrow(dx) < 20) next
        
        fit <- lm(form, data = dx)
        
        # Impute using genus-level model
        mag.imp[[v]][rows] <- exp(predict(fit, newdata = mag[rows, ]))
        
        mag.imp$imputedVariable[rows] <- v
        mag.imp$imputationLevel[rows] <- "genus"
    }
}

# Number imputed at each level
table(mag.imp$imputationLevel, useNA = "ifany")

# Imputations by measurement and level
table(mag.imp$imputedVariable,
      mag.imp$imputationLevel)

# Remaining individuals missing exactly one measurement
sum(mag$missing == 1 &
        rowSums(is.na(mag.imp[, meas.cols])) == 1)

# how many do we have?
mag.imp$include <- rowSums(is.na(mag[, meas.cols]))
inc <- as.data.frame(as.matrix(ftable(include~abb, data=mag.imp)))
inc[order(-inc$`5`),]
tax$species[which(tax$abb %in% rownames(inc)[which(inc$`5` > 0)])]
tax$species[which(tax$abb %in% rownames(inc)[which(inc$`5` > 10)])]

inc.orig <- as.data.frame(as.matrix(ftable(missing~abb, data=mag.imp)))
inc.orig[order(-inc.orig$`0`),]
tax$species[which(tax$abb %in% rownames(inc.orig)[which(inc.orig$`0` > 0)])]
tax$species[which(tax$abb %in% rownames(inc)[which(inc.orig$`0` > 10)])]


### at this point mag.imp contains the full imputed morphology dataset
out.mag.cols <- c("utag", "hindfootLength", "earLength", "tailLength", "totalLength", 
                  "weight", "imputedVariable", "imputationLevel")
out.mag <- mag.imp[,out.mag.cols]

### make output individual level dataset
out.ind.cols <- c("utag", "abb", "checksex", 
                  "totalLength", "tailLength", 
                  "hindfootLength", "earLength",
                  "weight")
out.ind <- ind[,out.ind.cols]

### make new taxonomic lookup table
out.tax.cols <- c("abb", "species", "tsn", 
                  "order", "suborder", 
                  "superfamily", "family", "subfamily",
                  "genus")
out.tax <- tax[,out.tax.cols]

### cap is already complete

### make richness at the site and grid level
sites <- sort(unique(cap$siteID))
nsite <- length(sites)
richs <- data.frame(site=sites)

taxa <- sort(unique(out.tax$abb))
ntaxa <- length(taxa)
capmat <- matrix(0, nrow=nsite, ncol=ntaxa)
colnames(capmat) <- taxa
for(i in 1:nsite){
    # flag site
    flag <- which(cap$siteID == sites[i])
    # get tag IDs of each capture
    site.tags <- cap$utag[flag]
    # get species of each capture
    cap.spp <- out.ind$abb[match(site.tags, out.ind$utag)]
    # tally up captures
    cap.tab <- table(cap.spp)
    # put in capture matrix
    capmat[i,names(cap.tab)] <- cap.tab
}

### make grid-level capture matrix
grids <- sort(unique(cap$plotID))
ngrid <- length(grids)
richs.g <- data.frame(grid=grids)
gridmat <- matrix(0, nrow=nrow(richs.g), ncol=ntaxa)
colnames(gridmat) <- taxa
for(i in 1:ngrid){
    # flag site
    flag <- which(cap$plotID == grids[i])
    # get tag IDs of each capture
    grid.tags <- cap$utag[flag]
    # get species of each capture
    cap.spp <- out.ind$abb[match(grid.tags, out.ind$utag)]
    # tally up captures
    cap.tab <- table(cap.spp)
    # put in capture matrix
    gridmat[i,names(cap.tab)] <- cap.tab    
}

# combine with site and grid names
out.site <- cbind(richs, as.data.frame(capmat))
out.grid <- cbind(richs.g, as.data.frame(gridmat))

# write out new datasets

out.name <- "data-neon-taxa.csv"
out.path <- paste("datasets", out.name, sep="/")
write.csv(out.tax, out.path)

out.name <- "data-neon-captures-grid.csv"
out.path <- paste("datasets", out.name, sep="/")
write.csv(out.grid, out.path)

out.name <- "data-neon-captures-site.csv"
out.path <- paste("datasets", out.name, sep="/")
write.csv(out.site, out.path)

out.name <- "data-neon-animals.csv"
out.path <- paste("datasets", out.name, sep="/")
names(out.ind)[1] <- "tagID"
names(out.ind)[3] <- "sex"
write.csv(out.ind, out.path)

out.name <- "data-neon-capture-events.csv"
out.path <- paste("datasets", out.name, sep="/")
names(cap)[6] <- "tagID"
names(cap)[2:3] <- c("site", "grid")
write.csv(cap, out.path)

# unique grids
g <- read.csv("datasets/neon-grids.csv")
h <- read.csv("datasets/dat-neon-site-rich-env.csv")

h$domain <- g$domain[match(h$site , g$site)]

out.h <- h[,c("domain", "site", "habitat", 
          "lat", "lon", "elev", 
          "precip", "mat", "npp",
          "seas.pre", "seas.temp")]
names(out.h)[8] <- "temp"
out.name <- "data-neon-metadata-site.csv"
out.path <- paste("datasets", out.name, sep="/")
write.csv(out.h, out.path)

out.g <- g[,c("grid", "site", "lat", "lon", "elev", "nlcd")]
out.name <- "data-neon-metadata-grid.csv"
out.path <- paste("datasets", out.name, sep="/")
write.csv(out.g, out.path)

# now we have:
## "data-neon-capture-events.csv": capture events
## "data-neon-captures-grid.csv":  # of captures of each taxon in each grid
## "data-neon-captures-site.csv":  # of captures of each taxon at each site
## "data-neon-metadata-grid.csv":  metadata on grids
## "data-neon-metadata-site.csv":  metadata on sites
## "data-neon-animals.csv":       measurements on individuals
## "data-neon-taxa.csv":           metadata on species

# name intersections
outputs <- c("cap", "out.g", "out.h",
             "out.ind", "out.site", "out.grid", "out.tax")
names(outputs) <- c("capture-events", "metadata-grid", "metadata-site",
                    "animals", "captures-site", "captures-grid",
                    "taxa")

outpairs <- combn(names(outputs), 2, simplify = FALSE)

for (p in outpairs) {
    common <- intersect(
        names(get(outputs[p[1]])),
        names(get(outputs[p[2]]))
    )
    
    cat("\n", paste(p, collapse = " & "), ":\n")
    print(common)
}

# end script!