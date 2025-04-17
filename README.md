# MIDSCalculator

This is the repository for the MIDSCalculator app, written initially by @LynnDelgat during an internship at Meise Botanic Garden in 2022. The MIDSCalculator is an Rshiny app that allows users to easily calculate MIDS scores for their natural history specimen datasets. The app currently supports data provided as Darwin Core Archives and Biocase ABCD archives.

## Minimum Information about a Digital Specimen

Minimum Information about a Digital Specimen (MIDS) is a data standard which aims to describe four different levels of digitization status for biological and geological specimens from natural history collections, each level requiring certain information elements (referred to as MIDS elements from here on) to be digitally available. With this standard, users of collection data, as well as funders and collection curators will have a better overview of how comprehensively digitized a collection or any other set of specimens really is. The standard is currently still under development by a Task Group of the global Biodiversity Information Standards (“TDWG”) organization. While MIDS elements for digitization levels 0 and 1 have been specified, MIDS elements for levels 2 and 3 are still under discussion.
For more details you can visit the GitHub repository https://github.com/tdwg/mids or the documentation website (still in draft stage) https://tdwg.github.io/mids.

## MIDSCalculator App
The MIDSCalculator is a Shiny app written in R, which allows users to calculate MIDS scores for each record in a submitted dataset, and explore the results.

* [How to install](/help/howtoinstall.md)
* [Using the app](/help/howtouse.md)
* [Additional info about the code](/help/codeinfo.md)
* [Create a new installer](/help/rinno_installer.md).

## Mapping MIDS elements: use of a JSON schema

As the MIDS standard is agnostic to the data model of the sourced data, the app uses [SSSOM mappings](https://github.com/mapping-commons/SSSOM) to map MIDS elements to properties of the sourced data. SSSOM mappings for Darwin Core archives and for Biocase ABCD archives are [included](/data/sssom) in the app. Latest versions of SSSOM mappings can be found in the [MIDS GitHub repository](https://github.com/tdwg/mids/tree/main/source/mappings) and more documentation can be viewed on the [MIDS documentation website](https://tdwg.github.io/mids/mappings/).

## Data
### Datasets
* Example datasets for testing, GBIF Occurrence Downloads [10.15468/dl.e8jnan](http://doi.org/10.15468/dl.e8jnan) and [10.15468/dl.fuu99k](http://doi.org/10.15468/dl.fuu99k), can be found as a zip file in the data folder for quick testing. Also included are an example Darwin Core archive from GBIF Norway's [IPT server](https://ipt.gbif.no/resource?r=trom_algae) and a Biocase archive for the Herbarium Berolinense.
