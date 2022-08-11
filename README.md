# MIDSCalculator

Repository for the MIDSCalculator app, written mostly by @LynnDelgat during an internship at Meise Botanic Garden.

## Minimum Information about a Digital Specimen

Minimum Information about a Digital Specimen (MIDS) is a data standard which aims to describe four different levels of digitization status for biological and geological specimens from natural history collections, each level requiring certain information elements (referred to as MIDS elements from here on) to be digitally available. With this standard, users of collection data, as well as funders and collection curators will have a better overview of how comprehensively digitized a collection or any other set of specimens really is. The standard is currently still under development by a Task Group of the global Biodiversity Information Standards (“TDWG”) organization. While MIDS elements for digitization levels 0 and 1 have been specified, MIDS elements for levels 2 and 3 are still under discussion.
For more details: https://github.com/tdwg/mids

## MIDSCalculator App
The MIDSCalculator is a Shiny app which allows users to calculate MIDS scores for each record in a submitted dataset, and explore the results.

* [How to install](/help/howtoinstall.md)
* [Using the app](/help/howtouse.md)
* [Additional info about the code](/help/codeinfo.md)
* [Create a new installer](/help/rinno_installer.md).

## Mapping MIDS elements: use of a JSON schema

As the MIDS standard is agnostic to the data model of the sourced data, the app uses a JSON schema to map MIDS elements to properties of the sourced data. A schema mapping to GBIF annotated DwC data is included in the app. Other JSON schemas can be uploaded.  In addition the schema is editable through the app’s interface. 

* [Documentation on the JSON schema](/help/jsonschema.md)

## Data
### Datasets
* A sample dataset for testing, GBIF Occurrence Download [10.15468/dl.e8jnan](http://doi.org/10.15468/dl.e8jnan), can be found as a zip file in the data folder for quick testing.

### Schemas
* [DwC-GBIF_schema.json](/data/schemas/DwC-GBIF_schema.json): This schema is used as the default by the app and is based on the most recent MIDS specification for levels 0 and 1. As levels 2 and 3 are still under discussion, the schema offers a basic interpretation of several potential properties.