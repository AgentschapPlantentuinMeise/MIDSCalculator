# Using the app

### Submit data

On the app's initial interface, a zipped Darwin Core Archive, a comma or tab separated Simple Darwin Core Occurrence file or a Biocase ABCD XML archive can be uploaded (max 5GB). During local use of the app, this simply means the dataset will be loaded into your local memory.

Depending on the file uploaded, the user should specify the discipline, standard and formatting of the standard provided. Currently, there are only mappings for biological specimens, not yet for geological or paleontological ones. The app supports Darwin Core, both as Simple Darwin core CSV files and as zipped Darwin Core archives, and ABCD in the form of zipped XML Biocase archives. After this a simple click on the Start button will commence the calculation process. Depending on the size of the data file, this can take several minutes. Biocase archives are particularly slow and progress is shown in terms of numbers of files already processed. The example Herbarium Berolinense archive should take a few minutes, but larger archives can take much longer.

### Results

The results of each analysis are visualized on a new page, where it is possible to explore summaries of the results of both MIDS levels and MIDS elements, either as plots or as tables. The MIDS element plot can be clicked to get more details on the results of the mappings of that element. It is also possible to explore the complete records table with the MIDS results for each record, and to download it as a csv file. In addition, the data can be filtered to see how MIDS results change when filtering on properties such as country code /taxonomic group/ collection date (the filtering still needs to be reworked following the changes in the mapping model). The filename of the dataset is shown, as well as the used MIDS implementation, to make the provenance of the calculations clear. The data with its MIDS scores and boolean checks per information element can be downloaded as CSV files.

### Configuration

The app can be configured to limited extent by modifying the config.ini file using a simple text editor. The following parameters are included:

* max_size: max datafile size in megabytes.
* format: dwc-a, simple-dwc or biocase
* standard: dwc or abcd
* discipline: biology
* dwc-a_verbatim: using the verbatim.txt occurrence file in a Darwin Core archive. (true or false)
* version: version number

Mappings can be updated and replaced in the folders under /data/sssom . They are discipline and standard specific.
