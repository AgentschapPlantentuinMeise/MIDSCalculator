# Using the app

### Submit data

On the app's initial interface, a zipped Darwin Core Archive, a comma or tab separated Simple Darwin Core Occurrence file or a Biocase ABCD XML archive can be uploaded, as well as an experimental archive format for the Darwin Core Mineralogy extension (max 15GB). During local use of the app, this simply means the dataset will be loaded into your local memory, so for very large datasets this may not be possible. The app will open in your browser (preferring Firefox).

Depending on the file uploaded, the user should choose the most suitable mappings for their data and its format. The app will not work if you load a Biocase archive and choose Darwin Core Archive mappings. The app supports Darwin Core, both as Simple Darwin core CSV files and as zipped Darwin Core archives, and ABCD in the form of zipped XML Biocase archives. After this a simple click on the Start button will commence the calculation process. Depending on the size of the data file, this can take several minutes. Biocase archives are particularly slow and progress is shown in terms of numbers of files already processed. The example Herbarium Berolinense archive should take a few minutes, but larger archives can take much longer.

### Results

The results of each analysis are visualized on a new page, where it is possible to explore summaries of the results of both MIDS levels and MIDS elements, either as plots or as tables. The MIDS element plot can be clicked to get more details on the results of the mappings of that element. It is also possible to explore the complete records table with the MIDS results for each record, and to download it as a csv file. In addition, the data can be filtered to see how MIDS results change when filtering on properties such as country code and taxonomic group. The filename of the dataset is shown, as well as the MIDS mappings used, to make the provenance of the calculations clear. The data with its MIDS scores and boolean checks per information element can be downloaded as CSV files. Some of the information element lists link through to more documentation on the MIDS website.

### Configuration

The app can be configured to limited extent by modifying the config.ini file using a simple text editor. The following parameters are included:

* max_size: max datafile size in megabytes.
* sssom_id: the id of the default SSSOM mappings to use. These files can be found under /data/sssom where the app is installed. Mappings can easily be added by adding the correct .tsv and .yml file to that folder following the MIDS [mapping specification](https://tdwg.github.io/mids/mappings/index.html).
* dwc-a_verbatim: using the verbatim.txt occurrence file in a Darwin Core archive. (true or false)
* version: version number of the app. This is mainly important for building new installers.
* r_version: r version that comes packed with the installer.