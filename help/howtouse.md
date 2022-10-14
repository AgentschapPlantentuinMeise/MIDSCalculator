# Using the app

### Submit data

On the app's initial interface, a zipped GBIF annotated Darwin Core Archive or a comma or tab separated occurrence file can be uploaded (max 5GB). During local use of the app, this simply means the dataset will be loaded into your local memory.

Also on the interface, the MIDS implementation to be used can be viewed, as specified by the JSON schema. The schema lists all MIDS elements and how they are mapped to properties in the uploaded dataset. You can either choose the default schema included in the app or upload a custom schema from file. It is also possible to choose to edit this schema interactively in the app. The interactive editing opens in a pop-up window, where in a first tab, MIDS elements can be added, removed, or moved to another MIDS level. In addition, mappings can be removed or added by clicking the "edit" icon of a MIDS element. In a second tab, the Unknown or Missing section of the schema can be edited, i.e. new properties and new values can be added. This interactively edited schema can be saved to file (JSON). The schema (be it default, custom or interactive) can be viewed by clicking the eye icon, which opens a human-friendly visualization of the MIDS schema, so that it is not necessary to read the JSON file to be able to understand the specifics of the MIDS schema used. Once a dataset and a MIDS implementation have been chosen, calculations can be started by clicking "Start MIDS score calculations".

### Results

The results of each analysis are visualized on a new page, where it is possible to explore summaries of the results of both MIDS levels and MIDS elements, either as plots or as tables. The MIDS element plot can be clicked to get more details on the results of the mappings of that element. It is also possible to explore the complete records table with the MIDS results for each record, and to download it as a csv file. In addition, the data can be filtered to see how MIDS results change when filtering on properties such as country code /taxonomic group/ collection date. The filename of the dataset is shown, as well as the used MIDS implementation, to make the provenance of the calculations clear.'
 
