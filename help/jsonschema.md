# Structure of the JSON schema

### Metadata:

* `schemaName`: A label for the schema, no use outside of human readability.
* `schemaVersion`: Simple version tracker for when a schema receives an update.
* `date`: Date (and optionally time) for when this schema version was created. More reliable versioning property than schemaVersion.
* `schemaType`: Should always be `MIDScalculator`.

### unknownOrMissing

This section lists values for properties that are to be understood as absence, rather than presence, of data. That is, these are known values that indicate absence of data.

If no `property` is listed for a value, the value is applied to any property mapped to a MIDS element. If a property is listed, the value is only ignored for that property.

A `midsAchieved` flag is always present and typically `false`, to indicate this value is to be understood as absence. The schema parser does support negation, that is, a MIDS level that requires a certain property to be absent of data. This is implemented by specifying the `operator` as `NOT`. To ignore certain values for such a mapping, the `midsAchieved` flag would be `true`. Note that negation is currently not part of the default schema and also not supported in the interactive schema editor in the app (only by manually editing the JSON).

### MIDS levels

The four levels have their own section each. Each level should have at least one element as a condition, but can otherwise have any number of elements.

Within an element, the mappings for this element are listed. Mappings to multiple properties in the data source are possible, and requirements of `OR` or `AND` can be set to specify the logic of the mapping. The operator `NOT` is also supported, but currently not part of the schema (anymore) and it is not implemented in the interface that enables interactive schema editing. Example of a more complex mapping for a Location term (note that this particular mapping is a suggestion, not currently agreed in the latest official MIDS draft):

```
"Location": [
    {
        "property": [
            "decimalLatitude",
            "decimalLongitude"
        ],
        "operator": "AND"
    },
    {
        "property": [
            "locality",
            "county",
            "verbatimLocality"
        ],
        "operator": "OR"
    }
]
```