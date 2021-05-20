# CH-CountryOfOrigin
Company registrations by Country of Origin for the basic Companies House data.

## tl;dr 
Using the [basic Companies House data](http://download.companieshouse.gov.uk/en_output.html), I calculate the registrations by `Country of Origin`. For those from the UK, I further separate them by country. The code creates (i) a donut pie chart and a (ii) sunburst. They are both interactive and supported by `plotly`.

## Files
The directory contains:

-   [:file\_folder: data/input](/data/input): includes the conversion between postcode areas (first one or two letters) and countries in the UK. 
-   [:file\_folder: data/output](/data/output): includes the resulting `.csv` file of registrations by country of origin *since January 2019*. 
-   [:file\_folder: output](/output): includes the resulting donut and sunburst graphs in `.html` format.


## Code
The code uses only **R**. Graphs are exported in `.html`.

## Feedback
For any feedback on the code or the graph(s), feel free to reach me at <galanakis.gian@gmail.com>.
