# Determining the Breeding Success of Red Kites (*Milvus milvus*) using GPS Telemetry
In my master’s thesis, the breeding behaviour of red kites (*Milvus milvus*) was analysed based on GPS point data collected by the Swiss Ornithological Institute. In particular, individual aspects of the breeding cycle were determined in order to evaluate the breeding success of red kites by developing a model that allows for detailed fine-tuning to account for the heterogeneity of behavioural patterns of individuals within a population. The following is an overview of the R scripts created for this purpose and an explanation of their function.

<br/>
### 00_master_script.R
The master script loads all required packages and runs all scripts.

<br/>
### 01_validation.R
This script creates an overview table with all available birds for each year in which tracking data was available and contains the following parameters:

- bird_id: unique identifier of a bird (may occur more than once if the data of the same bird are available in several years)
- year: year for which data is available
- year_id: unique identifier of a bird in a particular year (occurs only once)
- sex: biological sex of the respective bird
- nest_id: unique identifier of a nest (0 means that no nest was present)
- home_range_id: unique identifier of a home range (0 means that the bird has not settled in a home range)

This table is used for validation.

<br />
<br />

### 02_preparation.R
This script prepares the tracking data. The data of a bird is only retained if it appears in the validation data table. Furthermore, data is only kept for the time period relevant for breeding. Data containing NAs or empty coordinate columns is removed. Duplicates and spatial outliers are also removed. Finally, the data is resampled to a one-hour interval.

<br />
<br />

### 03_parameters.R
