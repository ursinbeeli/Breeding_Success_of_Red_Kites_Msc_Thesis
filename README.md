# Determining the Breeding Success of Red Kites (*Milvus milvus*) using GPS Telemetry
In my master’s thesis, the breeding behaviour of red kites (*Milvus milvus*) was analysed based on GPS point data collected by the Swiss Ornithological Institute. In particular, individual aspects of the breeding cycle were determined in order to evaluate the breeding success of red kites by developing a model that allows for detailed fine-tuning to account for the heterogeneity of behavioural patterns of individuals within a population. The model was also applied to movement data of red kites tagged in Thuringia (Germany) to evaluate the model's performance on birds living in a different habitat. The following is an overview of the R scripts created for this purpose and an explanation of their function.

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

<br/>

### 02_preparation.R
This script prepares the tracking data. The data of a bird is only retained if it appears in the validation data table. Furthermore, data is only kept for the time period relevant for breeding. Data containing NAs or empty coordinate columns is removed. Duplicates and spatial outliers are also removed. Finally, the data is resampled to a one-hour interval.

<br/>

### 03_parameters.R
This scripts calculates relevant movement parameters (7-Day MCP and its centroid) on a daily basis. The parameters serve as a basis for home range calculations in script 04_home_rang.R and nest calculations in script 05_nest.R.

<br/>

### 04_home_range.R
This script specifies thresholds that separate birds with a home range from birds without a home range.

<br/>

### 05_nest.R
This script calculates recursion parameters that allow an identification of birds with a nest. Thresholds are specified that separate birds with a nest from birds without a nest.

<br/>

### 06_mlrm_validation.R
This script creates a table of validation data about the breeding history of each bird. This information is used to validate the performance of the Multinomial Logistic Regression Model (MLRM) developed in 08_mlrm_model.R. Information is collected such as whether incubation took place, whether hatchlings were present, the egg laying date, the hatching date, the date the nest was empty, and the nest location.

<br/>

### 07_mlrm_parameters.R
This script calculates the relevant movement parameters on a daily basis. This data serves as training and testing data for the MLRM developed in script 08_mlrm_model.R.

<br/>

### 08_mlrm_model.R
This script trains an MLRM based on the training data, which later predicts the breeding behaviour of the birds on a daily basis. In addition, a plot with the model estimates is created to visually analyse the individual variables.

<br/>

### 09_mlrm_performance.R
This script predicts the breeding behaviour for the testing data and creates tables summarising the performance of the MLRM. It also creates the tables for the three brood phase identification methods.

<br/>

### 10_mlrm_prediction_plots.R
This script creates plots in which the predictions of the model for the testing data are visualised.

<br/>

### 11_mlrm_performance_de.R
This script predicts the breeding behaviour for the data of birds tagged in Thuringia (Germany) and creates tables summarising the performance of the MLRM. It also creates the tables for the three brood phase identification methods.


<br/>

### 12_mlrm_prediction_plots_de.R
This script creates plots in which the predictions of the model for the data of birds tagged in Thuringia (Germany) are visualised.

