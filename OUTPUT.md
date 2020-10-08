# Output description

The model generates the two following main outputs:

1. A time series of predictions (mean, 50% and 95% CI) of number of reported
COVID19 cases for each day and each LTLA.

2. The probility tha the total number of observed cases last week is greater
than what the model predicts. This is used to detect anomalies (e.g. a red flag 
will be generated if this probability is > 90%).

Please note that, if we call T the last day of cases present in the linelist data, 
the  model is fitted to data points that go from (T - 4 - 7 - 60):(T - 4 - 7). 
Predictions are then produced for the time window (T - 4 - 7 - 60):T and the 
second output is produced only for the last week (T - 6):T.