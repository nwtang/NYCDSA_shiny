import pandas as pd
import numpy as np
from google.cloud import bigquery
import os
from bq_helper import BigQueryHelper

os.environ["GOOGLE_APPLICATION_CREDENTIALS"]=r"C:\Users\ntang\Downloads\My Project-66567def43dc.json"
bq_assistant = BigQueryHelper("bigquery-public-data", "epa_historical_air_quality")
pollutants=['o3','no2']

QUERY = """
    SELECT
        pollutant.latitude AS Lat, pollutant.longitude AS Lon, pollutant.sample_measurement AS pollutant, pollutant.date_local as Date, pollutant.time_local as Time
    FROM
      `bigquery-public-data.epa_historical_air_quality.pollutant_hourly_summary` as pollutant
    WHERE
      pollutant.poc = 1
      AND EXTRACT(YEAR FROM pollutant.date_local) BETWEEN 2013 AND 2017
"""

df = None
for elem_g in pollutants : 
    query = QUERY.replace("pollutant", elem_g)
    temp = bq_assistant.query_to_pandas(query)
    df = pd.concat([df, temp], axis=1, join='outer')

