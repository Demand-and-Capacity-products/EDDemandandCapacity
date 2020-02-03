2019-08-02
Shiny file for single file upload.
Currently live at paulgbullard.shinyapps.io/EDForecasting

2019-08-25
Required updates - Excel model has been updated to accept only required columns. Live version to be updateed to force selection of particular columns.
Shinyapps.io was starting to fail due to memory issues. To be re-tested once most up to date version is live.

2019-08-06
Code updated. Not currently live, but available at paulgbullard.shinyapps.io/EDForecastingPrePublish (new site for tests before release to main app).

2019-08-27
Code made live. Users testing model (which only accepts two columns) were unable to import files correctly with previous Shiny script.

2019-09-18
Code updated to include time slider for non-24hour service. Currently only available at paulgbullard.shinyapps.io/EDForecastingPrePublish . Initial testing completed but some further (inc. documentation update) still to be done.

2019-09-26
Code updated to change formatting of text to match v1.8.1 model. User interface tidied up. Currently only available at paulgbullard.shinyapps.io/EDForecastingPrePublish. To update to main file when model is circulated to ensure compatability.

2019-11-13
Code updated to reflect changes in how model works with setting capacity to the next midnight. Available on main paulgbullard.shinyapps.io/EDForecasting site as of today.
TODO:
Work on percentiles currently in non-shiny prophet file. This needs to be brought in to shiny file.
Following conversations at NHSRConf19 file to be split in to two (server and UI), as well as reactivity improvements to be made.

UPDATE 2019-11-13
Percentile information brought in to shiny file. Not currently live on main site, but on EDForecastingPrePublish.

2020-02-03
Percentile information not currently in live. Further discussions to be had.
Performance increase for app by removing history from make_future_dataframe (monte-carlo only on newly predicted data).
Graph now hides if new data has been uploaded but not processed.
