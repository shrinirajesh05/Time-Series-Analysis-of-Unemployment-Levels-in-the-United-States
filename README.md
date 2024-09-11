# Time-Series-Analysis-of-Unemployment-Levels-in-the-United-States

Overview:

This project focuses on analyzing and forecasting unemployment trends in the United States using time series analysis techniques. By leveraging monthly unemployment data from January 1948 to February 2024, derived from the Current Population Survey, this study aims to provide valuable insights into labor market dynamics and their implications for economic health.

Data:

Source: Current Population Survey (CPS)
Time Span: January 1948 - February 2024
Frequency: Monthly
Measurement: The data captures the total number of individuals actively seeking employment during the survey's reference week, measured in thousands.

Methodology:

The analysis uses Seasonal ARIMA (SARIMA) modeling to understand and forecast unemployment trends. Specifically, the model identified as most reliable is:

Model: ARIMA(2,1,2) × (0,1,1)[12]
Process: Iterative modeling of seasonal and non-seasonal components was employed to select the best model.

The chosen model:

Shows minimal violations of statistical assumptions.
Produces significant parameter estimates, ensuring confidence in the forecast's accuracy.

Goals and Applications:

Forecasting: The ARIMA model is used to forecast future unemployment trends based on historical data.
Policy Implications: The insights gained from this analysis can inform policymakers and macroeconomic analysts in crafting strategies and interventions to address unemployment and labor market challenges.
Economic Monitoring: Unemployment is a key economic indicator, and robust forecasting models such as SARIMA are essential for anticipating labor market shifts and responding to early signs of economic changes.

Results and Significance:

The ARIMA(2,1,2) × (0,1,1)[12] model demonstrates strong predictive capabilities, positioning it as a reliable tool for analyzing unemployment trends. The use of advanced time series techniques ensures that policymakers and analysts can confidently rely on these forecasts for decision-making processes.

Future Work:

The project acknowledges that forecasting unemployment is a dynamic process. Future iterations may involve incorporating additional factors, refining the model, and extending the forecasting horizon to adapt to changing economic conditions.
