from darts.models import ExponentialSmoothing
from darts.models import ARIMA
from darts.models import AutoARIMA
from darts import TimeSeries
from darts.metrics import mae
import matplotlib.pyplot as plt

myseries = TimeSeries.from_csv("RunningData.csv", 
	time_col = "Date")

model_es = ExponentialSmoothing(seasonal_periods=7)
model_arima = ARIMA(p=1, d=0, q=0, seasonal_order=(1,1,0,7))
model_arima2 = ARIMA(p=0, d=0, q=1, seasonal_order=(2,1,0,7))

historical_fcast_es = model_es.historical_forecasts(
	myseries,
	start = 0.6,
	forecast_horizon = 7, 
	stride = 1)
historical_fcast_arima = model_arima.historical_forecasts(
	myseries,
	start = 0.6,
	forecast_horizon = 7, 
	stride = 1)
historical_fcast_arima2 = model_arima2.historical_forecasts(
	myseries,
	start = 0.6,
	forecast_horizon = 7, 
	stride = 1)

myseries.plot(label="data")
historical_fcast_es.plot(label="backtest forecast (Exp. Smoothing)")
print(f"MAE = {mae(historical_fcast_es, myseries):.2f}%")
plt.savefig("RunningForecast_ESP.pdf")
plt.show()

myseries.plot(label="data")
historical_fcast_arima.plot(label="backtest forecast (ARIMA)")
print(f"MAE = {mae(historical_fcast_arima, myseries):.2f}%")
plt.savefig("RunningForecast_ARIMA.pdf")
plt.show()

myseries.plot(label="data")
historical_fcast_arima2.plot(label="backtest forecast (alternative ARIMA)")
print(f"MAE = {mae(historical_fcast_arima2, myseries):.2f}%")
plt.savefig("RunningForecast_ARIMA2.pdf")
plt.show()

