from darts.datasets import AirPassengersDataset
from darts.models import ARIMA

myseries = AirPassengersDataset().load()

model = ARIMA(
    p=0, d=1, q=1,
    seasonal_order=(0, 1, 1, 12)
)

model.fit(myseries)

res = model.model

print("\nParameter estimates:")
print(res.summary())