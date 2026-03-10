from darts.datasets import AirPassengersDataset
from darts.models import ARIMA
from statsmodels.graphics.tsaplots import plot_acf
from statsmodels.graphics.gofplots import qqplot
from scipy.stats import gaussian_kde

import matplotlib.pyplot as plt
import numpy as np

myseries = AirPassengersDataset().load()

myseries.plot(figsize=(6, 4.5))
#plt.savefig("AirPassengersDataset.pdf", format="pdf")  
plt.show()

# Fit SARIMA(0,1,1)(0,1,1,12)
model = ARIMA(
    p=0, d=1, q=1,
    seasonal_order=(0, 1, 1, 12)
)

model.fit(myseries)

res = model.model

print(res.summary())

residuals = res.resid

####### Create a 4-panel diagnostic plot:
fig, axes = plt.subplots(2, 2, figsize=(6, 4.5))

# 1 Residuals time series
axes[0,0].plot(residuals)
axes[0,0].set_title("Residuals")
axes[0,0].set_xlabel("Time")
axes[0,0].set_ylabel("Residual")

# 2 Histogram of residuals
axes[0,1].hist(residuals, bins=15, density=True)
density = gaussian_kde(residuals)
x = np.linspace(min(residuals), max(residuals), 200)
axes[0,1].plot(x, density(x))
axes[0,1].set_title("Histogram of Residuals")

# 3 ac.f of residuals
plot_acf(residuals, lags=40, ax=axes[1,0])
axes[1,0].set_title("ACF of Residuals")

# 4 Q-Q plot of residuals
qqplot(residuals, line="s", ax=axes[1,1])
axes[1,1].set_title("Q-Q Plot")

plt.tight_layout()

#plt.savefig("sarima_residual_diagnostics.pdf")

plt.show()

