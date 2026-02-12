# a minimal script to test that Darts and other libraries are working

from darts import TimeSeries
import pandas as pd
import matplotlib.pyplot as plt

# Create a simple time series
data = pd.Series([1, 3, 2, 5, 4])
ts = TimeSeries.from_series(data)

# Plot using Darts
ts.plot()

# Show the plot (you may not need this)
plt.show()