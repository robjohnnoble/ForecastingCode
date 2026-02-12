# some of the demo code from https://unit8co.github.io/darts/quickstart/00-quickstart.html

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

from darts import TimeSeries
from darts.utils.utils import generate_index

from darts import set_option

set_option("plotting.use_darts_style", True)

# generate a sine and cosine wave
x = np.linspace(0, 2 * np.pi, 100)
sine_vals = np.sin(x)
cosine_vals = np.cos(x)

# generate a DatetimeIndex with daily frequency
dates = generate_index("2020-01-01", length=len(x), freq="D")

# create a DataFrame; here we use pandas but you can use any other backend (e.g. polars, pyarrow, ...).
df = pd.DataFrame({"sine": sine_vals, "cosine": cosine_vals, "time": dates})

series = TimeSeries.from_dataframe(df, time_col="time")
series.plot();

# Show the plot
plt.show()

from darts.datasets import AirPassengersDataset

series = AirPassengersDataset().load()
series.plot();

# Show the plot
plt.show()

from darts.utils.statistics import plot_acf

plot_acf(series)

# Show the plot
plt.show()