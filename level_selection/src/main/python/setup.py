
from distutils.core import setup

setup(
	name = "prediction",
	packages = [
		"prediction"
	],
	install_requires = [
    "cycler>=0.10.0",
    "joblib>=0.14.1",
    "kiwisolver>=1.1.0",
    "matplotlib>=3.1.3",
    "numpy>=1.18.1",
    "pandas>=1.0.1",
    "pyparsing>=2.4.6",
    "python-dateutil>=2.8.1",
    "pytz>=2019.3",
    "scikit-learn>=0.22.2.post1",
    "scipy>=1.4.1",
    "six>=1.14.0",
    "sklearn>=0.0",
    "sklearn-pandas>=1.8.0",
    "sklearn2pmml>=0.55.0",
	]
)