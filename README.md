# Spillover Data Analysis Package

This is a collections of scripts for munging the spillover data set,
cleaning the columns that we are interested in for the data analysis,
and then creating plots from these data.

This also extracts information from PDF documents, currently using 
a modified/forked version of EcoHealth Alliance's epitator Python module.
Our forked version of Epitator is at 
   github.com:dsidavis/EpiTator
and can be cloned via
```
git clone git@github.com:dsidavis/EpiTator.git
```

The set the environment variable
PYTHONPATH to the top-level director EpiTator,
e.g.
```
git clone git@github.com:dsidavis/EpiTator.git
export PYTHONPATH=`pwd`/EpiTator
```
Alternatively, install the module using
```
python3 setup.py install
```
Using PYTHONPATH allows us to a) not install in a centralized directory 
for which we do not have write permissions, and b) also 
have different local versions of EpiTator that we can switch between very easily.



To use the epitator code, we use Python3.
Using pip3, install all the required python modules
```
pip3 install -r requirements.txt 
```
or with
```
sudo -H pip3 install -r requirements.txt 
```

