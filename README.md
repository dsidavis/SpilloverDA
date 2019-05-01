# Spillover Data Analysis Package

This is a collection of scripts for munging the spillover data set,
cleaning the columns that we are interested in for the data analysis,
and then creating plots from these data.

This also contains code for extracting information from PDF documents, currently using 
a modified/forked version of EcoHealth Alliance's epitator Python module.

## Getting the EpiTator

Our forked version of Epitator is at 
   github.com:dsidavis/EpiTator
and can be cloned via
```
git clone git@github.com:dsidavis/EpiTator.git
```

## Installing EpiTator (and epitator)

First, you need python3 installed centrally on your machine.
Check by running the shell command 

```
python3
```

If this does not work, check if the command python gives you a python3
version. a) Just run python and look at the first line of the output

```
python
Python 2.7.5 (default, Aug  4 2017, 00:39:18) 
[GCC 4.8.5 20150623 (Red Hat 4.8.5-16)] on linux2
Type "help", "copyright", "credits" or "license" for more information.
>>> 
```

OR, b) 

```
python --version
Python 2.7.5
```
	
If this is python 2, install python3. Ask a system administrator
or install yourself centrally. Alternatively, you can install this in your own 
working local directory, separately from everybody.	 It may be simplest to instal
from source rather than a package manager. YMMV.

With python3 installed, install the required python modules that epitator needs.
The best thing to do is to create a virtual environment so that these will be installed
locally into your own directory and you can easily remove and/or update. Just a good thing.

```
virtualenv --python=python3 MyPyEnv
```

This creates MyPyEnv in the current directory. You can do this anywhere.

Activate or turn-on this virtual environment with

```
source MyPyEnv/bin/activate
```

This assumes you are running the bash shell. If not, run it - just issue the command `bash`.


Now we install the epitator required modules.
To do this, change directory to the EpiTator directory and issue the command

```
cd EpiTator
pip3 install -r requirements.txt
```

We will not verify that the modules are in this virtual environment

```
python3
```
and run the python commands

```
import inspect
import lazy
print(inspect.getsourcefile(lazy))
```

Now, we can also install the epitator code into this virtual environment,

```
python3 setup.py install
```

Again, we can check the epitator modules  were installed there.

```
python3
```
```
import inspect
import epitator.version
print(inspect.getsourcefile(epitator.version))
```


If you are likely to be changing any of the epitator code,
it probably makes more sense to not install it explicitly
but to leave here in the EpiTator directory.
Then we tell python3 to find that code by setting the environment
variable PYTHONPATH

The set the environment variable
PYTHONPATH to the top-level director EpiTator in which are currently located, i.e.

```
export PYTHONPATH=`pwd`
```

## Left over - same idea from earlier.

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


