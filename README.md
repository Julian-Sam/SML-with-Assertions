# SML With Assertions

In this project, we implement native support for function contracts in the form of requires and ensures statements (prerequisites and postrequisites)

## Getting Started

Enter the main folder and run the parse.py file as follows:

python parse.py -f file1 file2... [-c sources_file] 

The files given using the -f flag should include all the files compiled in the sources file.

The file given using the -c flag should be the sources.cm file necessary to compile the above files

### Prerequisites

Standard ML installation:
http://smlnj.org/

Python 3 installation:
https://www.python.org/

### Installing

A step by step series of examples that tell you how to get a development env running

Say what the step will be 

```
# The -f positional parameter takes n number of files mentioned in the sources.cm file.
filepath>> python parse.py -f sample1.sml sample2.sml ...  -c sources.cm
filepath>> ...
```
```
# This indicates that compilation is successful 
filepath>> val it = true : bool 
```

End with an example of getting some data out of the system or using it for a little demo

## Running the tests

The baseline for the implementation of our parser involves adding SML GRAMMAR to support the use of
assertions. To add assertions to your SML file, follow these necessary steps. 

```
# This is how an assertion block is represented in our grammar

```

### Break down into end to end tests

Explain what these tests test and why

```
Give an example
```

### And coding style tests

Explain what these tests test and why

```
Give an example
```

## Deployment

Add additional notes about how to deploy this on a live system

## Versioning

We use [GitHub](http://github.com/) for versioning. For the versions available, see the [tags on this repository](https://github.com/your/project/tags). 

## Authors

* **Julian Sam** - [juliansam72](https://github.com/juliansam72)
* **Sameer Ahmad** - [Shurikenladd](https://github.com/Shurikenladd)


See also the list of [contributors](https://github.com/your/project/contributors) who participated in this project.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* Hat tip to anyone whose code was used
* Inspiration
* etc
