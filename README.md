# bufr-dump
Software necessary to read data from bufr tanks to generate bufr dump files.

To install:

Clone repository:
```bash
git clone https://github.com/noaa-emc/bufr-dump
```

Move into desired branch and then run:

```bash
INSTALL_PREFIX=/path/you/wish/to/install/bufr-dump ./build.sh
```

or install in local clone space:

```bash
./build.sh
```

There is also the option to build and install in your local clone space but install the modulefile elsewhere:

```bash
MODULEFILE_INSTALL_PREFIX=/path/you/wish/to/install/bufr-dump/module ./build.sh
```
