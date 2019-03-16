# miv.select

This package builds methods to aid with binary classification through logistic regression based methods (including GLMNet and GAMS), particularly focused on applications in credit scoring. It provides methods for supervised and unsupervised binning, single factor screening using information values and an aid for variable selection via marginal information values. There is scope to develop a fully automated algorithm for variable selection but this hasn't been added yet.

Clike [here](https://htmlpreview.github.io/?https://github.com/louis-vines/miv.select/blob/master/vignettes/using-miv-to-select-variables-in-regression.html) to view the vignette describing the functionality of the package.

## Development

the `bin` folder provides command line executables for testing and documenting for those who prefer devevlopment at the command line.

Update documentation and namespace from a \*nix shell:

```
$ bin/document
```

Run all tests from a unix shell:

```
$ bin/test
```

Run only a specific file:

```
$ bin/test <path/to/file>
```

**NB** tests require testthat version > 2.0.0 to run as expected.

