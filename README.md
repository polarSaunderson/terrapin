# terrapin

**PRIVATE BRANCH OF THE pkg05_terrapin PACKAGE**

## Overview
The terrapin package is a personal package that slightly builds on the [terra](https://rspatial.org) package. It is mainly composed of functions that allow SpatRasters to be easily subset based on their dates in different ways.

## Public Version
The public version of this repo can be accessed at: 
  https://github.com/polarSaunderson/terrapin

## To-Do
### 2023-09-07
- [ ] Think about monthDay, year and day equivalents to retrieve_months

### 2023-08-10
- [ ] Should any of the racmoR functions be in terrapin?
  - [ ] exclude functions
  - [ ] multiMonth
  - [ ] calc_in_space
  - [ ] others?
  
### 2023-08-03
- [ ] Think about correlate and regress by cell functions
 - [ ] Which functions do these need, and where should they be? e.g. `apply_lm()`

### 2023-08-02
- [ ] Code examples in `subset_by_month()`
- [ ] Code examples in `remove_incomplete_years()`
- [ ] Code examples in `remove_incomplete_summers()`
- [X] `subset_by_month() uses `domR::make_sentence_case()`
- [X] `subset_by_date()`
  - [X] improve examples
  - [X] look at the code & logic; try to remove domR::sift()
- [X] check the `remove_incomplete_x()` functions
