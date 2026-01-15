# My thesis_tests configuration

This directory contains configuration files that define the experimental setup used hopefully in my thesis evaluation  
The goal is to clearly separate **what is being tested** from **how the tests are executed**, making the analysis reproducible, extensible, and easy for me to measure

---

## `thesis_tests/config/functions_q1.json`

This file defines the **evaluation metrics** for **my first research question (Question 1)**, which investigates whether _function-level dependency inference_ is sufficient to reproduce runtime behavior without relying on DESCRIPTION metadata.

Each entry specifies:

- `package`: the R package under analysis
- `function`: the exported function being tested
- `category`: a classification used for grouping results in the thesis, also to make sure function evaluation is thorough

An example entry looks like:

```json
{
  "package": "stringr",
  "function": "str_count",
  "category": "string_wrapper"
}
```

## `thesis_tests/config/cran_mirror.txt`

This file specifies the CRAN mirror used for all package installations during testing in order to make environments / results reproducible

## `thesis_tests/config/run_one_q1.R`

This is the core validation script, designed in a modular way (used in coordination with the run_batch script)
It runs one function (defined in functions_q\*) inside an isolated library, loads only predicted dependencies, and checks:

- does the function load?
- does it run with specified deps returned by my analyzer?
- does it return correct results?

## `thesis_tests/scripts/run_q1_batch.sh`

This shell script simply runs all functions listed in functions_q1.json, one by one.

## `thesis_tests/scripts/get_predicted_deps.py`

Running this uses my analyzer to generate json output of predicted packages, which then we test to see what else is pulled in.

To run, run on cmd line

```
python3 thesis_tests/scripts/get_predicted_deps.py stringr str_count \
  > thesis_tests/results/q1/stringr__str_count_deps.json

python3 thesis_tests/scripts/get_predicted_deps.py stringr str_detect \
  > thesis_tests/results/q1/stringr__str_detect_deps.json

python3 thesis_tests/scripts/get_predicted_deps.py tidyr replace_na \
  > thesis_tests/results/q1/tidyr__replace_na_deps.json
```

This command runs each package function
