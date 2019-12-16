# Wrapper around `mSentential` for non-lispers

This is intended to be a collection of useful tools to quickly inspect
`mSentential` for people who are not versed in the beauty of `lisp`. More
information about the theory `mSentential` implements in:

> Khemlani, S. S., Byrne, R. M., & Johnson‐Laird, P. N. (2018). Facts and
> Possibilities: A Model‐Based Theory of Sentential Reasoning. Cognitive
> science, 42(6), 1887-1924.

Authors of this project:

* [Mathias Sablé-Meyer](https://s-m.ac)
* [Salvador Mascarenhas](http://web-risc.ens.fr/~smascarenhas/)

## Requirements

For these tools to work, you need:

* `ecl` or any other lisp interpretor that can load several files before
  running a script from another file
* `R` with the packages `tidyr`, `dplyr` and `pander`
* `bash` to glue things together.

## `wrapper.R`

This tool provides a table containing the output of `mSentential` for a given
formula, for all extreme values of the `gamma` and `sigma` parameters. It
expects a formula and request (one of `necessary`, `possible`, `what-follows`,
`probability` and `verify`), runs the model 100 time per condition, and
aggregates the output.

```bash
$ ./wrapper.R "(a ore b)(a)" "necessary"
Table: summary of the output for the formula `(inference '((a ore b)(a)) 'necessary?)` under extreme parameters for sigma and gamma

+---------+---------+--------+-----+-----+
|         |         | result | NO  | YES |
+---------+---------+--------+-----+-----+
| UseSys2 | UseWeak |        |     |     |
+---------+---------+--------+-----+-----+
|  FALSE  |  FALSE  |        | 100 |  0  |
+---------+---------+--------+-----+-----+
|         |  TRUE   |        |  3  | 97  |
+---------+---------+--------+-----+-----+
|  TRUE   |  FALSE  |        | 100 |  0  |
+---------+---------+--------+-----+-----+
|         |  TRUE   |        |  0  | 100 |
+---------+---------+--------+-----+-----+
```

## `expose_internals.sh`

This tools takes values for `sigma` and `gamma` and a formula, and outputs
`mSentential`'s internal deliberations.

```bash
$ ./expose_internals.sh 1 0 "(a ore b)(a)"
---- --------- --------------------------------------------------------------------------- -------
Step System    Description                                                                 Runtime
---- --------- --------------------------------------------------------------------------- -------
0    --        Initialized trace                                                           0
0    Control   No task specified and so builds models of all the premises                  1
1    Language   Parsing premise:                                                           1
2    Language   A ore b.                                                                   1
3    System 1   Constructs models:                                                         2
4    Language   Parsing premise:                                                           2
5    Language   A.                                                                         2
6    System 1   Constructs models:                                                         2
7    System 1   Model of conjunction of premises:                                          2
8    System 1  The premises yield the models: (((- B) (A)))                                2
9    Language   Parsing premise:                                                           2
10   Language   A ore b.                                                                   2
11   System 1   Constructs models:                                                         2
12   Language   Parsing premise:                                                           2
13   Language   A.                                                                         2
14   System 1   Constructs models:                                                         2
15   System 1   Model of conjunction of premises:                                          2
16   System 1  Number of explicit mental models constructed equals 8.                      2
17   Control   Engaging System 2                                                           2
18   Language   Parsing premise:                                                           2
19   Language   A ore b.                                                                   2
20   System 2   Constructs models:                                                         2
21   Language   Parsing premise:                                                           2
22   Language   A.                                                                         2
23   System 2   Constructs models:                                                         2
24   System 2   Model of conjunction of premises:                                          3
25   System 2  The premises yield the models: (((A) (- B)))                                3
26   System 2  Number of fully explicit models constructed equals 4.                       3
```
