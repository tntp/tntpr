Factorizing a survey dataset
================
2019-07-16

## Worked Example

Imagine you conduct a survey and are about to analyze the data. One of
the first steps in the data cleaning process is to “factorize” the
dataset. R uses factors to handle categorical variables, variables that
have a fixed and known set of possible values. Usually this is one of
the most manual, error-prone parts of the data-cleaning process.

Below is a workflow I use that might be helpful for others. I use a
couple functions that may be new to you.

  - `glimpse`: This function makes it possible to see every column in a
    data frame. It shows columns run down the page and data runs across.

  - `map`: The purrr version of the apply functions. Transforms the
    input by applying a function to each element and returning a vector
    the same length as the input. In this example, I use the map
    function with a data frame as an arguement. In this case, the inputs
    are the variable vectors and map applies the function across these
    variable vectors.

  - `factorize_df`: examines each column in a data.frame; when it finds
    a column composed solely of the values provided to the `lvls`
    argument it updates them to be factor variables, with levels in the
    order provided.

### Step 1: Identify which variables should be factors.

Admittedly, this part usually takes some knowledge of the dataset and/or
exploratory perusing of the dataset. Let’s first use `glimpse` to take a
look at the dataset.

``` r
survey_dat %>% 
  glimpse()
```

    ## Observations: 100
    ## Variables: 11
    ## $ response_id         [3m[38;5;246m<int>[39m[23m 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,...
    ## $ years_of_experience [3m[38;5;246m<dbl>[39m[23m 0, 9, 6, 1, 7, 2, 6, 4, 0, 6, 2, 9, 6, 8, ...
    ## $ q1                  [3m[38;5;246m<chr>[39m[23m "Somewhat disagree", "Disagree", "Strongly...
    ## $ q2                  [3m[38;5;246m<chr>[39m[23m "Disagree", "Somewhat agree", "Strongly ag...
    ## $ q3                  [3m[38;5;246m<chr>[39m[23m "Disagree", "Disagree", "Agree", "Somewhat...
    ## $ q4                  [3m[38;5;246m<chr>[39m[23m "You are well-made!", "You are divine!", "...
    ## $ q5                  [3m[38;5;246m<chr>[39m[23m "Disagree", "Strongly agree", "Disagree", ...
    ## $ q6                  [3m[38;5;246m<chr>[39m[23m "Agree", "Agree", "Strongly agree", "Agree...
    ## $ q7                  [3m[38;5;246m<chr>[39m[23m "Mm! YAY!-AHA! This is just priceless!", "...
    ## $ q8                  [3m[38;5;246m<chr>[39m[23m "Yes", "Yes", "Yes", "Yes", "No", "No", "Y...
    ## $ q9                  [3m[38;5;246m<chr>[39m[23m "Yes", "No", "Yes", "Yes", "No", "Yes", "N...

First thing I notice is all the question variables are character
vectors. Since I know some of the questions where multiple-choice
questions, it’s likely some of these should be converted to factors.

One thing you could try is seeing how many unique values each variable
has.

``` r
survey_dat %>% 
  map(unique) %>% 
  map(length)
```

    ## $response_id
    ## [1] 100
    ## 
    ## $years_of_experience
    ## [1] 11
    ## 
    ## $q1
    ## [1] 6
    ## 
    ## $q2
    ## [1] 6
    ## 
    ## $q3
    ## [1] 6
    ## 
    ## $q4
    ## [1] 63
    ## 
    ## $q5
    ## [1] 5
    ## 
    ## $q6
    ## [1] 5
    ## 
    ## $q7
    ## [1] 100
    ## 
    ## $q8
    ## [1] 2
    ## 
    ## $q9
    ## [1] 2

Here I notice `response_id`, `q4`, and `q7` have 50+ unique values and
should likely not be transformed to factors. Knowing the dataset, I
think `response_id` should probably be a character data type but will
not do that in this example. Finally, since each `q1`, `q2`, `q3`, `q5`,
`q6`, `q8`, and `q9` are “variables that have a fixed and known set of
possible values” they are likely candidates to be factors.

Next, I’ll see what the unique values are for these variables.

``` r
survey_dat %>% 
  select(q1, q2, q3, q5, q6, q8, q9) %>% 
  map(unique)
```

    ## $q1
    ## [1] "Somewhat disagree" "Disagree"          "Strongly Disagree"
    ## [4] "Somewhat agree"    "Strongly agree"    "Agree"            
    ## 
    ## $q2
    ## [1] "Disagree"          "Somewhat agree"    "Strongly agree"   
    ## [4] "Strongly Disagree" "Agree"             "Somewhat disagree"
    ## 
    ## $q3
    ## [1] "Disagree"          "Agree"             "Somewhat agree"   
    ## [4] "Somewhat disagree" "Strongly Disagree" "Strongly agree"   
    ## 
    ## $q5
    ## [1] "Disagree"                   "Strongly agree"            
    ## [3] "Agree"                      "Neither agree nor disagree"
    ## [5] "Strongly Disagree"         
    ## 
    ## $q6
    ## [1] "Agree"                      "Strongly agree"            
    ## [3] "Strongly Disagree"          "Disagree"                  
    ## [5] "Neither agree nor disagree"
    ## 
    ## $q8
    ## [1] "Yes" "No" 
    ## 
    ## $q9
    ## [1] "Yes" "No"

Aha\! These do indeed look like factors.

### Step 2: Transform to factors with appropriate level ordering.

Here a lot of people would `mutate` their dataset with a series of
factor(., levels = c(…)) mutations. This works, but let’s use
`factorize_df` to help us do this faster with less typing.

First, let’s see what the “levels” of `q1` are.

``` r
survey_dat$q1 %>% unique()
```

    ## [1] "Somewhat disagree" "Disagree"          "Strongly Disagree"
    ## [4] "Somewhat agree"    "Strongly agree"    "Agree"

Okay, looks like a 6-pt likert agreement scale. Let’s pass these levels
through the `factorize_df` function (in order) as the lvls arguement.

``` r
survey_dat <- survey_dat %>% 
  factorize_df(lvls = c("Strongly Disagree", "Disagree", "Somewhat disagree", "Somewhat agree", "Agree", "Strongly agree"))
```

    ## Transformed these columns: 
    ##  *  q1, 
    ## *  q2, 
    ## *  q3

``` r
survey_dat %>% 
  glimpse()
```

    ## Observations: 100
    ## Variables: 11
    ## $ response_id         [3m[38;5;246m<int>[39m[23m 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,...
    ## $ years_of_experience [3m[38;5;246m<dbl>[39m[23m 0, 9, 6, 1, 7, 2, 6, 4, 0, 6, 2, 9, 6, 8, ...
    ## $ q1                  [3m[38;5;246m<fct>[39m[23m Somewhat disagree, Disagree, Strongly Disa...
    ## $ q2                  [3m[38;5;246m<fct>[39m[23m Disagree, Somewhat agree, Strongly agree, ...
    ## $ q3                  [3m[38;5;246m<fct>[39m[23m Disagree, Disagree, Agree, Somewhat agree,...
    ## $ q4                  [3m[38;5;246m<chr>[39m[23m "You are well-made!", "You are divine!", "...
    ## $ q5                  [3m[38;5;246m<chr>[39m[23m "Disagree", "Strongly agree", "Disagree", ...
    ## $ q6                  [3m[38;5;246m<chr>[39m[23m "Agree", "Agree", "Strongly agree", "Agree...
    ## $ q7                  [3m[38;5;246m<chr>[39m[23m "Mm! YAY!-AHA! This is just priceless!", "...
    ## $ q8                  [3m[38;5;246m<chr>[39m[23m "Yes", "Yes", "Yes", "Yes", "No", "No", "Y...
    ## $ q9                  [3m[38;5;246m<chr>[39m[23m "Yes", "No", "Yes", "Yes", "No", "Yes", "N...

``` r
survey_dat %>% 
  map(levels)
```

    ## $response_id
    ## NULL
    ## 
    ## $years_of_experience
    ## NULL
    ## 
    ## $q1
    ## [1] "Strongly Disagree" "Disagree"          "Somewhat disagree"
    ## [4] "Somewhat agree"    "Agree"             "Strongly agree"   
    ## 
    ## $q2
    ## [1] "Strongly Disagree" "Disagree"          "Somewhat disagree"
    ## [4] "Somewhat agree"    "Agree"             "Strongly agree"   
    ## 
    ## $q3
    ## [1] "Strongly Disagree" "Disagree"          "Somewhat disagree"
    ## [4] "Somewhat agree"    "Agree"             "Strongly agree"   
    ## 
    ## $q4
    ## NULL
    ## 
    ## $q5
    ## NULL
    ## 
    ## $q6
    ## NULL
    ## 
    ## $q7
    ## NULL
    ## 
    ## $q8
    ## NULL
    ## 
    ## $q9
    ## NULL

Okay, that doesn’t look too impressive but notice that the function: a)
searched each column in the dataset and tested whether the unique values
that were a subset of the values in the lvls arguement, b) identified
`q1`, `q2`, and `q3` as having fitting that criteria, and c) transformed
those and only those three columns to factors with the appropriate
levels

Next I can move onto the next factor `q5`.

``` r
survey_dat$q5 %>% unique()
```

    ## [1] "Disagree"                   "Strongly agree"            
    ## [3] "Agree"                      "Neither agree nor disagree"
    ## [5] "Strongly Disagree"

``` r
survey_dat <- survey_dat %>% 
  factorize_df(lvls = c("Strongly Disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree"))
```

    ## Transformed these columns: 
    ##  *  q5, 
    ## *  q6

And finally `q8`.

``` r
survey_dat$q8 %>% unique()
```

    ## [1] "Yes" "No"

``` r
survey_dat <- survey_dat %>% 
  factorize_df(lvls = c("No", "Yes"))
```

    ## Transformed these columns: 
    ##  *  q8, 
    ## *  q9

### Step 3: Check if factorizing worked like you intended.

Okay we said we needed to change `q1`, `q2`, `q3`, `q5`, `q6`, `q8`, and
`q9` to factors. Let’s verify this worked.

``` r
survey_dat %>% 
  select(q1, q2, q3, q5, q6, q8, q9) %>% 
  map(is.factor)
```

    ## $q1
    ## [1] TRUE
    ## 
    ## $q2
    ## [1] TRUE
    ## 
    ## $q3
    ## [1] TRUE
    ## 
    ## $q5
    ## [1] TRUE
    ## 
    ## $q6
    ## [1] TRUE
    ## 
    ## $q8
    ## [1] TRUE
    ## 
    ## $q9
    ## [1] TRUE

They’re all factors…

``` r
survey_dat %>% 
  select(q1, q2, q3, q5, q6, q8, q9) %>% 
  map(levels)
```

    ## $q1
    ## [1] "Strongly Disagree" "Disagree"          "Somewhat disagree"
    ## [4] "Somewhat agree"    "Agree"             "Strongly agree"   
    ## 
    ## $q2
    ## [1] "Strongly Disagree" "Disagree"          "Somewhat disagree"
    ## [4] "Somewhat agree"    "Agree"             "Strongly agree"   
    ## 
    ## $q3
    ## [1] "Strongly Disagree" "Disagree"          "Somewhat disagree"
    ## [4] "Somewhat agree"    "Agree"             "Strongly agree"   
    ## 
    ## $q5
    ## [1] "Strongly Disagree"          "Disagree"                  
    ## [3] "Neither agree nor disagree" "Agree"                     
    ## [5] "Strongly agree"            
    ## 
    ## $q6
    ## [1] "Strongly Disagree"          "Disagree"                  
    ## [3] "Neither agree nor disagree" "Agree"                     
    ## [5] "Strongly agree"            
    ## 
    ## $q8
    ## [1] "No"  "Yes"
    ## 
    ## $q9
    ## [1] "No"  "Yes"

… with the correct levels and level order. And voila, a ‘factorized’
dataset.
