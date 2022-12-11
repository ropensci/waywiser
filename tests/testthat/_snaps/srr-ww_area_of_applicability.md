# srr: expected failures for ww_area_of_applicability

    Code
      ww_area_of_applicability(y ~ ., train, test, importance)
    Error <rlang_error>
      All predictors must be numeric.

---

    Code
      ww_area_of_applicability(train, test, importance)
    Error <rlang_error>
      All predictors must be numeric.

---

    Code
      ww_area_of_applicability(comb_rset_no_y, importance = importance)
    Error <rlang_error>
      All predictors must be numeric.

---

    Code
      ww_area_of_applicability(y ~ ., train, test, importance)
    Error <simpleError>
      invalid type (list) for variable 'x3'

---

    Code
      ww_area_of_applicability(train, test, importance)
    Error <rlang_error>
      All predictors must be numeric.

---

    Code
      ww_area_of_applicability(comb_rset_no_y, importance = importance)
    Error <rlang_error>
      All predictors must be numeric.

---

    Code
      ww_area_of_applicability(y ~ ., head(train, 0), test, importance)
    Error <rlang_error>
      0 rows were passed as training data.

---

    Code
      ww_area_of_applicability(y ~ ., train, head(test, 0), importance)
    Error <rlang_error>
      0 rows were passed as testing data.

---

    Code
      ww_area_of_applicability(head(train[2:11], 0), test[2:11], importance)
    Error <rlang_error>
      0 rows were passed as training data.

---

    Code
      ww_area_of_applicability(train[2:11], head(test[2:11], 0), importance)
    Error <rlang_error>
      0 rows were passed as testing data.

---

    Code
      ww_area_of_applicability(head(as.matrix(train[2:11]), 0), as.matrix(test[2:11]),
      importance)
    Error <rlang_error>
      0 rows were passed as training data.

---

    Code
      ww_area_of_applicability(as.matrix(train[2:11]), head(as.matrix(test[2:11]), 0),
      importance)
    Error <rlang_error>
      0 rows were passed as testing data.

---

    Code
      ww_area_of_applicability(y ~ ., train_na, test, importance)
    Error <rlang_error>
      Missing values in the training set data (either `x` or `data`).
      i Either process your data to fix the NA values or set `na_action`.

---

    Code
      ww_area_of_applicability(y ~ ., train, test_na, importance)
    Error <rlang_error>
      Missing values in the testing set data (`testing` or `new_data`).
      i Either process your data to fix the NA values or set `na_action`.

---

    Code
      ww_area_of_applicability(train_na[2:11], test[2:11], importance)
    Error <rlang_error>
      Missing values in the training set data (either `x` or `data`).
      i Either process your data to fix the NA values or set `na_action`.

---

    Code
      ww_area_of_applicability(train[2:11], test_na[2:11], importance)
    Error <rlang_error>
      Missing values in the testing set data (`testing` or `new_data`).
      i Either process your data to fix the NA values or set `na_action`.

---

    Code
      ww_area_of_applicability(as.matrix(train_na[2:11]), as.matrix(test[2:11]),
      importance)
    Error <rlang_error>
      Missing values in the training set data (either `x` or `data`).
      i Either process your data to fix the NA values or set `na_action`.

---

    Code
      ww_area_of_applicability(as.matrix(train[2:11]), as.matrix(test_na[2:11]),
      importance)
    Error <rlang_error>
      Missing values in the testing set data (`testing` or `new_data`).
      i Either process your data to fix the NA values or set `na_action`.

---

    Code
      ww_area_of_applicability(comb_rset_no_y_train_na, importance = importance)
    Error <rlang_error>
      Missing values in the training set data (either `x` or `data`).
      i Either process your data to fix the NA values or set `na_action`.

---

    Code
      ww_area_of_applicability(comb_rset_no_y, comb_rset_no_y_test_na, importance)
    Error <rlang_error>
      All predictors must be numeric.

---

    Code
      ww_area_of_applicability(y ~ ., train, train, importance)
    Warning <rlang_warning>
      The AOA threshold was 0, which is usually unexpected.
    Output
      # Predictors:
         10
      Area-of-applicability threshold:
         0

---

    Code
      ww_area_of_applicability(train[2:11], train[2:11], importance)
    Warning <rlang_warning>
      The AOA threshold was 0, which is usually unexpected.
    Output
      # Predictors:
         10
      Area-of-applicability threshold:
         0

---

    Code
      ww_area_of_applicability(as.matrix(train[2:11]), as.matrix(train[2:11]),
      importance)
    Warning <rlang_warning>
      The AOA threshold was 0, which is usually unexpected.
    Output
      # Predictors:
         10
      Area-of-applicability threshold:
         0

---

    Code
      ww_area_of_applicability(comb_rset_no_y_identical, importance = importance)
    Warning <rlang_warning>
      The AOA threshold was 0, which is usually unexpected.
    Output
      # Predictors:
         10
      Area-of-applicability threshold:
         0

