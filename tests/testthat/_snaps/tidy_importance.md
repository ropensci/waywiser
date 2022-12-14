# expected failures

    Code
      tidy_importance(list())
    Error <rlang_error>
      Can't construct a tidy importance table from an object of class list

---

    Code
      tidy_importance(data.frame())
    Error <rlang_error>
      'term' and 'estimate' must be columns in `importance`

