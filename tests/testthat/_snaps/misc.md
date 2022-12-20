# ww_make_point_neighbors is stable

    Code
      ww_make_point_neighbors(guerry_pt)
    Output
      Neighbour list object:
      Number of regions: 85 
      Number of nonzero links: 85 
      Percentage nonzero weights: 1.176471 
      Average number of links: 1 
      Non-symmetric neighbours list

---

    Code
      ww_make_point_neighbors(guerry_pt, k = 2)
    Output
      Neighbour list object:
      Number of regions: 85 
      Number of nonzero links: 170 
      Percentage nonzero weights: 2.352941 
      Average number of links: 2 
      Non-symmetric neighbours list

# ww_make_polygon_neighbors is stable

    Code
      ww_make_polygon_neighbors(guerry)
    Output
      Neighbour list object:
      Number of regions: 85 
      Number of nonzero links: 420 
      Percentage nonzero weights: 5.813149 
      Average number of links: 4.941176 

---

    Code
      ww_make_polygon_neighbors(guerry, queen = FALSE)
    Output
      Neighbour list object:
      Number of regions: 85 
      Number of nonzero links: 420 
      Percentage nonzero weights: 5.813149 
      Average number of links: 4.941176 

# ww_build_neighbors is stable

    Code
      ww_build_neighbors(guerry)
    Output
      Neighbour list object:
      Number of regions: 85 
      Number of nonzero links: 420 
      Percentage nonzero weights: 5.813149 
      Average number of links: 4.941176 

---

    Code
      ww_build_neighbors(guerry_pt)
    Output
      Neighbour list object:
      Number of regions: 85 
      Number of nonzero links: 85 
      Percentage nonzero weights: 1.176471 
      Average number of links: 1 
      Non-symmetric neighbours list

---

    Code
      ww_build_neighbors(sf::st_cast(guerry, "MULTILINESTRING"))
    Warning <rlang_warning>
      Non-point or polygon geometry specified, calculating neighbors using sf::st_centroid()
      i To avoid this, provide neighbors explicitly
      i Or provide a neighbor-creating function
    Output
      Neighbour list object:
      Number of regions: 85 
      Number of nonzero links: 85 
      Percentage nonzero weights: 1.176471 
      Average number of links: 1 
      Non-symmetric neighbours list

---

    Code
      ww_build_neighbors(guerry, function(data) data)
    Error <rlang_error>
      Couldn't figure out how to build an `nb` object from the provided arguments

# ww_build_weights is stable

    Code
      ww_build_weights(guerry)
    Output
      Characteristics of weights list object:
      Neighbour list object:
      Number of regions: 85 
      Number of nonzero links: 420 
      Percentage nonzero weights: 5.813149 
      Average number of links: 4.941176 
      
      Weights style: W 
      Weights constants summary:
         n   nn S0      S1       S2
      W 85 7225 85 37.2761 347.6683

---

    Code
      ww_build_weights(guerry_pt)
    Output
      Characteristics of weights list object:
      Neighbour list object:
      Number of regions: 85 
      Number of nonzero links: 85 
      Percentage nonzero weights: 1.176471 
      Average number of links: 1 
      Non-symmetric neighbours list
      
      Weights style: W 
      Weights constants summary:
         n   nn S0  S1  S2
      W 85 7225 85 135 398

---

    Code
      ww_build_weights(sf::st_cast(guerry, "MULTILINESTRING"))
    Warning <rlang_warning>
      Non-point or polygon geometry specified, calculating neighbors using sf::st_centroid()
      i To avoid this, provide neighbors explicitly
      i Or provide a neighbor-creating function
    Output
      Characteristics of weights list object:
      Neighbour list object:
      Number of regions: 85 
      Number of nonzero links: 85 
      Percentage nonzero weights: 1.176471 
      Average number of links: 1 
      Non-symmetric neighbours list
      
      Weights style: W 
      Weights constants summary:
         n   nn S0  S1  S2
      W 85 7225 85 139 388

---

    Code
      ww_build_weights(guerry, function(data) data)
    Error <rlang_error>
      Couldn't figure out how to build a `listw` object from the provided arguments

---

    Code
      ww_build_weights(guerry, include_self = TRUE)
    Output
      Characteristics of weights list object:
      Neighbour list object:
      Number of regions: 85 
      Number of nonzero links: 505 
      Percentage nonzero weights: 6.989619 
      Average number of links: 5.941176 
      
      Weights style: W 
      Weights constants summary:
         n   nn S0       S1       S2
      W 85 7225 85 30.34284 343.0238

# expected_errors

    Code
      ww_agreement_coefficient_vec(1:5, c(1:4, NA), na_action = c(na.omit, na.pass))
    Error <rlang_error>
      Only one value can be passed to na_action.

---

    Code
      ww_build_neighbors(as.data.frame(guerry))
    Error <rlang_error>
      `data` must be an `sf` or `sfc` object.

---

    Code
      ww_make_point_neighbors(as.data.frame(guerry))
    Error <rlang_error>
      `data` must be an `sf` or `sfc` object.

---

    Code
      ww_make_point_neighbors(guerry, k = c(1, 5))
    Error <rlang_error>
      `k` must be a single numeric integer.

