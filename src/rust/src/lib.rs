use extendr_api::prelude::*;
use ndarray_stats::{DeviationExt};

/// Calculate Euclidean distance matrix
#[extendr]
fn d_bar(a: ArrayView2<f64>) -> f64 {
    let nrow = a.nrows();
    let outsize = nrow * (nrow - 1) / 2;
    let mut out = Vec::<f64>::with_capacity(outsize);

    for x in 0..(nrow - 1) {
        for y in (x + 1)..nrow {
            let z = a.slice(s![x, ..]).l2_dist(&a.slice(s![y, ..])).unwrap();
            out.push(z);
        }
    }
    out.iter().sum::<f64>() / out.len() as f64
}

/// Calculate geometric mean functional relationship parameters
#[extendr]
fn gmfr_rust(truth: ArrayView1<f64>, estimate: ArrayView1<f64>, corsign: i8) -> [f64; 2] {
    let mean_truth = truth.mean().unwrap();
    let mean_estimate = estimate.mean().unwrap();

    let b = ((truth.into_owned() - mean_truth).mapv(|a| a.powi(2)).sum() /
        (estimate.into_owned() - mean_estimate).mapv(|a| a.powi(2)).sum()).sqrt().abs() * corsign as f64;

    let a = mean_truth - (b * mean_estimate);

    [a, b]
}

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod waywiserrr;
    fn d_bar;
    fn gmfr_rust;
}
