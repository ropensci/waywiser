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

/// Calculate ssd
#[extendr]
fn ssd_rust(truth: ArrayView1<f64>, estimate: ArrayView1<f64>) -> [f64; 1] {
    [(truth.into_owned() - estimate.into_owned()).mapv(|a| a.powi(2)).sum()]
}

/// Sum of Potential Difference from Ji and Gallo (2006)
#[extendr]
fn spod_rust(truth: ArrayView1<f64>, estimate: ArrayView1<f64>) -> [f64; 1] {
    let mean_truth = truth.mean().unwrap();
    let mean_estimate = estimate.mean().unwrap();
    let term_1 = (mean_truth - mean_estimate).abs();

    [((term_1 + (truth.into_owned() - mean_truth).mapv(|a| a.abs())) *
    (term_1 + (estimate.into_owned() - mean_estimate).mapv(|a| a.abs()))).sum()]
}

/// Return the unsystematic sum product-difference from Ji and Gallo (2006)
#[extendr]
fn spdu_rust(truth: ArrayView1<f64>, estimate: ArrayView1<f64>, corsign: i8) -> [f64; 1] {
    let gmfr_predict_truth = gmfr_rust(truth, estimate, corsign);
    let gmfr_predict_estimate = gmfr_rust(estimate, truth, corsign);

    let owned_estimate = estimate.into_owned();
    let owned_truth = truth.into_owned();

    let predicted_truth = gmfr_predict_truth[0] + (gmfr_predict_truth[1] * &owned_estimate);
    let predicted_estimate = gmfr_predict_estimate[0] + (gmfr_predict_estimate[1] * &owned_truth);

    [((owned_estimate - predicted_estimate).mapv(|a| a.abs()) *
    (owned_truth - predicted_truth).mapv(|a| a.abs())).sum()]
}

/// Return the systematic sum product-difference from Ji and Gallo (2006)
#[extendr]
fn spds_rust(truth: ArrayView1<f64>, estimate: ArrayView1<f64>, corsign: i8) -> [f64; 1] {
  [ssd_rust(truth, estimate)[0] - spdu_rust(truth, estimate, corsign)[0]]
}

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod waywiserrr;
    fn d_bar;
    fn gmfr_rust;
    fn ssd_rust;
    fn spod_rust;
    fn spdu_rust;
    fn spds_rust;
}
