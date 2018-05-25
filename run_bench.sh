#!/bin/sh
mkdir -p ./benchdata/
bench_output=benchdata/bench.csv
bench_output_pp=benchdata/bench_preprocessed.csv
rm -f $bench_output
stack bench --ba="--csv=$bench_output"
sed -e 's/Name/Name\/Category\/Method/' "$bench_output" | tr '/' ',' > "$bench_output_pp"
R CMD BATCH --no-save --no-restore "--args $bench_output_pp" make_plots.R
cp -f "$bench_output_pp" doc/
