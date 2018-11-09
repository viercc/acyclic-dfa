#!/bin/sh

# Prepare the output directory
mkdir -p ./benchdata/

# Prepare benchmark URI data
bench_uri_all=bench/externallinks.txt.all
bench_uri_1=benchdata/externallinks.txt.1
bench_uri_2=benchdata/externallinks.txt.2
if [ -f "$bench_uri_1" ]; then
    echo $bench_uri_1 exists
else
    head -n 10000 "$bench_uri_all" > "$bench_uri_1"
    head -n 15000 "$bench_uri_all" | \
        tail -n 10000 > "$bench_uri_2"
fi

# Delete previous output file
bench_output=benchdata/bench.csv
bench_output_pp=benchdata/bench_preprocessed.csv
rm -f $bench_output

# Run benchmark
stack bench --ba="--csv=$bench_output"
sed -e 's/Name/DataSet\/Name\/Category\/Method/' "$bench_output" | tr '/' ',' > "$bench_output_pp"

# Make plots
R CMD BATCH --no-save --no-restore "--args $bench_output_pp" make_plots.R

# Copy the benchmark result into doc/ directory
cp -f "$bench_output_pp" doc/
