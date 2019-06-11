#!/usr/bin/env bash

elm-app build &&
gcloud config set account julian.zucker@gmail.com &&
gsutil -m rsync -r build/ gs://www.julianzucker.com
