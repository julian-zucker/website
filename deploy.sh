#!/usr/bin/env bash
# This file builds and deploys the website.
# Run from the top level with ./deploy.sh

elm-app build &&
gcloud config set account julian.zucker@gmail.com &&
gsutil -m rsync -r build/ gs://www.julianzucker.com
