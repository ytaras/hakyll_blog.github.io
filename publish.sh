#!/bin/bash

git add .
git commit -m "publish" || true
git push heroku master
