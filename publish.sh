#!/bin/bash

runhaskell site.hs rebuild
git add .
git commit -m "publish" || true
git push heroku master
