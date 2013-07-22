#!/bin/bash

dist/build/blog/blog rebuild
git add .
git commit -m "publish" || true
git push heroku master
