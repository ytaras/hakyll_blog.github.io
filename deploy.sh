runhaskell site.hs clean
git clone ./ _site
cd _site
git checkout gh-pages
cd ..
runhaskell site.hs build
cd _site
git commit -m "Generating..."
git push
cd ..
git push
