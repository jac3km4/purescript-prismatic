
NODE_ENV=production pulp browserify --to bundle.js --optimise
closure-compiler -O SIMPLE --js bundle.js --js_output_file opt.js
