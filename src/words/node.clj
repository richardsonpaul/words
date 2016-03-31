(require 'cljs.build.api)

(cljs.build.api/build "src"
 {:main 'words.core
  :output-to "main.js"
  :target :nodejs})
