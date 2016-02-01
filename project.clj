(defproject rm-hull/wam "0.0.1-SNAPSHOT"`
  :description "Warren's Abstract Machine"
  :url "https://github.com/rm-hull/wam"
  :license {
    :name "The MIT License (MIT)"
    :url "http://opensource.org/licenses/MIT"}
  :dependencies [
    [org.clojure/clojure "1.8.0"]
    [table "0.4.0"]]
  :scm {:url "git@github.com:rm-hull/wam.git"}
  :plugins [
    [codox "0.9.1"] ]
  :source-paths ["src"]
  :jar-exclusions [#"(?:^|/).git"]
  :codox {
    :sources ["src"]
    :output-dir "doc/api"
    :src-dir-uri "http://github.com/rm-hull/wam/blob/master/"
    :src-linenum-anchor-prefix "L" }
  :min-lein-version "2.5.3"
  :profiles {
    :dev {
      :global-vars {*warn-on-reflection* true}
      :plugins [
        [lein-cloverage "1.0.6"]]}})
