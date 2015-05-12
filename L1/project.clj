(defproject rm-hull/wam-l1 "0.0.1-SNAPSHOT"`
  :description "WAM: Language L1"
  :url "https://github.com/rm-hull/wam"
  :license {
    :name "The MIT License (MIT)"
    :url "http://opensource.org/licenses/MIT"}
  :dependencies [
    [org.clojure/clojure "1.7.0-beta2"]
    [table "0.4.0"]]
  :scm {:url "git@github.com:rm-hull/wam.git"}
  :plugins [
    [codox "0.8.11"] ]
  :source-paths ["src"]
  :jar-exclusions [#"(?:^|/).git"]
  :codox {
    :sources ["src"]
    :output-dir "doc/api"
    :src-dir-uri "http://github.com/rm-hull/infix/blob/master/"
    :src-linenum-anchor-prefix "L" }
  :min-lein-version "2.5.1"
  :global-vars {*warn-on-reflection* true})
