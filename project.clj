(defproject rm-hull/wam "0.0.1-SNAPSHOT"
  :description "Warren's Abstract Machine"
  :url "https://github.com/rm-hull/wam"
  :license {
    :name "The MIT License (MIT)"
    :url "http://opensource.org/licenses/MIT"}
  :dependencies [
    [org.clojure/clojure "1.8.0"]
    [rm-hull/jasentaa "0.2.3"]
    [rm-hull/table "0.6.0"]]
  :scm {:url "git@github.com:rm-hull/wam.git"}
  :plugins [
    [codox "0.9.5"] ]
  :source-paths ["src"]
  :jar-exclusions [#"(?:^|/).git"]
  :codox {
    :source-paths ["src"]
    :output-path "doc/api"
    :source-uri "http://github.com/rm-hull/wam/blob/master/{filepath}#L{line}"}
  :min-lein-version "2.6.1"
  :profiles {
    :dev {
      :global-vars {*warn-on-reflection* true}
      :plugins [
        [lein-cloverage "1.0.6"]]}})
