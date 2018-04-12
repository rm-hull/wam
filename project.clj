(defproject rm-hull/wam "0.0.1-SNAPSHOT"
  :description "Warren's Abstract Machine"
  :url "https://github.com/rm-hull/wam"
  :license {
    :name "The MIT License (MIT)"
    :url "http://opensource.org/licenses/MIT"}
  :dependencies [
    [org.clojure/clojure "1.9.0"]
    [rm-hull/jasentaa "0.2.5"]
    [rm-hull/table "0.7.0"]]
  :scm {:url "git@github.com:rm-hull/wam.git"}
  :source-paths ["src"]
  :jar-exclusions [#"(?:^|/).git"]
  :codox {
    :source-paths ["src"]
    :output-path "doc/api"
    :source-uri "http://github.com/rm-hull/wam/blob/master/{filepath}#L{line}"}
  :min-lein-version "2.8.1"
  :profiles {
    :dev {
      :global-vars {*warn-on-reflection* true}
      :plugins [
        [lein-codox "0.10.3"]
        [lein-cljfmt "0.5.7"]
        [lein-cloverage "1.0.10"]]}})
