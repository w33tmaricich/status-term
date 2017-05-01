(ns status-term.git
  (:require [clj-time.core :as t]
            [clj-time.coerce :as c]
            [clj-time.local :as l])
  (:use [clj-jgit.porcelain]
        [clj-jgit.querying]))

(defn project-commits
  "returns a list of all commits in the repo"
  [project-info]
  (let [repo (load-repo (:path project-info))
        logs (git-log repo)]
    (map #(commit-info repo %) logs)))

(defn your-commits
  "Returns a list of your commits."
  [project-info]
  (let [commits (project-commits project-info)]
    (filter #(= (:email %) (:email project-info)) commits)))

(defn time-within-week
  "Returns true if the time given is within a week timespan."
  [time-check]
  (let [unix-time (.getTime time-check)
        joda (c/from-long unix-time)
        today (l/local-now)
        week-ago (t/minus today (t/weeks 1))
        interval (t/interval week-ago today)]
    (t/within? interval joda)))

(defn weekly-commits
  "returns a list of commits that happened this week for a given project."
  [project-info]
  (let [commits (your-commits project-info)]
    (filter #(time-within-week (:time %)) commits)))
