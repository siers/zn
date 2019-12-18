#!/usr/bin/env ruby

require 'bundler'
Bundler.require

Dir.chdir(File.dirname($0))

# I tried to preseve as much code as snowball has written,
# so it uses StringIO, which is silly, but lets me keep f.puts.
# snowball's code below

require("json");
require("marky_markov");

json = JSON.parse(File.read("../data/namedays-extended.json"));
names = json.values.flatten;
s = ''

names.each do |name|
  s += name.split("").join(" ") + "." + "\n";
end

markov = MarkyMarkov::TemporaryDictionary.new;
markov.parse_string s;

sentence = markov.generate_n_sentences 1;
name = sentence.split(" ").join("").sub(".", "");

puts name;

