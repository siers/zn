#!/usr/bin/env ruby

require("json");
require("marky_markov");

json = JSON.parse(File.read("namedays-extended.json"));
names = json.values.flatten;

File.open("names.txt", "w") do |f|
    names.each do |name|
        f.puts(name.split("").join(" ") + ".");
    end
end

markov = MarkyMarkov::TemporaryDictionary.new;
markov.parse_file "names.txt";

sentence = markov.generate_n_sentences 1;
name = sentence.split(" ").join("").sub(".", "");

puts name;

