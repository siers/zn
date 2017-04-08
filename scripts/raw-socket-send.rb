#!/usr/bin/env ruby

require 'json'
require 'base64'

msg = ARGV.map { |a| Base64.encode64(a).chomp }.to_json
$stderr.puts(msg)

IO.popen(['socat', '-', "unix:#{ Dir.glob('/tmp/zn.sock.*')[0] }"], 'w') do |io|
  io.write(msg)
  io.close
end
