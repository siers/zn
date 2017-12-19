# zn, the bot
It's a «developers» channel bot that is a great excuse to code haskell.

Uses regex-tdfa, lens, maybes and eithers, megaparsec, telegram-api, http-client-tls.  
Can be compiled with either nix or stack.

Currently deployed to a droplet inside a NixOS-built container[(read more)](https://raitis.veinbahs.lv/posts/2017-10-11-building-docker-containers-with-nixos.html),
together with the [`open_nsfw`](https://github.com/yahoo/open_nsfw) neural network, also wrapped in a [container](https://github.com/siers/zn/blob/master/lib/opennsfw-caffe-server/Dockerfile).

## syntax expose
    58:28          ij | zn: ping && čiki
    58:29          zn | pong ╱ briki
    01:12          ij | zn, 'quote' one 't wo' "three" fou\ r \x66ive
    01:12          zn | "one" "t wo" "three" "fou r" "five"
    05:34    daGrevis | ij ir kruts parseris ^
    05:49          ij | Pašam arī prieks.
    06:09          ij | Sanāk, ka komandas var pieprasīt hexā arī tad…
    07:06          ij | irb(main):009:0> puts "zn, " + "ping".each_char.map \
                      |   { |x| "\\x#{ x.ord.to_s(16) }" }.join
    07:06          ij | zn, \x70\x69\x6e\x67
    07:08          zn | pong
    07:20          ij | nenu labs
    08:23    daGrevis | :D

    13:27:02      ij | !echo tada && sleep 120 && ping
    13:27:02     vdk |  tada && sleep 120 && ping
    13:27:02    msks | tada && sleep 120 && ping
    13:27:02      zn | tada
    13:29:04      zn | pong

## url printing expose
    15:26       ij | http://www.delfi.lv/news/par-desu-nozagsanu?id=48438569
    15:26       zn | Par sešu desu nozagšanu apsūdzēto sievieti izsludina meklēšanā - DELFI

Bonus: NSFW image detection with `open_nsfw`! The code responsible for this is in [this tree](https://github.com/siers/zn/tree/master/src/Zn/Commands/URL).

https://developers.lv/da728bfb-5559-43fd-8682-fb98cd9ee35b :

    14:59  snowball | https://i.imgur.com/PPT9zUF.png
    14:59       +zn | ¬ image/png · NSFW: 88.6078%

## telegram expose
This bot can print links to images you've sent over telegram! [t.me/zn_devlv_bot](http://t.me/zn_devlv_bot).

Here's how it looked from the [developers.lv viewer](https://github.com/daGrevis/msks):
1\. [msg](https://developers.lv/925062ac-22fb-4211-aa87-35040fbdf6a8),
[pic](http://haskell.lv/share/2017-12-19-07:43:49-telegram-aasuea-910757721-bibl%20krasas.jpg),
2\. [msg](https://developers.lv/7fafd6a1-f88b-4a29-ab19-a7b614819503),
[pic](http://haskell.lv/share/2017-12-19-10:51:05-telegram-aasuea-910757722-pieliksu%20zn%20readme%20tekstu%20par%20telegrammu.jpg).

The telegram module's code is [here](https://github.com/siers/zn/blob/master/src/Zn/Telegram.hs).

    13:20   ij | !telegram
    13:20   zn | Upload images to #developerslv via the http://t.me/zn_devlv_bot telegram bot!

## sed substitute expose
    38:53 Tenjou   | heya
    41:53 Tenjou   | so whats new?
    43:17 Aleksejs | botofobi izbanoja manu botu no kanāla
    44:22 ij       | s/([^ ]+) ([^ ]+)/\\2 \\1/g
    44:23 zn       | izbanoja botofobi botu manu kanāla no
    44:41 ij       | Bet vismaz strādā ar kārtīgiem regexiem.

## botcast expose
This feature sends out the same command to the channel's bots and collects their output.

    13:15   ij | !botcast !ping
    13:16   zn | agni[pong ], msks[pong], vdk[pong], zn[pong]

# Command list
(as of 2017-12-19-13:18:10)

    -- Args, Pure, Reply, Output(no input/args), Lift(IO), Monad(noargs)

    commands :: M.Map Text (Command Text -> Bot ())
    commands = M.fromList
        [ commandPRA "echo"     (T.intercalate " ")
        , commandPRA "quote"    (\x -> "\"" <> T.intercalate "\" \"" x <> "\"")

        , commandPO  "version"  Zn.version
        , commandO   "uptime"   uptime
        , commandLO  "uname"  $ pack <$> shell "uname -a"

        , commandM   "reload"   reload
        , commandA   "sleep"  $ sleep . read . unpack . head
        , commandM   "shush"  $ do silence .= True; sleep 10; silence .= False

        , commandPO  "mping"      $ "--> !distribute !ping"
        , commandRA  "distribute" $ botcast . T.intercalate " "
        , alias      "botcast"    "distribute"

        , commandO   "replies"      Replies.list
        , commandLO  "iesauka"    $ pack <$> shell "./lib/names-lv/bundle_wrapper.rb"
        , commandRAL "urban"      $ urban

        -- leaks important data to chan, but might be useful for debugging sometimes
        -- , command "dump" (\_ ->
        --      (L.unpack . decodeUtf8 . encode . toJSON) <$> getTVar stateTVar)
        ]
