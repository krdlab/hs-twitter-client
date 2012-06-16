hs-twitter-client
=================

Twitter client written in Haskell

## Prepare

Edit configuration file. (e.g. "twitter.conf")

    oauthConsumerKey = xxx
    oauthConsumerSecret = xxx
    accessToken = xxx
    accessSecret = xxx

## Build & Run

    $ cd ${project}
    $ cabal-dev install
    $ cabal-dev/bin/hstter twitter.conf user-stream       # fetch and show your statuses
    $ cabal-dev/bin/hstter twitter.conf update-statuses   # update your statuses
    status=<input text>
    Status { ... }
    $ 


