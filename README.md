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

    $ ghc --make hstter.hs config.hs model.hs
    $ ./hstter twitter.conf user-stream       # fetch and show your statuses
    $ ./hstter twitter.conf update-statuses   # update your statuses
    status=<input text>
    Status { ... }
    $ 


