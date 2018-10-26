# purescript-ssh2-sftp-client

This library is a thin wrapper around npm's ssh2-ftp-client.
If provided documentation is not enough, please refer to the original library documentation
or look at the code - it is kept simple.

## Installation

Currently Node.js is only supported platform. To use the library make sure you have base npm's package installed:

```
npm i ssh2-sftp-client
```

Then simply add the library as a dependency in your Purescript project:

```
bower install purescript-ssh2-sftp-client
```

### Implementation status

Almost entire functionality of npm's ssh2-ftp-client is already implemented.
Here is a list of not yet implemented stuff:

- normal get operation (only fastGet is implemented)
- normal put operation (only fastPut is implemented)
- fastGet and fastPut don't take any configuration parameters.

If you want to see them implemented let me know or consider doing it yourself and issueing a PR.

### Usage


```purescript

module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (catchError, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.Network.SftpClient (delete, fastGet, fastPut, list, mkdir, rmdir, runSftpSession)

main :: Effect Unit
main = launchAff_ $ flip catchError
  onError $
  runSftpSession config $ do
    liftEffect <<< log <<< show =<< list "/"

    mkdir {path: "/test", recursive: false}
    fastPut { local: "./README.md", remote: "/test/README.md" }
    liftEffect <<< log <<< show =<< list "/test/"

    fastGet { local: "./README2.md", remote: "/test/README.md" }

    delete "/test/README.md"
    rmdir {path: "/test", recursive: false}

  where
    onError e = liftEffect $ log $ "Catched error: " <> (show e)
    config =
      { username: "foo"
      , password: "asdqwe123"
      , host: "127.0.0.1"
      , port: "22"}

```

### UnsafeInternal module

The unsafety lies in foreign type data SftpClientRef, which is a reference to the npm's package and which state may be a subject to change during each call.
You should not have to use this module unless to extend the library.
 
### Tests

Currently there are no tests provided, mainly because the library is just a thin wrapper.

### Credits

Library is co-created and funded by [`λ-terms`](https://github.com/lambdaterms/)

### Module documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-ssh2-sftp-client).

### License & copyrights

See LICENSE file.
