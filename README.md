# Rainbow Hash

A simple hash-based document store web service and associated applications.

![Rainbow Hash](./img/logo/rainbow-hash.png)

## Building

If you are using Nix or NixOS, start a shell with

    $ nix-shell

To build all the components, run

    $ cabal build all

During development use

    cabal build --ghc-options="-fforce-recomp" all

To always show compiler warnings (they're suppressed after the first build
otherwise.)

## Running

### Server

To start the web server, run the following command:

    $ cabal run rh-server

Then visit http://localhost:3000.  You can use the web UI to view and add
content to the service.

### CLI

Currently the CLI has only one available command: `watch`. The `watch` command
will watch the given directory and will upload files that are added to it.  Call
it like:

    $ cabal run rh-cli watch </path/to/directory>

Configuration of the server used by `rh-cli` can be found in
`<xdg-conf-dir>/rainbowhash/cli/config.yaml`.
