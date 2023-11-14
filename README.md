# Rainbow Hash

A simple hash-based document store web service and associated applications.

![Rainbow Hash](./img/logo/rainbow-hash.png)

## Building

If you are using Nix or NixOS, start a shell with

    $ nix-shell

To build all the components, run

    $ cabal build all

## Running

To start the web server, run the following command:

    $ cabal run rh-server

Then visit http://localhost:3000.  You can use the web UI to view and add
content to the service.
