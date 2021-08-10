# CSDC DAO

This repository contains a work-in-progress webapp for the CSDC DAO. It
currently has two components: a server and a GUI.

This fork exists to manage the project with Stack. With Stack, the [haskell-language-server](https://github.com/haskell/haskell-language-server) works out of the box.

## Development

This projects uses [Stack](https://docs.haskellstack.org/en/stable/README/) and [Elm](https://elm-lang.org/).

On Debian you must use the Bullseye repos for Stack to work:

```
sudo apt install haskell-stack
```

You will need an ID and a secret from
[ORCID](https://orcid.org/developer-tools), and write them to the `secret.json`
file, which should follow the model of the `secret-model.json` file.

## Dependencies

On Debian:

```
sudo apt install libpq-dev zlib1g-dev
```

## Postgresql configuration without docker

On Debian:

```
sudo su postgres
```

Then:

```
createuser --pwprompt csdc
createdb -O csdc csdc
```

## Building the server

### Debug (faster compile times)

Building the GUI:

```
make gui-debug
```

Building the server:

```
make build-debug
```

### Release

Clean the debug builds:

```
make clean-haskell && make clean-elm
```

Building the GUI:

```
make gui-release
```

Building the server:

```
make build-release
```

## Running the server

```
make serve
```

and the server should be available at `localhost:8080`.
