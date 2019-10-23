# duta

A mail service written in Haskell

## Components

* [duta-model](https://github.com/chrisdone/duta/tree/master/duta-model) - the library which reads/writes email to a PostgreSQL
  database. Provides the model for all other components.
* [duta-smtp-receiver](https://github.com/chrisdone/duta/tree/master/duta-smtp-receiver) - a simple server which receives mail on SMTP
  (port 25)
* [duta-web](https://github.com/chrisdone/duta/tree/master/duta-web) - a web service which provides a simple web interface and
  JSON API.
* [duta-types](https://github.com/chrisdone/duta/tree/master/duta-types) - shared types between the various packages.

## Deploying

See
[Spinning up a Duta mail server on DigitalOcean](https://gist.github.com/chrisdone/bba2f8562ef58f1eed99081835ddf77e).

## Building

Make the image in which you can build duta, it contains the
dependencies required to build duta:

    $ docker image build -t chrisdone/duta-build docker -f docker/build.Dockerfile

You should now have `chrisdone/duta-build` on your machine.

Compile the project:

    $ docker image build -t chrisdone/duta-deploy docker -f docker/deploy.Dockerfile

You should now have an image `chrisdone/duta-deploy` on your machine
with the deploy.

    $ docker images chrisdone/duta-deploy
    REPOSITORY              TAG                 IMAGE ID            CREATED             SIZE
    chrisdone/duta-deploy   latest              e1cfecaf22b6        2 minutes ago       327MB

The executables are there ready to run under `/opt/duta/`:

    $ docker run --rm chrisdone/duta-deploy ls /opt/duta/
    duta-smtp-receiver
    duta-web
