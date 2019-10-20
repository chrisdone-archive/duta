# duta

## Getting the build environment

Make the image in which you can build duta, it contains the
dependencies required to build duta:

    $ docker image build -t chrisdone/duta-build docker -f docker/build.Dockerfile

You should now have `chrisdone/duta-build` on your machine.

## Compiling a deploy

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

## Deploying

Create a postgres database somewhere, e.g. a DigitalOcean managed
database.

Create an environment file like this called `env`.

    DUTA_WEB_connstr="dbname=duta user=duta password=YOURPASS host=YOURHOST port=YOURPORT"
    DUTA_WEB_port=3000
    DUTA_WEB_approot="https://yourdomain.com"
    DUTA_WEB_max_db_connections=10
    DUTA_WEB_username=YOURUSER
    DUTA_WEB_password="YOURPASS"

Spin up a DigitalOcean service (using `.do-token` for your token-file):

    $ docker-machine create --driver digitalocean --digitalocean-access-token $(cat .do-token) --digitalocean-monitoring --digitalocean-region "lon1" --digitalocean-size "s-1vcpu-1gb" YOURNAMEHERE
    $ eval $(docker-machine env YOURNAMEHERE)
