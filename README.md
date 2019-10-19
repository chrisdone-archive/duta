# duta

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
