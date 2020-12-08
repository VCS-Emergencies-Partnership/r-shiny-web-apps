# R Shiny Apps

A simplified workflow for deploying R Shiny applications 

## Development Mode

> This is optional. If you have R installed on your machine, you can use it locally as normal.

As a convenience, there exists a [Docker Compose](https://docs.docker.com/compose) configuration
file. This enables you to get a development environment set up quickly and reliably.

### Commands

```shell
make run PACKAGE=hello-world
```

This gets a specific package running. Replace the `PACKAGE` for the package you wish to run (in this
example, `hello-world` is being run)

---

```shell
make serve
```

This gets the whole stack running. It's probably not advisable to run this if there are many
applications in here as R apps can be rather heavyweight. Also, the applications are likely to be
independent.

---

```shell
make down
```

This destroys all running stacks. It's worth doing this when finished working on the application
as it uses resources on your machine.

---

```shell
make build PACKAGE=hello-world
```

Sometimes, you will need to rebuild the current image on your machine. Typically, this will be if
you have installed a new dependency in `deps.txt`. As with `make run`, the `PACKAGE` variable
indicates the package to build.

---

## Packages

A list of packages, and the local URLs in the Docker Compose stack.

 - [hello-world](http://localhost:3000)

## Dependency Management

When Dockerising an R application, you will need to install your dependencies during the build
phase. To simplify that, add a list of dependencies to the `deps.txt` file - one per line.

Sometimes you will need to install operating system dependencies as well. This will need to
be added to the Dockerfile:
 
```Dockerfile
RUN apt-get update \
    && apt-get install -y curl
```

As this is an automated system, it's imperative that you use the `-y` flag with the `install`
command.

## Further Reading

You will need to ensure that you have Docker and Docker Compose installed.

 - [Installing Docker](https://docs.docker.com/get-docker)
 - [Installing Docker Compose](https://docs.docker.com/compose/install)
