# R Shiny Apps

A simplified workflow for deploying R Shiny applications

## Deployment

> These instructions are for the `prod` environment. Additional environments
> can be configured by adding a new cluster into `/infrastructure/clusters` and
> adding the environment in the `/.github/workflows/environments.yaml` file and
> adding to the `jobs.provision.strategy.matrix.environment` key.

Deployments are controlled automatically using
[Terraform](https://www.terraform.io).

To configure the deployed web apps, go to
`/infrastructure/clusters/prod.tfvars` to configure the `prod` environment. The
`web_apps` is a list of objects that takes the following parameters - all are
required.

 - **always_on** (boolean) - decides whether the web app turns off
   automatically or remains alive.
 - **img** (string) - the Docker image name. Typically, will be the same as the
   folder names in `/packages`. 
 - **name** (string) - the name of the web app, which will be displayed in the
   URL. Typically, this will be the same as `img`.
 - **tag** (string) - the tag to deploy. A webhook is configured in the
   container registry to watch for changes. Typically, this will be `latest`.
 - **url** (string) - the url subdomain that this will appear under. This will
   be combined with the `domain` (typically, this will be `vcsep.org.uk`).

## Development Mode

> This is optional. If you have R installed on your machine, you can use it
> locally as normal.

As a convenience, there exists a [Docker
Compose](https://docs.docker.com/compose) configuration file. This enables you
to get a development environment set up quickly and reliably.

### Commands

```shell
make run PACKAGE=hello-world
```

This gets a specific package running. Replace the `PACKAGE` for the package you
wish to run (in this example, `hello-world` is being run)

---

```shell
make serve
```

This gets the whole stack running. It's probably not advisable to run this if
there are many applications in here as R apps can be rather heavyweight. Also,
the applications are likely to be independent.

---

```shell
make down
```

This destroys all running stacks. It's worth doing this when finished working
on the application as it uses resources on your machine.

---

```shell
make build PACKAGE=hello-world
```

Sometimes, you will need to rebuild the current image on your machine.
Typically, this will be if you have installed a new dependency in `deps.txt`.
As with `make run`, the `PACKAGE` variable indicates the package to build.

---

## Packages

A list of packages, and the local URLs in the Docker Compose stack.

 - [dashboard](http://localhost:3000)
 - [hello-world](http://localhost:3001)

### Adding A New R Application

In order to create a new application in the repo, will need to make the
following changes (use the `hello-world` as the example).

 - Add a new service in `/docker-compose.yml`
 - Create a new workflow file in `/.github/workflows`
 - Add the web app configuration in `/infrastructure/clusters/prod.tfvars`
 - Create the application in `/packages`

## Dependency Management

### R Environment

Application dependencies are managed with
[{packrat}](https://rstudio.github.io/packrat/). {packrat} doesn't just control
the _packages_ used by our application: it also controls the _versions_ of
these packages. Accounting for package versions aids in reproducibility and
long-term application stability. It is recommended that you [read the
information on this page](https://rstudio.github.io/packrat/walkthrough.html)
before proceeding.

To see if an existing project is already a {packrat} project look for the
existence of a `packrat/` directory in your project root (the project root is
the directory containing your project's `.Rproj` file).

If your project _is not_ a {packrat} project then you can make it one by
running `packrat::init()` from the project root. This will, amongst other
things, create a `packrat/packrat.lock` file (which details your project's
dependencies) and a private package library (`packrat/lib/`). The
`packrat/packrat.lock` file should be committed to your project's repo. The
private package library (`packrat/lib/`) should **not** be committed to your
project's repo (an entry has been added to this repo's `.gitignore` to try
dissuade this practice).

If your project is already a {packrat} project, but contains only a
`packrat/packrat.lock` file (and maybe a `packrat/packrat.opts` file), but no
private package library (`packrat/lib/`), then you need to _restore_ this
project's environment with `packrat::restore()`.

Once you are working from within a {packrat} project, any `install.packages()`
commands will install to your project's private package library (as opposed to
your global R library). If you have set `auto.snapshot: TRUE` in
`packrat/packrat.opts`, then using `install.packages()` will automatically
update the package listings in `packrat/packrat.lock`. If `auto.snapshot:
FALSE` you will need to update the `packrat/packrat.lock` file after any
package updates / installs, you can do with `packrat::snapshot()`.

### System Dependencies

Sometimes you will need to install operating system dependencies for your
application to function. These should be added to the application's
`Dockerfile`
 
```Dockerfile
RUN apt-get update \
    && apt-get install -y curl
```

## Further Reading

You will need to ensure that you have Docker and Docker Compose installed.

 - [Installing Docker](https://docs.docker.com/get-docker)
 - [Installing Docker Compose](https://docs.docker.com/compose/install)
