PACKAGES = packages/*

build:
	docker-compose build ${PACKAGE}
.PHONY: build

down:
	docker-compose down
.PHONY: down

run:
	docker-compose run \
		--rm \
		--service-ports \
		${PACKAGE} \
		${CMD}
.PHONY: run

serve:
	docker-compose up
.PHONY: serve
