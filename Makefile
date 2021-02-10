PACKAGES = packages/*
STACKS = environments

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

tf-doc:
	# Requires Terraform Docs
	# @link https://github.com/terraform-docs/terraform-docs
	for dir in ${STACKS}; do \
  		terraform-docs \
  			-c ../.terraform-docs.yml \
  			./infrastructure/$${dir} \
  			> ./infrastructure/$${dir}/README.md; \
  	done
.PHONY: tf-doc
