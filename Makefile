PACKAGES = packages/*
STACKS = environments

.PHONY: build down run server tf-doc

build:
	docker-compose build ${PACKAGE}

down:
	docker-compose down

run:
	docker-compose run \
		--service-ports \
		# See https://github.com/rocker-org/shiny/issues/79
		--rm ${PACKAGE} /usr/bin/shiny-server \
		${CMD}

serve:
	docker-compose up 

tf-doc:
	# Requires Terraform Docs
	# @link https://github.com/terraform-docs/terraform-docs
	for dir in ${STACKS}; do \
  		terraform-docs \
  			-c ../.terraform-docs.yml \
  			./infrastructure/$${dir} \
  			> ./infrastructure/$${dir}/README.md; \
  	done
