IMAGE_NAME=ryanglscott/submods-to-source-repos
WORKDIR=/home/user/src
DOCKER_RUN=docker run --rm -it --volume $(shell pwd):${WORKDIR} --workdir ${WORKDIR} ${IMAGE_NAME}

all: alpine-release

docker-build:
	docker build -t ${IMAGE_NAME} \
	  --build-arg USER_ID=$(shell id -u) \
	  --build-arg GROUP_ID=$(shell id -g) \
	  .

.PHONY: bash
bash: docker-build
	${DOCKER_RUN}

.PHONY: alpine-release
alpine-release: docker-build
	${DOCKER_RUN} ./alpine-release.sh
