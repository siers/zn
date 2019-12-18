.PHONY: date build build-image docker restart

all: date build build-image docker-tag docker-push restart date

date:
	date "+### %F-%T"

build:
	nix build -f nix/default.nix -o result-zn

build-image:
	nix build -f nix/docker.nix -o result-docker

docker-tag:
	docker load < result-docker
	docker tag zn:latest siers/zn:$$(date +%F)
	docker tag zn:latest siers/zn:latest

docker-push:
	docker push siers/zn:latest
	docker push siers/zn:$$(date +\%F)

restart:
	ssh rv sudo -iu zn bash -c "\" \
		docker pull siers/zn:latest; \
		docker-compose pull zn; \
		docker-compose kill zn; \
		docker-compose up -d zn; \
	\""
