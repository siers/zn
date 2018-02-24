.PHONY: date build build-image docker restart

all: date build build-image docker restart date

date:
	date "+### %F-%T"

build:
	nix-build nix/default.nix -o result-zn

build-image:
	nix-build nix/docker.nix -o result-docker

docker:
	docker load < result-docker
	docker tag zn:latest siers/zn:$$(date +%F)
	docker tag zn:latest siers/zn:latest
	docker push siers/zn:latest
	docker push siers/zn:$$(date +\%F)

restart:
	ssh haskell.lv sudo -iu bot bash -c "\" \
		docker pull siers/zn:latest; \
		docker stop zn; \
		docker rm zn; \
		docker run -dv ~/zn:/work --name zn --link nsfw:nsfw siers/zn:latest; \
	\""
