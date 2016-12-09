all:
	stack build && stack exec zn

# init:
#     cd ~/zn
#     git init zn --bare
#     su -c 'pacman -S haskell-stack'

update:
	git fetch origin master
	git reset --hard origin/master

deploy:
	git push origin master $(GITARGS)
	ssh zn make -C '~/zn' service

stack:
	mkdir -p ~/stack-tmp
	TMPDIR=~/stack-tmp stack setup
	rm -r ~/stack-tmp

setup: stack
	date
	mkdir -p ~/.config{,/systemd{,/user}}
	cp $$(realpath zn.service) ~/.config/systemd/user
	systemctl --user enable zn

reload:
	systemctl --user reload zn

build:
	stack build

service: update setup build reload
	date
