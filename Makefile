all:
	stack build && stack exec zn

# init:
#     cd ~/zn
#     git init zn --bare
#     su -c 'pacman -S haskell-stack'

deploy:
	git push origin master $(GITARGS)
	ssh zn \
		cd '~/zn'\; \
		git fetch origin master\; \
		git reset --hard origin/master\; \
		make service

service:
	mkdir -p ~/stack-tmp
	TMPDIR=~/stack-tmp stack setup
	stack build
	mkdir -p ~/.config{,/systemd{,/user}}
	cp $$(realpath zn.service) ~/.config/systemd/user
	systemctl --user enable zn
	systemctl --user restart zn
	rm -r ~/stack-tmp
