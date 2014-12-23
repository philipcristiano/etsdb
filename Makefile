PROJECT=etsdb
CT_OPTS = -create_priv_dir auto_per_tc

DEPS = leveltsdb riak_core cowboy jsx
dep_leveltsdb = git https://github.com/philipcristiano/leveltsdb.git 0.1.2
dep_riak_core = git https://github.com/basho/riak_core 2.0.2
dep_cowboy = git https://github.com/ninenines/cowboy 1.0.0
dep_jsx = git https://github.com/talentdeficit/jsx.git v2.1.1

.PHONY: release clean-release
release: clean-release all projects
	relx -o rel/$(PROJECT)

clean-release:
	rm -rf rel/$(PROJECT)

.PHONY: deploy
deploy: package
	time ansible-playbook -i ansible/linode -u root ansible/deploy.yml

.PHONY: provision
provision:
	time ansible-playbook -i ansible/linode -u root ansible/site.yml

shell_1: app
	mkdir -p data/{cluster_meta,ring}
	erl -pag ebin \
	-name etsdb@127.0.0.1 \
	-setcookie shell \
	-pa {apps,deps}/*/ebin \
	-pa ebin \
	-config etsdb \
	-eval "application:ensure_all_started(etsdb)."

shell_2: app
	mkdir -p data_2/{cluster_meta,ring}
	erl -pag ebin \
	-name etsdb_2@127.0.0.1 \
	-setcookie shell \
	-pa {apps,deps}/*/ebin \
	-pa ebin \
	-config etsdb_2 \
	-eval "application:ensure_all_started(etsdb)."

shell_3: app
	mkdir -p data_3/{cluster_meta,ring}
	erl -pag ebin \
	-name etsdb_3@127.0.0.1 \
	-setcookie shell \
	-pa {apps,deps}/*/ebin \
	-pa ebin \
	-config etsdb_3 \
	-eval "application:ensure_all_started(etsdb)."
include erlang.mk
