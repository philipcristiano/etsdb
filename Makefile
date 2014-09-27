compile: deps
	rebar compile

.PHONY: deps
deps:
	rebar get-deps

erl: compile
	ERL_LIBS=deps erl -pa ebin -pa deps -s pager_app -sname pager

.PHONY: server
server: compile
	ERL_LIBS=deps erl -pa ebin -pa deps -s pager_app -noshell -sname pager

.PHONY: test
test: compile
	rebar eunit

.PHONY: package
package:
	tar -czf package.tgz src Makefile start_server src rebar.config templates priv

.PHONY: deploy
deploy: package
	time ansible-playbook -i ansible/linode -u root ansible/deploy.yml

.PHONY: provision
provision:
	time ansible-playbook -i ansible/linode -u root ansible/site.yml

shell: compile
	erl -pag ebin \
	-name etsdb@127.0.0.1 \
	-setcookie shell \
	-pa deps/*/ebin \
	-pa ebin \
	-config etsdb \
	-eval "application:ensure_all_started(etsdb)."

shell_2: compile
	erl -pag ebin \
	-name etsdb_2@127.0.0.1 \
	-setcookie shell \
	-pa deps/*/ebin \
	-pa ebin \
	-config etsdb_2 \
	-eval "application:ensure_all_started(etsdb)."
