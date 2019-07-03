help:
	@echo "    push"
	@echo "        Upload current configuration files to remote server";
	@echo "    pull"
	@echo "        Override current configuration files with the remote server's version"

push:
	@./scripts/push.sh

pull:
	@./scripts/pull.sh
