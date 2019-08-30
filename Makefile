help:
	@echo "    backup"
	@echo "        Upload current configuration files to remote server"
	@echo "    restore_backup"
	@echo "        Override current configuration files with the remote server's version"
	@echo "    install"
	@echo "        Use to install all applications and dependencies"
	@echo "    commit"
	@echo "        Auto generate commit message"

backup:
	@./scripts/backup.sh

restore_backup:
	@./scripts/restore_backup.sh

install:
	@./scripts/install.sh

commit:
	@./scripts/commit.sh

