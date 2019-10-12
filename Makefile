.PHONY: help
help:
	@echo "    backup"
	@echo "        Pull configuration files from local computer"
	@echo "    restore_backup"
	@echo "        Push configuration files to local computer"
	@echo "    install"
	@echo "        Use to install all applications and dependencies"
	@echo "    commit"
	@echo "        Auto generate commit message"
	@echo "    full_backup"
	@echo "        Pull all configuration files from local computer"
	@echo "    full_restore_backup"
	@echo "        Push all configuration files to local computer"


.PHONY: backup
backup:
	@./scripts/backup.sh

.PHONY: restore_backup
restore_backup:
	@./scripts/restore_backup.sh

.PHONY: install
install:
	@./scripts/install.sh

.PHONY: commit
commit:
	@./scripts/commit.sh

.PHONY: full_backup
full_backup:
	@./scripts/full_backup.sh

.PHONY: full_restore_backup
full_restore_backup:
	@./scripts/full_restore_backup.sh

