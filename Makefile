helasdasp:
	@echo "    backup"
	@echo "        Upload current configuration files to remote server";
	@echo "    restore_backup"
	@echo "        Override current configuration files with the remote server's version"
	@echo "    vim"
	@echo "   	   Edit initialize_env script"
backup:
	@./scripts/backup.sh

restore_backup:
	@./scripts/restore_backup.sh
vim:
	@vim ./scripts/initialize_env
