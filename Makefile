.PHONY: help
help:
	@echo "    create_sym_link"
	@echo "        Generates symbolic link in the home directory"
	@echo "    delete_sym_link"
	@echo "        Remove the symblic link that was generated in the respective 'HOME' directory"
	@echo "    setup_arch"
	@echo "        Automate package installation for archlinux based OS"
	@echo "    setup_ubuntu"
	@echo "        Automate package installation for debian(Ubuntu) based OS (depreciated)"


.PHONY: create_sym_link
create_sym_link:
	@./scripts/create_sym_link.sh

.PHONY: delete_sym_link
delete_sym_link:
	@./scripts/delete_sym_link.sh

.PHONY: setup_arch
setup_arch:
	@./scripts/create_sym_link.sh
	@./scripts/setup_arch.sh

.PHONY: setup_ubuntu
setup_ubuntu:
	@./scripts/create_sym_link.sh
	@./scripts/setup_ubuntu.sh
