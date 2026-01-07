##
## EPITECH PROJECT, 2025
## B-FUN-500-LYN-4-1-glados-yanis.mignot
## File description:
## Makefile
##

COMPILER_DIR:=compiler
VM_DIR:=vm

all: compiler vm

compiler:
	$(MAKE) -C $(COMPILER_DIR)

vm:
	$(MAKE) -C $(VM_DIR)

clean:
	$(MAKE) -C $(COMPILER_DIR) clean
	$(MAKE) -C $(VM_DIR) clean

fclean:
	$(MAKE) -C $(COMPILER_DIR) fclean
	$(MAKE) -C $(VM_DIR) fclean

re: fclean all

tests_run:
	$(MAKE) -C $(COMPILER_DIR) tests_run
	$(MAKE) -C $(VM_DIR) tests_run	

.PHONY: all clean fclean re tests_run compiler vm
