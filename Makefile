##
## EPITECH PROJECT, 2025
## B-FUN-500-LYN-4-1-glados-yanis.mignot
## File description:
## Makefile
##

BINARY_PATH 	:=	$(shell stack path --local-install-root)/bin/
NAME 			= 	glados

all		:
			stack build
			cp $(BINARY_PATH)$(NAME)-exe $(NAME)

clean	:
			stack clean
			rm -rf .stack
			rm -f coding-style-reports.log

fclean	:	clean
			stack purge
			rm -f $(NAME)

re		:	fclean all

tests_run	:
			stack test

.PHONY	:	all clean fclean re tests_run
