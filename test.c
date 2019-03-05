#include "parser.c"

int main(void)
{
	result *rt = get_result("p<~q");
	print_result(rt);

	return 0;
}