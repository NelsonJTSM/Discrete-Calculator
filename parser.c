//
//  Parser.c
//  Discrete Math
//
//  Written by Nelson Torres and Arjun Pherwani on 3/2/19.
//  Copyright © 2019 Joseph Kocis. All rights reserved.
//

// TODO: Make sure the expression is valid (check parenthesis/all symbols/etc)

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "stack.h"
#include "parser.h"

#define ERROR_CHAR '?'
#define NUM_OPERATORS 5
#define LETTERS_LEN 26

int power(int a, int b);
char *create_string(int len);
result *create_result(int var_cnt, char *expression, int exp_len, int *letters);

char operators[] = {'~', '&', '^', '<', '>'};
int precedence[] = {2, 3, 3, 4, 4};

char *create_string(int len)
{
	char *new_string = malloc(sizeof(char) * (len + 1));

	if (new_string == NULL)
		return NULL;

	new_string[len] = '\0';
	return new_string;
}

result *create_result(int var_cnt, char *expression, int exp_len, int *letters)
{
	int i, ctrl = 0;
	result *final_result = malloc(sizeof(result));

	if (final_result == NULL)
		return NULL;

	// Initialize final_result.
	final_result->row_size = power(2, var_cnt);
	final_result->col_size = var_cnt + 1;

	// Fill header.
	final_result->header = malloc(sizeof(char *) * final_result->col_size);

	for (i = 0; i < LETTERS_LEN; i++)
	{	
		if (letters[i] > 0)
		{
			final_result->header[ctrl] = create_string(1);
			final_result->header[ctrl][0] = (char)('a' + i);
			ctrl++;
		}
	}

	final_result->header[ctrl] = create_string(exp_len);
	strcpy(final_result->header[ctrl], expression);

	// Do grid stuff
	final_result->grid = malloc(sizeof(char *) * final_result->row_size);
	for (i = 0; i < final_result->row_size; i++)
		final_result->grid[i] = malloc(sizeof(char) * final_result->col_size);

	return final_result;
}

void destroy_result(result *rt)
{
	int i, j;

	for (i = 0; i < rt->col_size; i++)
		free(rt->header[i]);

	for (i = 0; i < rt->row_size; i++)
		free(rt->grid[i]);

	free(rt->header);
	free(rt->grid);
	free(rt);
}

int is_operand(char c)
{
	switch (c) {
		case '0':
		case '1':
			return 1;
	}

	return 0;
}

int is_operator(char c)
{
	int i;

	for (i = 0; i < NUM_OPERATORS; i++)
		if (operators[i] == c)
			return 1;

	return 0;
}

// Returns -1 if c is not an operator
int get_precedence(char c)
{
	int i;

	for (i = 0; i < NUM_OPERATORS; i++)
		if (operators[i] == c)
			return precedence[i];

	return -1;
}

char to_char(int value)
{
	return (char)('0'+value);
}

char evaluate(char value1, char value2, char operator)
{
	int val1 = value1 - '0', val2 = value2 - '0';

	switch (operator) {
		case '^':
			return to_char(val1 && val2);
		case '&':
			return to_char(val1 || val2);
		case '<':
		case '~':
			return to_char(val1 == val2);
		case '>':
			if (val1 == 1 && val2 == 0)
				return '0';
			else
				return '1';
	}

	return ERROR_CHAR;
}

int power(int a, int b)
{
	int i, sum = 1;

	for (i = 0; i < b; i++)
	{
		sum *= a;
	}

	return sum;
}

char *infix_to_postfix(char *expression)
{
	char temp;
	int i, len, ctr = 0; 
	char *new_expression;
	stack *op;

	if (expression == NULL)
		return NULL;

	len = strlen(expression);
	new_expression = create_string(len);
	op = create_stack();

	for(i = 0; i < len; i++) 
	{
		temp = expression[i];
		
		if(is_operand(temp))
			new_expression[ctr++] = temp;

		if(temp == '(')
			push(op, temp);
		
		if(temp == ')')
		{
			while(!is_empty(op) && top(op) != '(')
			{
				new_expression[ctr++] = pop(op);
			}
			pop(op);
		}

		if(is_operator(temp))
		{
			if(is_empty(op) || top(op) == '(')
			{
				push(op, temp);
			}
			else
			{
				while(!is_empty(op) && top(op) != '(' && (get_precedence(temp) >= get_precedence(top(op))))
				{
					new_expression[ctr++] = pop(op);
				}

				push(op, temp);
			}
		}
	}

	while (!is_empty(op))
		new_expression[ctr++] = pop(op);

	new_expression[ctr++] = '\0';

	free_stack(op);
	op = NULL;

	return new_expression;
}

// TODO: Change length of allocated space (since it's wasting about half the space).
char *fix_exp(char *str)
{
	int i, len, ctr;
	char *new_exp;

	if (str == NULL)
		return NULL;

	len = strlen(str);

	new_exp = create_string(len*2);
	ctr = 0;

	for (i = 0; i < len; i++)
	{
		if (str[i] == '~')
		{
			new_exp[ctr++] = '0';
			new_exp[ctr++] = '~';
		}
		else
		{
			new_exp[ctr++] = str[i];
		}
	}

	new_exp[ctr] = '\0';

	return new_exp;
}

char evaluate_postfix_exp(char *postfix_exp)
{
	int i, len;
	char token, result, value1, value2;
	stack *operand_stack, *operator_stack;

	if (postfix_exp == NULL)
		return ERROR_CHAR;

	len = strlen(postfix_exp);

	operand_stack = create_stack();
	operator_stack = create_stack();

	for (i = 0; i < len; i++)
	{
		token = postfix_exp[i];

		if (is_operand(token))
		{
			push(operand_stack, token);
		}
		else if (is_operator(token))
		{
			value1 = pop(operand_stack);
			value2 = pop(operand_stack);
			result = evaluate(value2, value1, token);
			push(operand_stack, result);
		}
	}

	result = pop(operand_stack);

	free_stack(operator_stack);
	free_stack(operand_stack);
	operator_stack = NULL;
	operand_stack = NULL;

	return result;
}

char evaluate_infix_exp(char *infix_exp)
{
	int len;
	char *postfix_exp, result;

	if (infix_exp == NULL)
		return ERROR_CHAR;

	len = strlen(infix_exp);

	postfix_exp = infix_to_postfix(infix_exp);
	result = evaluate_postfix_exp(postfix_exp);

	free(postfix_exp);
	postfix_exp = NULL;

	return result;
}

void print_result(result *rt)
{
	int i, j;

	if (rt == NULL)
		return;

	if (rt->header == NULL)
		return;

	if (rt->grid == NULL)
		return;

	printf("row_size: %d col_size: %d\n", rt->row_size, rt->col_size);

	for (i = 0; i < rt->col_size-1; i++)
		printf("%s ", rt->header[i]);

	printf("%15s\n", rt->header[rt->col_size-1]);

	for (i = 0; i < rt->row_size; i++)
	{
		for (j = 0; j < rt->col_size-1; j++)
		{
			printf("%c ", rt->grid[i][j]);
		}

		printf("%15c\n", rt->grid[i][rt->col_size-1]);
	}
}

// TODO: Change malloc calls to calloc calls.
result *get_result(char *expressions)
{
	int i, j, k, pos_comb, count, exp_len, var_cnt;
	int *letters;
	char bit, *new_exp, *fixed_exp;
	result *final_result;

	if (expressions == NULL)
		return NULL;

	exp_len = strlen(expressions);

	// Creates a int[LETTER_LEN] of the amount of times
	// a specific letters appears in the original expression.
	// Ex: (a^b)>b^e
	// letters = {1, 2, 0, 0, 1, 0, ...}
	letters = (int*)calloc(LETTERS_LEN, sizeof(int));
	var_cnt = 0;

	for (i = 0; i < exp_len; i++)
	{
		if (expressions[i] >= 'a' && expressions[i] <= 'z')
		{
			if (letters[expressions[i] - 'a'] == 0)
				var_cnt++;

			letters[expressions[i] - 'a']++;			
		}
	}

	final_result = create_result(var_cnt, expressions, exp_len, letters);

	free(letters);

	// Fill up final_result->grid with all possible
	// combinations (pos_comb) of 1s and 0s
	// for each variable. 
	// Ex: p^q
	// p q p^q
	// 1 1		<- 
	// 1 0		<- Fills these up.
	// 0 1		<-
	// 0 0		<-
	pos_comb = power(2, var_cnt);

	for (i = 0; i < var_cnt; i++)
	{
		count = 0;
		bit = '0';
		pos_comb /= 2;

		for (j = 0; j < final_result->row_size; j++)
		{
			if (count++ % pos_comb == 0)
			{
				if (bit == '0')
					bit = '1';
				else
					bit = '0';
			}

			final_result->grid[j][i] = bit;
		}
	}

	// Goes through all possible combinations
	// of the variables, makes an new expression (new_exp)
	// of them, solves the new expression,
	// and puts the result into the final column
	// in final_result.
	new_exp = create_string(exp_len);

	for (i = 0; i < final_result->row_size; i++)
	{
		for (k = 0; k < exp_len; k++)
		{
			new_exp[k] = expressions[k];
		}

		// Fix new_exp
		for (j = 0; j < final_result->col_size-1; j++)
		{
			for (k = 0; k < exp_len; k++)
			{
				if (expressions[k] == final_result->header[j][0])
					new_exp[k] = final_result->grid[i][j];
			}
		}

		fixed_exp = fix_exp(new_exp);
		final_result->grid[i][final_result->col_size - 1] = evaluate_infix_exp(fixed_exp);
		free(fixed_exp);
	}

	free(new_exp);

	return final_result;
}

int main(void)
{
	result *rt = get_result("p^q");
	print_result(rt);
	destroy_result(rt);

	return 0;
}