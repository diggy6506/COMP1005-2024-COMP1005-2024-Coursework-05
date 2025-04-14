#include <ctype.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "queue.h"
#include "rpn.h"
#include "stack.h"

int is_operator(char token) {
  switch (token) {
    case '+':
    case '-':
    case '*':
    case '/':
    case '^':
      return 1;
    default:
      return 0;
  }
}

int operator_precedence(char operator) {
  switch (operator) {
    case '+':
    case '-':
      return 1;
    case '*':
    case '/':
      return 2;
    case '^':
      return 3;
    default:
      return 0;
  }
}

int is_left_associative(char operator) {
  switch (operator) {
    case '^':
      return 0;
    default:
      return 1;
  }
}

int is_number(const char *token) {
  char *endptr;
  strtod(token, &endptr);

  return *endptr == '\0';
}

void process_number(Queue *postfix_queue, const char *token) {
  push_queue(postfix_queue, (void *)token, strlen(token) + 1);
}

void process_operator(Stack *op_stack, Queue *postfix_queue,
                      const char *token) {
  char *top_operator;
  char *popped_operator;

  while (peek_stack(op_stack) != NULL) {
    top_operator = (char *)peek_stack(op_stack);

    if (is_operator(top_operator[0]) &&
        ((is_left_associative(token[0]) &&
          operator_precedence(token[0]) <=
              operator_precedence(top_operator[0])) ||
         (!is_left_associative(token[0]) &&
          operator_precedence(token[0]) <
              operator_precedence(top_operator[0])))) {
      popped_operator = (char *)pop_stack(op_stack);
      push_queue(postfix_queue, popped_operator, strlen(popped_operator) + 1);

      free(popped_operator);
    }

    else {
      break;
    }
  }

  push_stack(op_stack, (void *)token, strlen(token) + 1);
}

void process_parentheses(Stack *operator_stack, Queue *postfix_queue,
                         const char *token) {
  char *popped_operator;

  if (strcmp(token, "(") == 0) {
    push_stack(operator_stack, (void *)token, strlen(token) + 1);
  }

  else {
    while (peek_stack(operator_stack) != NULL &&
           strcmp((char *)peek_stack(operator_stack), "(") != 0) {
      popped_operator = (char *)pop_stack(operator_stack);
      push_queue(postfix_queue, popped_operator, strlen(popped_operator) + 1);

      free(popped_operator);
    }

    if (peek_stack(operator_stack) == NULL) {
      printf("error: unable to parse expression.\n");

      free_stack(operator_stack);
      free_queue(postfix_queue);

      return;
    }

    free(pop_stack(operator_stack));
  }
}

int is_operator_or_parenthesis(char c) {
  return is_operator(c) || c == '(' || c == ')';
}

int tokenize_expression(char *expression, Queue *tokens_queue) {
  char token[100];
  int i = 0, token_index = 0;
  int prev_token_operator = 1;

  while (expression[i] != '\0') {
    if (isdigit(expression[i]) || expression[i] == '.') {
      token[token_index++] = expression[i];
      prev_token_operator = 0;
    }

    else if (is_operator_or_parenthesis(expression[i])) {
      if (token_index > 0) {
        token[token_index] = '\0';
        push_queue(tokens_queue, token, strlen(token) + 1);
        token_index = 0;
      }

      if ((expression[i] == '+' || expression[i] == '-') &&
          prev_token_operator) {
        token[token_index++] = expression[i];
      }

      else {
        token[0] = expression[i];
        token[1] = '\0';

        push_queue(tokens_queue, token, strlen(token) + 1);

        prev_token_operator = (expression[i] != ')');
      }
    }

    else if (!isspace(expression[i])) {
      return 1;
    }

    i++;
  }
  if (token_index > 0) {
    token[token_index] = '\0';
    push_queue(tokens_queue, token, strlen(token) + 1);
  }

  return 0;
}

Queue *infix_to_postfix(char *expression) {
  Stack *op_stack = initialise_stack();
  Queue *postfix_queue = initialise_queue();
  Queue *tokens_queue = initialise_queue();
  char *token;

  if (tokenize_expression(expression, tokens_queue) == 1) {
    free_stack(op_stack);
    free_queue(postfix_queue);
    free_queue(tokens_queue);
    return NULL;
  }

  while ((token = (char *)pop_queue(tokens_queue)) != NULL) {
    if (is_number(token)) {
      process_number(postfix_queue, token);
    }

    else if (is_operator(token[0])) {
      process_operator(op_stack, postfix_queue, token);
    }

    else if (strcmp(token, "(") == 0 || strcmp(token, ")") == 0) {
      process_parentheses(op_stack, postfix_queue, token);
    }

    else {
      printf("error: unable to parse expression.\n");

      free_stack(op_stack);
      free_queue(postfix_queue);
      free_queue(tokens_queue);

      return NULL;
    }

    free(token);
  }

  while (peek_stack(op_stack) != NULL) {
    char *popped_operator = (char *)pop_stack(op_stack);

    if (strcmp(popped_operator, "(") == 0 ||
        strcmp(popped_operator, ")") == 0) {
      free(popped_operator);
      free_stack(op_stack);
      free_queue(postfix_queue);
      free_queue(tokens_queue);
      return NULL;
    }

    push_queue(postfix_queue, popped_operator, strlen(popped_operator) + 1);

    free(popped_operator);
  }

  free_stack(op_stack);
  free_queue(tokens_queue);

  return postfix_queue;
}

double perform_calculation(double operand1, double operand2, char operator) {
  switch (operator) {
    case '+':
      return operand1 + operand2;

    case '-':
      return operand1 - operand2;

    case '*':
      return operand1 * operand2;

    case '/':
      if (operand2 == 0) {
        return HUGE_VAL;
      }
      return operand1 / operand2;

    case '^':
      return pow(operand1, operand2);

    default:
      return HUGE_VAL;
  }
}

double round(double value, int dec_place) {
  double factor = 1.0;
  int i;
  for (i = 0; i < dec_place; i++) {
    factor = factor * 10;
  }

  return floor(value * factor) / factor;
}

double evaluate_rpn(Queue *queue) {
  Stack *stack = initialise_stack();
  double result = 0.0;
  char *token;
  double operand1, operand2, parsed_value;
  double *operand2_ptr;
  double *operand1_ptr;
  double *result_ptr;

  while ((token = (char *)pop_queue(queue)) != NULL) {
    if (isdigit(token[0]) ||
        (token[0] == '-' && (isdigit(token[1]) || token[1] == '.')) ||
        (token[0] == '+' && (isdigit(token[1]) || token[1] == '.'))) {
      parsed_value = atof(token);
      push_stack(stack, &parsed_value, sizeof(double));
    }

    else {
      operand2_ptr = (double *)pop_stack(stack);

      if (operand2_ptr == NULL) {
        free(token);
        free_stack(stack);

        return HUGE_VAL;
      }

      operand2 = *operand2_ptr;
      free(operand2_ptr);

      operand1_ptr = (double *)pop_stack(stack);

      if (operand1_ptr == NULL) {
        free(token);
        free_stack(stack);

        return HUGE_VAL;
      }

      operand1 = *operand1_ptr;
      free(operand1_ptr);

      result = perform_calculation(operand1, operand2, token[0]);

      if (result == HUGE_VAL) {
        free(token);
        free_stack(stack);

        return result;
      }

      push_stack(stack, &result, sizeof(double));
    }

    free(token);
  }

  result_ptr = (double *)pop_stack(stack);

  if (result_ptr == NULL) {
    free_stack(stack);

    return HUGE_VAL;
  }
  result = *result_ptr;

  free(result_ptr);

  if (stack->head != NULL) {
    free_stack(stack);

    return HUGE_VAL;
  }

  free_stack(stack);

  return result;
}

