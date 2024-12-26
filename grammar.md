translation-unit:
  external-declaration
  translation-unit external-declaration
external-declaration:
  function-definition
  declaration
function-definition:
  declaration-specifiers_opt declarator declaration-list_opt compound-statement
declaration:
  declaration-specifiers init-declarator-list_opt;
declaration-list:
  declaration
  declaration-list declaration
declaration-specifiers:
  storage-class-specifier declaration-specifiers_opt
  type-specifier declaration-specifiers_opt
  type-qualifier declaration-specifiers_opt
storage-class specifier: one of
  auto register static extern typedef
type specifier: one of
  void char short int long float double signed
  unsigned struct-or-union-specifier enum-specifier typedef-name
type-qualifier: one of
  const volatile

struct-or-union-specifier:
  struct-or-union identifier_opt { struct-declaration-list }
  struct-or-union identifier
struct-or-union: one of
  struct union
struct-declaration-list:
  struct declaration
  struct-declaration-list struct declaration
init-declarator-list:
  init-declarator
  init-declarator-list, init-declarator
init-declarator:
  declarator
  declarator = initializer
struct-declaration:
  specifier-qualifier-list struct-declarator-list;
specifier-qualifier-list:
  type-specifier specifier-qualifier-list_opt
  type-qualifier specifier-qualifier-list_opt
struct-declarator-list:
  struct-declarator
  struct-declarator-list , struct-declarator
struct-declarator:
  declarator
declarator_opt : constant-expression
enum-specifier:
  enum identifier_opt { enumerator-list }
  enum identifier
enumerator-list:
  enumerator
  enumerator-list , enumerator
enumerator:
  identifier
  identifier = constant-expression

declarator:
  pointer_opt direct-declarator
direct-declarator:
  identifier
  (declarator)
  direct-declarator [ constant-expression_opt ]
  direct-declarator ( parameter-type-list )
  direct-declarator ( identifier-list_opt )
pointer:
  * type-qualifier-list_opt
  * type-qualifier-list_opt pointer
type-qualifier-list:
  type-qualifier
  type-qualifier-list type-qualifier
parameter-type-list:
  parameter-list
  parameter-list , ...
parameter-list:
  parameter-declaration
  parameter-list , parameter-declaration
parameter-declaration:
  declaration-specifiers declarator
  declaration-specifiers abstract-declarator_opt
identifier-list:
  identifier
  identifier-list , identifier
initializer:
  assignment-expression
  { initializer-list }
  { initializer-list , }
initializer-list:
  initializer
  initializer-list , initializer
type-name:
  specifier-qualifier-list abstract-declarator_opt
abstract-declarator:
  pointer
  pointer_opt direct-abstract-declarator
direct-abstract-declarator:
  ( abstract-declarator )
  direct-abstract-declarator_opt [constant-expression_opt]
  direct-abstract-declarator_opt (parameter-type-list_opt)
typedef-name:
  identifier

statement:
  labeled-statement
  expression-statement
  compound-statement
  selection-statement
  iteration-statement
  jump-statement
labeled-statement:
identifier : statement
case constant-expression : statement
default : statement
expression-statement:
  expression_opt;
compound-statement:
  { declaration-list_opt statement-list_opt }
statement-list:
  statement
  statement-list statement
selection-statement:
  if (expression) statement
  if (expression) statement else statement
  switch (expression) statement
iteration-statement:
  while (expression) statement
  do statement while (expression);
  for (expression_opt; expression_opt; expression_opt) statement
jump-statement:
  goto identifier;
  continue;
  break;
  return expression_opt;

expression:
  assignment-expression
  expression , assignment-expression
assignment-expression:
  constant-expression
  unary-expression assignment-operator assignment-expression
assignment-operator: one of
  = *= /= %= += -= <<= >>= &= ^= |=
constant-expression:
  logical-OR-expression
  logical-OR-expression ? expression : constant-expression
logical-OR-expression:
  logical-AND-expression
  logical-OR-expression || logical-AND-expression
logical-AND-expression:
  inclusive-OR-expression
  logical-AND-expression && inclusive-OR-expression
inclusive-OR-expression:
  exclusive-OR-expression
  inclusive-OR-expression | exclusive-OR-expression
exclusive-OR-expression:
  AND-expression
  exclusive-OR-expression ^ AND-expression
AND-expression:
  equality-expression
  AND-expression & equality-expression
equality-expression:
  relational-expression
  equality-expression == relational-expression
  equality-expression != relational-expression
relational-expression:
  shift-expression
  relational-expression < shift-expression
  relational-expression > shift-expression
  relational-expression <= shift-expression
  relational-expression >= shift-expression
shift-expression:
  additive-expression
  shift-expression << additive-expression
  shift-expression >> additive-expression
additive-expression:
  multiplicative-expression
  additive-expression + multiplicative-expression
  additive-expression - multiplicative-expression
multiplicative-expression:
  cast-expression
  multiplicative-expression * cast-expression
  multiplicative-expression / cast-expression
  multiplicative-expression % cast-expression
cast-expression:
  unary-expression
  (type-name) cast-expression
unary-expression:
  postfix expression
  ++unary-expression
  --unary-expression
  unary-operator cast-expression
  sizeof unary-expression
  sizeof (type-name)
unary-operator: one of
  & * + - ~ !
postfix-expression:
  primary-expression
  postfix-expression[expression]
  postfix-expression(expression_opt)
  postfix-expression.identifier
  postfix-expression->+identifier
  postfix-expression++
  postfix-expression--
primary-expression:
  identifier
  constant
  string
  (expression)

constant:
  integer-constant
  character-constant
  floating-constant
  enumeration-constant
