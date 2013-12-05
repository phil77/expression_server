Definitions.

VAR     = [a-z_]
D       = [0-9]
AOP     = (\+|-)
MOP     = (\*|/|%)
COMP    = (<|<=|==|>=|>|!=)
LOGIC   = (and|or)
WS      = ([\000-\s]|#.*)
OPEN    = \(
CLOSE   = \)
ASSIGN  = =
BOOL    = (true|false)

Rules.

{AOP}           : {token,{add_operator,list_to_atom(TokenChars)}}.
{MOP}           : {token,{mul_operator,list_to_atom(TokenChars)}}.
{D}+            : {token,{integer,list_to_integer(TokenChars)}}.
{D}+\.{D}+      : {token,{float,list_to_float(TokenChars)}}.
{BOOL}          : {token,{boolean,list_to_atom(TokenChars)}}.
{LOGIC}         : {token,{logic,list_to_atom(TokenChars)}}.
{COMP}          : {token,{comparation,list_to_atom(TokenChars)}}.
{OPEN}          : {token,{open,list_to_atom(TokenChars)}}.
{CLOSE}         : {token,{close,list_to_atom(TokenChars)}}.
{ASSIGN}        : {token,{assign,list_to_atom(TokenChars)}}.
{VAR}+          : {token,{var,TokenChars}}.
{WS}+           : skip_token.

Erlang code.