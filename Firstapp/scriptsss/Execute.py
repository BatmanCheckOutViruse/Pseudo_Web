import copy
from .strings_with_arrows import string_with_arrows
import string
from pathlib import Path
##################################
# TOKEN
##################################
import os

TT_INT = 'INT'
TT_STRING = 'STRING'
TT_IDENTIFIER = 'IDENTIFIER'
TT_KEYWORD = 'KEYWORD'
TT_BOOLEAN = 'BOOLEAN'
TT_DOT = '.'
TT_EQ = 'EQ'
TT_LSQUARE = 'LSQUARE'
TT_RSQUARE = 'RSQUARE'
TT_LCURLYB = 'LCURLYB'
TT_RCURLYB = 'RCURLYB'
TT_FLOAT = 'FLOAT'
TT_PLUS = 'PLUS'
TT_MINUS = 'MINUS'
TT_MUL = 'MUL'
TT_DIV = 'DIV'
TT_LPAREN = 'LARPEN'
TT_RPAREN = 'RPAREN'
TT_EE = 'EE'
TT_NE = 'NE'
TT_NEWLINE = 'NEWLINE'
TT_COMMA = 'COMMA'
TT_COLON = 'COLON'
TT_LT = 'LT'
TT_GT = 'GT'
TT_LTE = 'LTE'
TT_GTE = 'GTE'
TT_EOF = 'EOF'


class Token:
    def __init__(self, type_, value=None, pos_start=None, pos_end=None):
        self.type = type_
        self.value = value


        if pos_start:
            self.pos_start = pos_start.copy()
            self.pos_end = pos_start.copy()
            self.pos_end.advance()

        if pos_end:
            self.pos_end = pos_end.copy()

    def matches(self, type_, value):
        if type(value) == tuple:
            return self.type == type_ and (self.value in value)
        else:
            return self.type == type_ and self.value == value

    def __repr__(self):
        if self.value: return f'{self.type}:{self.value}'
        return f'{self.type}'


##################################
# CONSTANT
##################################
DIGITS = '01234567890'
LETTERS = string.ascii_letters
LETTERS_DIGITS = LETTERS + DIGITS
KEYWORDS = [
    'ENDCASE',
    'CASEWHERE',
    'REPEAT',
    'UNTIL',
    'AND',
    'OR',
    'NOT',
    'IF',
    'ELSE IF',
    'ELSE',
    'ENDIF',
    'THEN',
    'FOR',
    'NEXT',
    'STEP',
    'WHILE',
    'ENDWHILE',
    'TO',
    'BEGIN',
    'RETURN',
    'END',
    "OTHERWISE"
]

##################################
# CONTEXT
##################################
class Context:
    def __init__(self, display_name , parent= None,parent_entry_pos= None) :
        self.display_name = display_name
        self.parent = parent
        self.parent_entry_pos = parent_entry_pos
        self.symbol_table = None

##################################
# RUNTIME RESULT
##################################
class RTResult:
    def __init__(self):
        self.reset()

    def reset(self):
        self.value = None
        self.error = None
        self.func_return_value = None

    def register(self,res):
        if res.error: self.error = res.error
        self.func_return_value = res.func_return_value
        return res.value

    def success(self, value):
        self.reset()
        self.value = value
        return self
    def success_return(self, value):
        self.reset()
        self.func_return_value = value
        return self
    def failure(self,error ):
        self.reset()
        self.error = error
        return self

    def should_return(self):
        return (
            self.error or self.func_return_value
        )
##################################
# ERRORS
##################################
class Error:

    def __init__(self, pos_start, pos_end, error_name, details):
        self.pos_start = pos_start
        self.pos_end = pos_end
        self.error_name = error_name
        self.details = details

    def as_string(self):
        result = f'{self.error_name} : {self.details}'
        if self.pos_start != None and self.pos_end != None:
            result += f'\n line :{self.pos_start.line + 1}'
            result += '\n\n' + string_with_arrows(self.pos_start.filetxt, self.pos_start, self.pos_end)
        return result


class IlleagalCharError(Error):
    def __init__(self, pos_start, pos_end, details ):
        super().__init__(pos_start, pos_end, 'Illegal Character', details)

class ExpectedCharError(Error):
    def __init__(self, pos_start, pos_end, details):
        super().__init__(pos_start, pos_end, 'Expected Character', details)


class InvalidSyntaxError(Error):
    def __init__(self, pos_start, pos_end, details ):
        super().__init__(pos_start, pos_end, 'InvalidSyntaxError', details)

class RTError(Error):
    def __init__(self, pos_start, pos_end,details, context):
        super().__init__(pos_start, pos_end, 'Runtime error' , details )
        self.context = context

    def as_string(self):
        result = self.generate_traceback()
        result += '\n' + f'{self.error_name} : {self.details}'
        if self.pos_start != None and self.pos_end != None:
            result += '\n\n' + string_with_arrows(self.pos_start.filetxt, self.pos_start, self.pos_end)
        return result

    def generate_traceback(self):
        result = ''
        pos = self.pos_start
        ctx = self.context

        while ctx:
            result = f'\n  line :{self.pos_start.line + 1}, in {ctx.display_name}\n' + result
            pos = ctx.parent_entry_pos
            ctx = ctx.parent

        return 'Traceback (most recent call last) :' + result
##################################
# POSTITION
##################################

class Position:
    def __init__(self, index, line, colum, filetxt):
        self.index = index
        self.line = line
        self.colum = colum
        self.filetxt = filetxt

    def advance(self, current_char=None):
        self.index += 1
        self.colum += 1

        if current_char == '\n':
            self.line += 1
            self.colum = 0

        return self

    def copy(self):
        return Position(self.index, self.line, self.colum, self.filetxt)


##################################
# LEXER
##################################

class Lexer:
    def __init__(self, filetxt):
        self.text = filetxt
        self.pos = Position(-1, 0, -1, filetxt)
        self.current_Char = None
        self.advance()

    def advance(self):
        # advance to the next characteer by changing the pos value
        self.pos.advance(self.current_Char)
        self.current_Char = self.text[self.pos.index] if self.pos.index < len(self.text) else None

    def make_tokens(self):
        # to determine the type and amount of tokens
        tokens = []
        while self.current_Char != None:
            if self.current_Char in ' \t':
                self.advance()

            elif self.current_Char in LETTERS:
                tokens.extend(self.make_identifier())

            elif self.current_Char == '.':

                # check if user is calling the attribute of a varaible
                while  self.current_Char == '.':

                    tokens.append(Token(TT_DOT, pos_start=self.pos))
                    self.advance()

                    # get the identifier of attribute
                    if self.current_Char in LETTERS:
                        tokens.extend(self.make_identifier())

                        # check if it is a method - idenitfier.identifier(
                        if self.current_Char ==  '(':
                            tokens.append(Token(TT_LPAREN, pos_start=self.pos))
                            self.advance()

                            # check if it is a method - idenitfier.identifier()
                            if self.current_Char == ')':
                                tokens.append(Token(TT_RPAREN, pos_start =self.pos))
                                self.advance()
                            else:
                                return [], ExpectedCharError(self.pos, self.pos, "Expected ')'")

                    else:
                        return [],ExpectedCharError(self.pos , self.pos , "Expected an identifier")

            elif self.current_Char in DIGITS:
                tokens.append(self.make_numbers())

            elif self.current_Char == '=':
                tokens.append(self.make_equals())

            elif self.current_Char == '+':
                tokens.append(Token(TT_PLUS, pos_start=self.pos))
                self.advance()

            elif self.current_Char == ':':
                tokens.append(Token(TT_COLON, pos_start=self.pos))
                self.advance()

            elif self.current_Char == '-':
                tokens.append(Token(TT_MINUS, pos_start=self.pos))
                self.advance()

            elif self.current_Char == '*':
                tokens.append(Token(TT_MUL, pos_start=self.pos))
                self.advance()

            elif self.current_Char == '/':
                tokens.append(Token(TT_DIV, pos_start=self.pos))
                self.advance()

            elif self.current_Char == '"':
                token = self.make_string()
                if self.current_Char != '"':
                    return [], ExpectedCharError(token.pos_start, self.pos, 'Expected " at the end of string')
                tokens.append(token)
                self.advance()

            elif self.current_Char == '(':
                tokens.append(Token(TT_LPAREN, pos_start=self.pos))
                self.advance()

            elif self.current_Char == ')':
                tokens.append(Token(TT_RPAREN, pos_start=self.pos))
                self.advance()

            elif self.current_Char == '!':
                tok ,error = self.make_not_equals()
                if error: return [], error
                tokens.append(tok)

            elif self.current_Char == '<':
                tokens.append(self.make_less_than())

            elif self.current_Char == '>':
                tokens.append(self.make_greater_than())

            elif self.current_Char == ',':
                tokens.append(Token(TT_COMMA, pos_start=self.pos))
                self.advance()

            elif self.current_Char == '[':
                tokens.append(Token(TT_LSQUARE, pos_start=self.pos))
                self.advance()

            elif self.current_Char == ']':
                tokens.append(Token(TT_RSQUARE, pos_start=self.pos))
                self.advance()

            elif self.current_Char in ';\n':
                tokens.append(Token(TT_NEWLINE, pos_start=self.pos))
                self.advance()

            elif self.current_Char == '{':
                tokens.append(Token(TT_LCURLYB, pos_start=self.pos))
                self.advance()

            elif self.current_Char == '}':
                tokens.append(Token(TT_RCURLYB, pos_start=self.pos))
                self.advance()

            else:
                # return some error
                self.advance()
                return [], IlleagalCharError(self.pos.copy(), self.pos, "'" + self.current_Char + "'")
        tokens.append(Token(TT_EOF, pos_start=self.pos))
        return tokens, None

    def make_identifier(self):
        id_str = ''
        pos_start = self.pos.copy()

        while self.current_Char != None and self.current_Char in LETTERS_DIGITS + '_':
            id_str += self.current_Char
            self.advance()

        #'save' down the current posotion and character
        now_pos = self.pos
        now_char = self.current_Char

        #Check for a special case 'ELSE IF'
        if id_str == 'ELSE':

            id_str += ' '
            if self.current_Char in ' ' and self.current_Char != None:
                self.advance()

            while self.current_Char != None and self.current_Char != ' 'and  self.current_Char in LETTERS_DIGITS + '_':
                id_str += self.current_Char
                self.advance()

            ## reverse the action if it is not 'ElSE IF'
            if id_str != "ELSE IF":
                id_str = "ELSE"
                self.pos = now_pos
                self.char = now_char


        # Check if it is a Boolean
        if id_str == "True" or id_str == 'False':
            id_str = (True if id_str == "True" else False)
            tok_type = TT_BOOLEAN

        elif id_str in KEYWORDS:
            tok_type = TT_KEYWORD
        else:
            tok_type = TT_IDENTIFIER
        return [Token(tok_type, id_str, pos_start, self.pos)]

    def make_string(self):
        string = ''
        pos_start = self.pos.copy()
        self.advance()

        while self.current_Char != None and (self.current_Char != '"' ):
            string += self.current_Char
            self.advance()
        return Token(TT_STRING, string , pos_start,self.pos )

    def make_numbers(self):
        num_str = ''
        dot_count = 0
        pos_start = self.pos.copy()

        while self.current_Char != None and self.current_Char in DIGITS + '.':

            if self.current_Char == '.':  # It will be a float number

                if dot_count == 1: break  # cant have more than one dot
                dot_count += 1
                num_str += '.'

            else:
                num_str += self.current_Char
            self.advance()

        if dot_count == 0:
            return Token(TT_INT, int(num_str), pos_start, self.pos)
        else:
            return Token(TT_FLOAT, float(num_str), pos_start, self.pos)

    def make_not_equals(self):
        pos_start = self.pos.copy()
        self.advance()

        if self.current_Char == '=':
            self.advance()
            return Token(TT_NE , pos_start=pos_start , pos_end = self.pos) , None

        self.advance()
        return None , ExpectedCharError(pos_start, self.pos , "= after !")

    def make_equals(self):
        tok_type = TT_EQ
        pos_start = self.pos.copy()
        self.advance()

        if self.current_Char == '=':
            self.advance()
            tok_type = TT_EE

        return Token(tok_type , pos_start= pos_start, pos_end= self.pos)

    def make_less_than(self):
        tok_type = TT_LT
        pos_start = self.pos.copy()
        self.advance()

        if self.current_Char == '=':
            self.advance()
            tok_type = TT_LTE

        return Token(tok_type , pos_start= pos_start, pos_end= self.pos)

    def make_greater_than(self):
        tok_type = TT_GT
        pos_start = self.pos.copy()
        self.advance()

        if self.current_Char == '=':
            self.advance()
            tok_type = TT_GTE

        return Token(tok_type , pos_start= pos_start, pos_end= self.pos)
########################################################################


##################################
# NODES
##################################
class FloatNode:
    def __init__(self, tok):
        self.tok = tok
        self.pos_start = self.tok.pos_start
        self.pos_end = self.tok.pos_end

    def __repr__(self):
        return f'{self.tok}'

class NumberNode:
    def __init__(self, tok):
        self.tok = tok
        self.pos_start = self.tok.pos_start
        self.pos_end = self.tok.pos_end

    def __repr__(self):
        return f'{self.tok}'

class StringNode:
    def __init__(self, tok):
        self.tok = tok
        self.pos_start = self.tok.pos_start
        self.pos_end = self.tok.pos_end

    def __repr__(self):
        return f'{self.tok}'

class ForNode:
    def __init__(self, var_name_tok , start_value_node ,end_value_node, step_value_node , body_node):
        self.var_name_tok = var_name_tok
        self.start_value_node = start_value_node

        self.end_value_node = end_value_node
        self.step_value_node = step_value_node
        self.body_node = body_node

        self.pos_start = self.var_name_tok.pos_start
        self.pos_end = self.body_node.pos_end

class IfNode:
    def __init__(self, cases , else_case):
        self.cases = cases
        self.else_case = else_case

        self.pos_start = self.cases[0][0].pos_start
        self.pos_end = (self.else_case or self.cases[len(self.cases) -1 ][0]).pos_end
class CasewhereNode:
    def __init__(self, cases , var_node):
        self.cases = cases
        self.var_node = var_node

        self.pos_start = self.cases[0][0].pos_start
        self.pos_end = self.cases[len(cases) -1][1].pos_end

class WhileNode:
    def __init__(self, condition_node , body_node ):
        self.condition_node = condition_node
        self.body_node = body_node

        self.pos_start = self.condition_node.pos_start
        self.pos_end = self.body_node.pos_end

class RepeatNode:
    def __init__(self,condition_node , body_node ):
        self.condition_node = condition_node
        self.body_node = body_node

        #condtion is at the end of loop so pos_end is the pos_end of loop
        self.pos_start = self.body_node.pos_start
        self.pos_end = self.condition_node.pos_end

class BinOpNode:
    def __init__(self, left_node, op_tok, right_node):
        self.left_node = left_node
        self.op_tok = op_tok
        self.right_node = right_node

        self.pos_start = self.left_node.pos_start
        self.pos_end = self.right_node.pos_end

    def __repr__(self):
        return f'({self.left_node}, {self.op_tok}, {self.right_node})'

class BooleanNode:
    def __init__(self, tok):
        self.tok = tok
        self.pos_start = self.tok.pos_start
        self.pos_end = self.tok.pos_end
        self.value = tok.value

    def __repr__(self):
        return f'{self.tok}'

class UnaryOpNode:
    def __init__(self, op_tok, node):
        self.op_tok = op_tok
        self.node = node

        self.pos_start = self.op_tok.pos_start
        self.pos_end = node.pos_end
    def __repr__(self):
        return f'({self.op_tok}, {self.node})'

class FuncDefNode:
    def __init__(self, var_name_toks , args_name_tok , body_node ):
        self.var_name_toks = var_name_toks
        self.args_name_tok = args_name_tok
        self.body_node = body_node

        if self.var_name_toks:
            self.pos_start = self.var_name_toks.pos_start
        elif len(self.args_name_tok) > 0:
            self.pos_start = self.args_name_tok[0].pos_start
        else:
            self.pos_start = self.body_node.pos_start

        self.pos_end = self.body_node.pos_end

class ArrayDefNode:
    def __init__(self, var_name_toks , elements_node_list, pos_start , pos_end):
        self.var_name_toks = var_name_toks
        self.elements_node_list = elements_node_list
        self.pos_start = pos_start
        self.pos_end = pos_end
        self.array_depth = 1

class ArrayAccessNode:
    def __init__(self, var_name_tok , index_node_list, pos_start, pos_end, Check_value , new_element_node=None ):
        self.var_name_tok = var_name_tok
        self.index_node_list = index_node_list
        self.new_element_node  = new_element_node
        self.Check_value = Check_value
        self.pos_start = pos_start
        self.pos_end = pos_end

class AttributeAccessNode:
    def __init__(self, main_tok, attribute_toks):
        self.main_tok = main_tok
        self.attribute_tok = attribute_toks[0]
        self.function_or_not = attribute_toks[1]

        self.pos_start = main_tok.pos_start
        self.pos_end = attribute_toks[0].pos_end
class CallNode:
    def __init__(self, node_to_call, arg_nodes):
        self.node_to_call = node_to_call
        self.arg_nodes = arg_nodes

        self.pos_start = self.node_to_call.pos_start

        if len(self.arg_nodes) > 0 :
            self.pos_end = self.arg_nodes[len(self.arg_nodes) - 1].pos_end
        else:
            self.pos_end = self.node_to_call.pos_end

class VarAccessNode:
    def __init__(self, var_name_tok) :
        self.var_name_tok = var_name_tok
        self.pos_start = self.var_name_tok.pos_start
        self.pos_end = self.var_name_tok.pos_end

class VarAssignNode:
    def __init__(self, var_name_tok , value_node):
        self.var_name_tok = var_name_tok
        self.value_node = value_node
        self.pos_start = self.var_name_tok.pos_start
        self.pos_end = self.value_node.pos_end

class ReturnNode:
    def __init__(self, node_to_return, pos_start, pos_end):
        self.node_to_return = node_to_return
        self.pos_start = pos_start
        self.pos_end = pos_end
##################################
# PARASER RESULT
##################################
class ParseResult:
    def __init__(self):
        self.error = None
        self.node = None
        self.advance_count = 0

    def register_advancement(self):
        self.advance_count += 1

    def register(self, res):
        self.advance_count += res.advance_count
        if res.error: self.error = res.error
        return res.node

    def success(self, node):
        self.node = node
        return self

    def failure(self, error):
        # if no error in current result or current result havn't advance yet , then error happen in other place
        if not self.error or self.advance_count == 0:
            self.error = error
        return self


##################################
# PARASER
##################################
class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.tok_idx = -1
        self.previous_tok = None
        self.advance()

    def advance(self):
        self.tok_idx += 1
        if self.tok_idx >= 1:
            self.previous_tok = self.current_tok
        if self.tok_idx < len(self.tokens):
            self.current_tok = self.tokens[self.tok_idx]
        return self.current_tok

    def update_current_tok(self):
        if self.tok_idx >= 0 and self.tok_idx < len(self.tokens):
            self.current_tok = self.tokens[self.tok_idx]

    def Factor(self):
        res = ParseResult()
        tok = self.current_tok
        if self.current_tok.type in (TT_PLUS, TT_MINUS): # check for unary op
            res.register_advancement()
            self.advance()

            if self.current_tok.type in (TT_INT, TT_FLOAT): # e.g -4
                number = self.current_tok
                res.register_advancement()
                self.advance()
                return res.success(UnaryOpNode(tok, NumberNode(number)))

            elif self.current_tok.type == TT_IDENTIFIER:  # e.g -b
                id = self.current_tok
                res.register_advancement()
                self.advance()
                return res.success(UnaryOpNode(tok, VarAccessNode(id)))

            elif self.current_tok.type == TT_LPAREN: #e.g -(-4)
                res.register_advancement()
                self.advance()
                expression = res.register(self.expression())
                if res.error: return res
                if self.current_tok.type == TT_RPAREN:
                    res.register_advancement()
                    self.advance()
                    return res.success(UnaryOpNode(tok, expression))


                else:
                    return res.failure(ExpectedCharError(
                        self.current_tok.pos_start, self.current_tok.pos_end, "Expected ')' "))
            else: # --4 is invalid
                return res.failure(InvalidSyntaxError(
                    tok.pos_start, tok.pos_end, "Invalid sign"
                ))

        elif self.current_tok.type == TT_STRING:
            res.register_advancement()
            self.advance()
            return res.success(StringNode(tok))

        elif self.current_tok.type == TT_BOOLEAN:
            res.register_advancement()
            self.advance()
            return res.success(BooleanNode(tok))

        elif self.current_tok.type == TT_IDENTIFIER:

            var_name_tok = self.current_tok

            res.register_advancement()
            self.advance()

            # Check for the built-in method first (they don't required LPAREN to execute)
            if var_name_tok.matches(TT_IDENTIFIER, "Get"):
                value = res.register(self.call(VarAccessNode(var_name_tok)))
                if res.error: return res
                return res.success(value)

            elif var_name_tok.matches(TT_IDENTIFIER, "Display"):
                value = res.register(self.call(VarAccessNode(var_name_tok)))
                if res.error: return res
                return res.success(value)

            #Check for other method , identifier()
            elif self.current_tok.type == TT_LPAREN:
                value =  res.register(self.call(VarAccessNode(var_name_tok)))
                if res.error: return res
                return res.success(value)

            # Check for an error (e.g identifier identifier)
            elif self.current_tok.type == TT_IDENTIFIER:
                return res.failure(InvalidSyntaxError(
                    var_name_tok.pos_start, self.current_tok.pos_end,
                    "Invalid placement of an identifier"
                ))

            #identider[ can be defining an array or acessing an array
            elif self.current_tok.type == TT_LSQUARE:
                pos_start = self.current_tok.pos_start.copy()
                res.register_advancement()
                self.advance()
                #e.g if it is a[] instead of a[number] than it is declaring an array
                if self.current_tok.type == TT_RSQUARE:
                    array = res.register(self.make_array(var_name_tok, pos_start))
                    if res.error : return res
                    return res.success(array)
                else:
                    ArrayAccessNode_ = res.register(self.access_array(var_name_tok, pos_start))
                    if res.error: return res

                    #Check if acessing attribute(array of record)
                    if self.current_tok.type == TT_DOT:
                        AttributeAccessNode_ = self.make_attribute(ArrayAccessNode_)
                        return res.success(AttributeAccessNode_)
                    return res.success(ArrayAccessNode_)

            #identifer. can only be accessing an attribute
            elif self.current_tok.type == TT_DOT:
                AttributeAccessNode_ = self.make_attribute(var_name_tok)
                return res.success(AttributeAccessNode_)

            return res.success(VarAccessNode(tok))

        elif self.current_tok.type == TT_INT:
            res.register_advancement()
            self.advance()
            return res.success(NumberNode(tok))

        elif self.current_tok.type == TT_FLOAT:
            res.register_advancement()
            self.advance()
            return res.success(FloatNode(tok))

        elif self.current_tok.type == TT_LPAREN:
            res.register_advancement()
            self.advance()
            expression = res.register(self.expression())
            if res.error: return res
            if self.current_tok.type == TT_RPAREN:
                res.register_advancement()
                self.advance()
                return res.success(expression)

            else:
                return res.failure(ExpectedCharError(
                    self.current_tok.pos_start, self.current_tok.pos_end, "Expected ')' "))

        elif self.current_tok.matches(TT_KEYWORD, 'IF'):
            if_expr = res.register(self.if_expr())
            if res.error: return res
            return res.success(if_expr)

        elif self.current_tok.matches(TT_KEYWORD, 'FOR'):
            for_expr = res.register(self.for_expr())
            if res.error: return res
            return res.success(for_expr)

        elif self.current_tok.matches(TT_KEYWORD, 'CASEWHERE'):
            casewhere_expr = res.register(self.casewhere_expr())
            if res.error: return res
            return res.success(casewhere_expr)

        elif self.current_tok.matches(TT_KEYWORD, 'WHILE'):
            while_expr = res.register(self.while_expr())
            if res.error: return res
            return res.success(while_expr)

        elif self.current_tok.matches(TT_KEYWORD, 'REPEAT'):
            repeat_expr = res.register(self.repeat_expr())
            if res.error: return res
            return res.success(repeat_expr)

        return res.failure(InvalidSyntaxError(
            self.current_tok.pos_start, self.current_tok.pos_end, "1Expected int, float, identifier, 'IF', 'FOR', 'WHILE' ,'+', '-' or '('"
        ))

    ''''
        def parse(self):
           res = self.statements()
            if not res.error and self.current_tok.type != TT_EOF:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    "Expected '+','-','*' or '/' (Unable to find EOF)"
                ))

            if not res.error:
                for x in res.node.elements_node_list:
                    if type(x) != FuncDefNode:
                        res.error = InvalidSyntaxError(x.pos_start , x.pos_end , "Out of the scope")

            return res
    '''
    def make_2D_array(self, pos_start):
        res = ParseResult()
        element_node_list = []

        if self.current_tok.type != TT_LCURLYB:
            return res.failure(ExpectedCharError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected '{'"
            ))

        res.register_advancement()
        self.advance()

        while self.current_tok.type != TT_RCURLYB:

            node = res.register(self.expression())
            if res.error : return res

            #prevent some weird things stored in the array
            if isinstance(node, IfNode) or isinstance(node , WhileNode) or isinstance(node , ForNode):
                return res.failure(InvalidSyntaxError(
                            self.current_tok.pos_start , self.current_tok.pos_end,
                            "Invalid element in array"
                        ))
            element_node_list.append(node)


            if self.current_tok.type != TT_RCURLYB:
                if self.current_tok.type != TT_COMMA:
                    return res.failure(InvalidSyntaxError(
                        self.current_tok.pos_start, self.current_tok.pos_end,
                        "Expected ',' or '}'"
                    ))
                else:
                    res.register_advancement()
                    self.advance()

        res.register_advancement()
        self.advance()

        result = ArrayDefNode(True, element_node_list, pos_start, self.current_tok.pos_end.copy())
        result.array_depth  = result.array_depth + 1
        return res.success(result)

    def make_attribute(self, var_name_tok):
        res = ParseResult()
        result = None

        while self.current_tok.type == TT_DOT:  # variable.attribute.length()

            res.register_advancement()
            self.advance()

            if self.current_tok.type != TT_IDENTIFIER:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    "Expected identifier"
                ))

            attribute_token = self.current_tok

            res.register_advancement()
            self.advance()

            if self.current_tok.type == TT_LPAREN:
                res.register_advancement()
                self.advance()
                if self.current_tok.type != TT_RPAREN:
                    return res.failure(InvalidSyntaxError(
                        self.current_tok.pos_start, self.current_tok.pos_end,
                        "Expected ')'"
                    ))

                res.register_advancement()
                self.advance()

                attribute_token = [attribute_token, True]  # method
            else:
                attribute_token = [attribute_token, False]  # attribute
            result = AttributeAccessNode(var_name_tok, attribute_token)
        return result

    def make_array(self, var_name, pos_start):
        res = ParseResult()
        element_node_list = []

        if self.current_tok.type != TT_RSQUARE:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected ']' or an index number"
            ))

        res.register_advancement()
        self.advance()

        if self.current_tok.type != TT_EQ:
            return res.failure(ExpectedCharError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected '=' (Error when making array)"
            ))

        res.register_advancement()
        self.advance()

        if self.current_tok.type != TT_LCURLYB:
            return res.failure(ExpectedCharError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected '{'"
            ))

        res.register_advancement()
        self.advance()

        while self.current_tok.type != TT_RCURLYB:

            #check if 2d array
            if self.current_tok.type == TT_LCURLYB:
                node = res.register(self.make_2D_array(self.current_tok.pos_start))
            else:
                node = res.register(self.expression())
            if res.error : return res

            # check whether weird things is stored in the array
            if isinstance(node, IfNode) or isinstance(node , WhileNode) or isinstance(node , ForNode):
                return res.failure(InvalidSyntaxError(
                            self.current_tok.pos_start , self.current_tok.pos_end,
                            "Invalid element in array"
                        ))
            element_node_list.append(node)


            if self.current_tok.type != TT_RCURLYB:
                if self.current_tok.type != TT_COMMA:
                    return res.failure(InvalidSyntaxError(
                        self.current_tok.pos_start, self.current_tok.pos_end,
                        "Expected ',' or '}'"
                    ))
                else:
                    res.register_advancement()
                    self.advance()

        result = ArrayDefNode(var_name , element_node_list, pos_start , self.current_tok.pos_end.copy())
        if len(element_node_list) > 0 and isinstance(element_node_list[0], ArrayDefNode):
            result.array_depth = result.array_depth + element_node_list[0].array_depth

        res.register_advancement()
        self.advance()


        return res.success(result)

    def access_array(self, var_name, pos_start):
        res= ParseResult()
        index_node_list = []
        index_node_list.append(res.register(self.expression()))
        #index_node = res.register(self.expression())
        #index_node_2 = None

        ## check whether user is changing array or referencing array
        Check_value = False
        if res.error: return res

        if self.current_tok.type != TT_RSQUARE:
            return res.failure(ExpectedCharError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected ']'"
            ))

        res.register_advancement()
        self.advance()

        #Check 2D array
        while self.current_tok.type == TT_LSQUARE:
            res.register_advancement()
            self.advance()

            index_node_list.append(res.register(self.expression()))

            if res.error: return res
            if self.current_tok.type != TT_RSQUARE:
                return res.failure(ExpectedCharError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    "Expected ']'"
                ))
            res.register_advancement()
            self.advance()


        #It is not allow to add new e element in an array now
        new_element_node = None

        # If the a[] is follow by an equal sing(changing array)
        if self.current_tok.type == TT_EQ:
            Check_value = True
            res.register_advancement()
            self.advance()

            new_element_node = res.register(self.expression())
            if res.error: return res
            if isinstance(new_element_node , IfNode) or isinstance(new_element_node, WhileNode) or isinstance(new_element_node, ForNode) :
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    "Invalid element of array"
                ))

        return res.success(ArrayAccessNode(var_name, index_node_list , pos_start, self.current_tok.pos_end.copy(),Check_value,new_element_node))

    def call(self, node_to_call):
            res = ParseResult()

            if self.current_tok.type == TT_LPAREN:
                res.register_advancement()
                self.advance()
                arg_nodes = []

                if self.current_tok.type == TT_RPAREN:
                    res.register_advancement()
                    self.advance()

                else:
                    arg_nodes.append(res.register(self.expression()))
                    if res.error:
                        return res.failure(InvalidSyntaxError(
                            self.current_tok.pos_start , self.current_tok.pos_end,
                            "Expected ')', 'IF', 'FOR', 'WHILE', int , float , identifier "
                        ))
                    while self.current_tok.type == TT_COMMA:
                        res.register_advancement()
                        self.advance()

                        arg_nodes.append(res.register(self.expression()))
                        if res.error : return res

                    if self.current_tok.type != TT_RPAREN:
                        return res.failure(InvalidSyntaxError(
                            self.current_tok.pos_start, self.current_tok.pos_end,
                            "Expected ',' or ')' "
                        ))

                    res.register_advancement()
                    self.advance()
                return res.success(CallNode(node_to_call , arg_nodes))

            #Then check for built-in method Display
            elif node_to_call.var_name_tok.value == "Display":

                arg_node = res.register(self.expression())
                if res.error:
                    return res.failure(InvalidSyntaxError(
                        self.current_tok.pos_start, self.current_tok.pos_end,
                        "Expected ')', 'IF', 'FOR', 'WHILE', int , float , identifier "
                    ))

                return res.success(CallNode(node_to_call , [arg_node]))

            # Check for the built-in method Get
            elif node_to_call.var_name_tok.value == "Get":

                # There must be a identifeir follow by get method or else it is wrong syntax
                if self.current_tok.type != TT_IDENTIFIER:
                    return res.failure(InvalidSyntaxError(
                        node_to_call.var_name_tok.pos_start, node_to_call.var_name_tok.pos_end,
                        "Expected an identifier after Get"
                    ))

                var_to_be_assigned = self.current_tok

                res.register_advancement()
                self.advance()

                return res.success(CallNode(node_to_call, [StringNode(
                    var_to_be_assigned)]))  ## We list it because the syntax of call node required a list of argnodes
            return res.failure(InvalidSyntaxError(node_to_call.var_name_tok.pos_start, node_to_call.var_name_tok.pos_end,
                        "1Invalid Syntax"))


    def Term(self):
        return self.bin_op(self.Factor, (TT_MUL, TT_DIV))

    def repeat_expr(self):
        res = ParseResult()

        # First look for the keyword 'REPEAT'
        if not self.current_tok.matches(TT_KEYWORD, 'REPEAT'):
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end, "Expected REPEAT"
            ))

        res.register_advancement()
        self.advance()

        # At least one newline token is needed after the keyword 'REPEAT'
        if self.current_tok.type != TT_NEWLINE:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end, "Expected NEWLINE or invalid statement"
            ))

        res.register_advancement()
        self.advance()

        # get the statements inside the loop
        body_node = res.register(self.statements("UNTIL"))
        if res.error: return res

        # There should be newline before the the keyword 'UNTIL'
        if self.previous_tok.type != TT_NEWLINE:
            return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end, "Expected NEWLINE or invalid statement"
                ))

        # Double check for the keyword
        if self.current_tok.matches(TT_KEYWORD, 'UNTIL') == False:
            return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end, "Expected 'UNTIL'"
                ))

        res.register_advancement()
        self.advance()

        # get the condition of the loop
        condition = res.register(self.expression())

        return res.success(RepeatNode(condition, body_node))

    def casewhere_expr(self):
        res = ParseResult()
        cases=[]
        is_there_otherwise = False

        # First look for the keyword 'CASEWHERE'
        if not self.current_tok.matches(TT_KEYWORD, 'CASEWHERE'):
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end, "Expected CASEWHERE"
            ))

        res.register_advancement()
        self.advance()

        if self.current_tok.type != TT_IDENTIFIER:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end, "Expected Identifier after CASEWHERE"
            ))
        var_tok = self.current_tok

        res.register_advancement()
        self.advance()

        if self.current_tok.type != TT_NEWLINE:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end, "Expected Newline or statement is invalid"
            ))

        while not self.current_tok.matches(TT_KEYWORD, 'ENDCASE'):
            res.register(self.advance_newline())

            #Get the condition
            if not self.current_tok.matches(TT_KEYWORD, "OTHERWISE"):
                condition = res.register(self.expression())
                if res.error : return res
            else:
                condition = None
                res.register_advancement()
                self.advance()
                is_there_otherwise = True

            # Check for the colon
            if self.current_tok.type != TT_COLON:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end, "Expected ':'"
                ))
            res.register_advancement()
            self.advance()

            action = res.register(self.expression())
            if res.error: return res

            cases.append((condition ,action))

            if self.current_tok.type != TT_NEWLINE:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end, "Expected Newline or statement is invalid"
                ))
            res.register(self.advance_newline())

            #Check if token is EOF to prevent unexpected error
            if self.current_tok.type == TT_EOF:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end, "Expected 'ENDCASE'"
                ))
        res.register_advancement()
        self.advance()
        if is_there_otherwise == False:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end, "Expected 'OTHERWISE'"
            ))
        return res.success(CasewhereNode(cases, VarAccessNode(var_tok)))

    def advance_newline(self):
        res = ParseResult()

        while self.current_tok.type == TT_NEWLINE:
            res.register_advancement()
            self.advance()

        return res.success(None)
    def while_expr(self):
        res = ParseResult()

        # First look for the keyword 'WHILE'
        if not self.current_tok.matches(TT_KEYWORD, 'WHILE'):
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end, "Expected WHILE"
            ))

        res.register_advancement()
        self.advance()

        condition = res.register(self.expression())

        # Check for a newline after condition
        if self.current_tok.type != TT_NEWLINE:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end, "Expected NEWLINE or invalid statement"
            ))

        res.register_advancement()
        self.advance()

        # Get the body node of loop
        body_node = res.register(self.statements("ENDWHILE"))
        if res.error: return res

        # Check for the newline before keyword 'ENDWHILE'
        if self.previous_tok.type != TT_NEWLINE:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end, "Expected NEWLINE or invalid statement"
            ))

        # Double Check for the keyword 'ENDWHILE'
        if self.current_tok.matches(TT_KEYWORD, 'ENDWHILE') == False:
            return res.failure(InvalidSyntaxError(
                        self.current_tok.pos_start, self.current_tok.pos_end, "Expected 'ENDWHILE'"
                    ))

        res.register_advancement()
        self.advance()

        return res.success(WhileNode(condition , body_node))


    def for_expr(self):
        res = ParseResult()

        # First look for the keyword 'FOR'
        if not self.current_tok.matches(TT_KEYWORD, 'FOR'):
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end, "Expected FOR"
            ))

        res.register_advancement()
        self.advance()
        if self.current_tok.type != TT_IDENTIFIER:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end, "Expected IDENTIFIER"
                ))
        var_name_tok = self.current_tok

        res.register_advancement()
        self.advance()
        if self.current_tok.type != TT_EQ:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end, "Expected '='"
                ))

        res.register_advancement()
        self.advance()

        start_value_node = res.register(self.expression())
        if res.error: return res

        if self.current_tok.matches(TT_KEYWORD, 'TO') == False:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end, "Expected 'TO'"
            ))

        res.register_advancement()
        self.advance()

        end_value_node = res.register(self.expression())
        if res.error: return res

        if self.current_tok.matches(TT_KEYWORD, 'STEP') == False:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end, "Expected 'STEP'"
            ))

        res.register_advancement()
        self.advance()
        step_value_node = res.register(self.expression())
        if res.error: return res

        if self.current_tok.type == TT_NEWLINE:
            res.register_advancement()
            self.advance()
            body_node = res.register(self.statements("NEXT"))
            if res.error: return res
            if self.previous_tok.type == TT_NEWLINE:
                if self.current_tok.matches(TT_KEYWORD, 'NEXT') == False:
                    return res.failure(InvalidSyntaxError(
                        self.current_tok.pos_start, self.current_tok.pos_end, "Expected 'NEXT'"
                    ))

                res.register_advancement()
                self.advance()

                if self.current_tok.type != TT_IDENTIFIER or self.current_tok.value != var_name_tok.value:
                    return res.failure(InvalidSyntaxError(
                        self.current_tok.pos_start, self.current_tok.pos_end, f"Expected {var_name_tok.value}"
                    ))

                res.register_advancement()
                self.advance()
            else:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end, "Expected NEWLINE or invalid statement"
                ))
        else:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end, "Expected NEWLINE or invalid statement"
            ))
        return res.success(ForNode(var_name_tok,start_value_node,  end_value_node ,step_value_node , body_node))

    # As IF , ELIF and ELSE have similar syntax so this function is reusable
    def if_expr_cases(self, case_keyword):
        res = ParseResult()
        cases = []
        else_case = None

        # First look for the keyword
        if not self.current_tok.matches(TT_KEYWORD, case_keyword):
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end, f"Expected '{case_keyword}'"
            ))

         # if there is the keyword , look for the expression follow by it
        res.register_advancement()
        self.advance()

        condition = res.register(self.expression())
        if res.error: return res  # Check error

        # Look for the word 'THEN'
        if not self.current_tok.matches(TT_KEYWORD, 'THEN'):
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end, "Expected THEN"
            ))

        res.register_advancement()
        self.advance()

        if self.current_tok.type != TT_NEWLINE:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end, "Expected NEWLINE after THEN or invalid statement"
            ))

        res.register_advancement()
        self.advance()

        statements = res.register(self.statements(("ELSE", "ELSE IF",  "ENDIF")))
        if res.error: return res

        cases.append((condition , statements))

        if self.previous_tok.type  == TT_NEWLINE:
            if self.current_tok.matches(TT_KEYWORD, 'ENDIF'):
                res.register_advancement()
                self.advance()
            else:
                all_cases = res.register(self.elif_expr_or_else())
                if res.error: return res
                new_cases, else_case = all_cases
                cases.extend(new_cases)
            return res.success((cases, else_case))
        else:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end, "Expected NEWLINE before ENDIF or invalid statement"
            ))

    def elif_expr_or_else(self):
        res = ParseResult()
        cases, else_case = [], None

        if self.current_tok.matches(TT_KEYWORD, 'ELSE IF'):
            all_cases = res.register(self.elif_expr())
            if res.error : return res
            cases , else_case = all_cases
        elif self.current_tok.matches(TT_KEYWORD, 'ELSE'):
            else_case = res.register(self.else_expr())
            if res.error : return res
        else:
            return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    "Expected ELSE ,ELSE IF or ENDIF"
                ))
        return res.success((cases, else_case))

    def elif_expr(self):
        return self.if_expr_cases('ELSE IF')

    def else_expr(self):
        res = ParseResult()
        else_case = None
        if self.current_tok.matches(TT_KEYWORD, 'ELSE') == False:
            return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    "Expected 'ELSE'"
                ))
        res.register_advancement()
        self.advance()

        if self.current_tok.type != TT_NEWLINE:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected NEWLINE after else or invalid statement"
            ))

        res.register_advancement()
        self.advance()

        statements = res.register(self.statements("ENDIF"))
        if res.error: return res
        else_case = statements

        if self.previous_tok.type != TT_NEWLINE:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected NEWLINE after else or invalid statement"
            ))

        if self.current_tok.matches(TT_KEYWORD, 'ENDIF'):
            res.register_advancement()
            self.advance()
        else:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected 'ENDIF'"
            ))

        return res.success(else_case)

    def if_expr(self):
        res = ParseResult()
        all_cases = res.register(self.if_expr_cases('IF'))
        if res.error: return res
        if self.previous_tok.matches (TT_KEYWORD , 'ENDIF') != True:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected ENDIF"
            ))
        cases, else_case = all_cases
        return res.success(IfNode(cases, else_case))


    def comp_expr(self): # comparsion expression
        res = ParseResult()
        if self.current_tok.matches(TT_KEYWORD , 'NOT'):
            op_tok = self.current_tok
            res.register_advancement()
            self.advance()

            node = res.register(self.comp_expr())
            if res.error :return res
            return res.success(UnaryOpNode(op_tok, node))
        node = res.register(self.bin_op(self.arith_expr, (TT_EE, TT_NE , TT_LTE , TT_GTE , TT_GT, TT_LT)))

        if res.error:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected 'NOT', int, float, identifier, '+', '-' or '('"
            ))

        return res.success(node)

    def statements_func_def(self):
        res = ParseResult()
        functions = []
        pos_start = self.current_tok.pos_start.copy()
        #gET RID OF newline
        while self.current_tok.type == TT_NEWLINE:
            res.register_advancement()
            self.advance()

        # while the token is 'BEGIN',  the loop will continue execute
        while self.current_tok.matches(TT_KEYWORD, 'BEGIN') == True:

                # First get rid of the newline token
                while self.current_tok.type == TT_NEWLINE:
                    res.register_advancement()
                    self.advance()

                # check whether the current token is 'BEGIN' or not , if so , FuncDefNode will be generated
                if self.current_tok.matches(TT_KEYWORD, 'BEGIN') == True :
                    function_ = res.register(self.func_def())
                    if res.error: return res
                    functions.append(function_)

                # Get rid of the newline token again
                while self.current_tok.type == TT_NEWLINE:
                    res.register_advancement()
                    self.advance()
        if self.current_tok.type != TT_EOF:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected 'BEGIN' or EOF"
            ))
        return res.success(ArrayDefNode(None, functions, pos_start, self.current_tok.pos_end.copy()))

    def statements(self , keyword=None):
        res = ParseResult()
        statements = []
        pos_start = self.current_tok.pos_start.copy()

        '''''
        while self.current_tok.type == TT_NEWLINE:
            res.register_advancement()
            self.advance()
        if not self.current_tok.matches(TT_KEYWORD , keyword):
            statement = res.register(self.statement())
            if res.error: return res
            statements.append(statement)
        '''

        if keyword == None: #Can possblely be ignored?
            more_statements = True
            while True :
                newline_count = 0
                while self.current_tok.type == TT_NEWLINE:
                    res.register_advancement()
                    self.advance()
                    newline_count += 1
                if newline_count == 0:
                    more_statements = False
                if more_statements == False or  self.current_tok.type ==  TT_EOF: break
                statement = res.register(self.statement())
                if res.error: return res
                '''
                if not statement:
                    # cause we will advance a few time once we call self.expression in above
                    self.reverse(res.to_reverse_count)
                    more_statements = False
                    continue
                '''
                statements.append(statement)
        else:
            #While current tokens is not the ending keyword
            while self.current_tok.matches(TT_KEYWORD, keyword) == False:

                # First get rid of the newline token
                while self.current_tok.type == TT_NEWLINE:
                    res.register_advancement()
                    self.advance()

                #check whether the current token is keyword or not , if not , node of this statement will be generated
                if self.current_tok.matches(TT_KEYWORD, keyword) == False and self.current_tok.type !=  TT_EOF and self.current_tok.matches(TT_KEYWORD, 'END') == False:
                    statement = res.register(self.statement())
                    if res.error: return res
                    statements.append(statement)

                # Get rid of the newline token again
                while self.current_tok.type == TT_NEWLINE:
                    res.register_advancement()
                    self.advance()

                # Check if it is the end of statements , if so then the keyword is missing and an error will generated
                if self.current_tok.type == TT_EOF or (self.current_tok.matches(TT_KEYWORD, 'END') and keyword != 'END') :
                    return res.failure(InvalidSyntaxError(
                        self.current_tok.pos_start, self.current_tok.pos_end,
                        f"Unable to find the keyword '{keyword}'"
                    ))
        return res.success(ArrayDefNode(None,  statements , pos_start, self.current_tok.pos_end.copy() ))

    def statement(self):
        res = ParseResult()
        pos_start = self.current_tok.pos_start.copy

        if self.current_tok.matches(TT_KEYWORD, 'RETURN'):
            res.register_advancement()
            self.advance()
            ## Check if we return anything
            if self.current_tok.type == TT_NEWLINE or self.current_tok.type == TT_EOF:
                return res.success(ReturnNode(None, pos_start, self.current_tok.pos_start.copy()))

            expr = res.register(self.expression())
            if res.error: return res

            return res.success(ReturnNode(expr, pos_start , self.current_tok.pos_start.copy()))

        expr = res.register(self.expression())
        if res.error:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected 'RETURN' , 'NOT', int,  float, 'IF', 'FOR', 'WHILE', identifier , 'END______' ,  '+', '-' . '[' or '(' "
            ))
        return res.success(expr)

    def expression(self):
        res = ParseResult()

        if len(self.tokens) >= self.tok_idx:
            #if it is defining a variable
            if self.current_tok.type == TT_IDENTIFIER and self.tokens[self.tok_idx+1].type == TT_EQ:
                #Or self.current_tok.type == TT_IDENTIFIER and self.tokens[self.tok_idx+1].type == TT_DOT and self.tokens[self.tok_idx+3].type == TT_EQ  # check if there is a variable name
                var_name = self.current_tok
                res.register_advancement()
                self.advance()

                if self.current_tok.type != TT_EQ:  # check if there is equal sign
                    return res.failure(InvalidSyntaxError(
                        self.current_tok.pos_start, self.current_tok.pos_end, "Expected '='"
                    ))

                res.register_advancement()
                self.advance()

                # variabe_Name = expression
                expr = res.register(self.expression())
                if res.error: return res
                return res.success(VarAssignNode(var_name, expr))

        node = res.register(self.bin_op(self.comp_expr, ((TT_KEYWORD, 'AND'), (TT_KEYWORD, 'OR'))))

        if res.error:
            return res.failure(InvalidSyntaxError(
                        self.current_tok.pos_start, self.current_tok.pos_end,
                        "Expected 'NOT', int,  float, 'IF', 'FOR', 'WHILE', identifier , '+', '-' or '(' "
                    ))
        return res.success(node)

    def arith_expr(self):
        return self.bin_op(self.Term , (TT_PLUS , TT_MINUS))

    def bin_op(self, func_a, ops ):
        res = ParseResult()
        left = res.register(func_a())
        if res.error: return res

        while self.current_tok.type in ops or (self.current_tok.type , self.current_tok.value) in ops :
            op_tok = self.current_tok
            res.register_advancement()
            self.advance()
            right = res.register(func_a())
            if res.error: return res
            left = BinOpNode(left, op_tok, right)
        return res.success(left)

    def func_def(self):
        res = ParseResult()

        #Double check for the keyword 'BEGIN'
        if not self.current_tok.matches(TT_KEYWORD, 'BEGIN'):
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start , self.current_tok.pos_end,
                "Expected 'BEGIN'"
            ))

        res.register_advancement()
        self.advance()

        # Check for the identifier
        if self.current_tok.type != TT_IDENTIFIER:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected identifer"
            ))

        #Save down the identifier token
        var_name_tok = self.current_tok
        res.register_advancement()
        self.advance()

        arg_name_tok = []

        # When the function's identifeier is not Mainprogram (Normal subrountine)
        if var_name_tok.value != 'Mainprogram':

            #Check for '('
            if self.current_tok.type != TT_LPAREN:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    "Expected '('"
                ))

            res.register_advancement()
            self.advance()

            if self.current_tok.type == TT_IDENTIFIER:
                arg_name_tok.append(self.current_tok)
                res.register_advancement()
                self.advance()

                while self.current_tok.type == TT_COMMA:

                    res.register_advancement()
                    self.advance()

                    if self.current_tok.type != TT_IDENTIFIER:
                        return res.failure(InvalidSyntaxError(
                            self.current_tok.pos_start, self.current_tok.pos_end,
                            "Expected identifier"
                        ))

                    arg_name_tok.append(self.current_tok)
                    res.register_advancement()
                    self.advance()
            # Check for ')'
            if self.current_tok.type != TT_RPAREN:
                    return res.failure(InvalidSyntaxError(
                        self.current_tok.pos_start, self.current_tok.pos_end,
                        "Expected ')' , ',' or identifier "
                    ))

            res.register_advancement()
            self.advance()

        if self.current_tok.type != TT_NEWLINE:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected NEWLINE after declaring method or invalid statement"
            ))

        res.register_advancement()
        self.advance()

        body = res.register(self.statements("END"))
        if res.error: return res

        #Check for the newline before the end of fucntion
        if self.previous_tok.type != TT_NEWLINE:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected NEWLINE before the end of function or invalid statement"
            ))

        #Double check for the keyword END
        if not self.current_tok.matches(TT_KEYWORD, 'END'):
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected 'END'"
            ))

        res.register_advancement()
        self.advance()

        #Check for the identifier after END
        if self.current_tok.value != var_name_tok.value:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                f"Expected END {var_name_tok.value} "
            ))
        res.register_advancement()
        self.advance()

        return res.success(FuncDefNode(
            var_name_tok, arg_name_tok, body
        ))



##################################
# VALUE
##################################
class Value:
    def __init__(self):
        self.set_pos()
        self.set_context()

    def set_context(self, context=None):
        self.context = context
        return self

    def set_pos(self, pos_start=None, pos_end=None):
        self.pos_start = pos_start
        self.pos_end = pos_end
        return self

    def added_to(self, other):
        return None , self.illegal_operation(other)

    def subbed_by(self, other):
        return None , self.illegal_operation(other)
    def multed_by(self, other):
        return None , self.illegal_operation(other)
    def dived_by(self, other):
        return None , self.illegal_operation(other)

    def get_comparison_eq(self, other):
        return Boolean(bool(self.value == other.value)).set_context(self.context), None
    def get_comparison_ne(self, other):
        return Boolean(bool(self.value != other.value)).set_context(self.context), None
    def get_comparison_lt(self, other):
        return None , self.illegal_operation(other)
    def get_comparison_gt(self, other):
          return None, self.illegal_operation(other)
    def get_comparison_lte(self, other):
            return None , self.illegal_operation(other)
    def get_comparison_gte(self, other):
            return None , self.illegal_operation(other)
    def anded_by(self, other):
        return None , self.illegal_operation(other)
    def ored_by(self, other):
        return None , self.illegal_operation(other)
    def notted(self):
        return None, self.illegal_operation()
    def copy(self):
        return Exception('No copy method defined')
    def illegal_operation(self, other = None):
        if not other : other = self
        return RTError(
            self.pos_start ,other.pos_end,
            f'Illegal operation between {self.value} and {other.value}' ,
            self.context
        )

class Array(Value):
    def __init__(self, node_list, array_type ):
        super().__init__()
        self.value = node_list
        self.array_type = array_type
        self.depth = 1

    def copy(self):
        copy = Array(self.value, self.array_type)
        copy.set_pos(self.pos_start , self.pos_end)
        copy.set_context(self.context)
        return copy

    def get_comparison_eq(self, other):
        if isinstance(other, Array):
            for i in range(0 , len(self.value)):
                if self.value[i].value != other.value[i].value:
                    return Boolean(False).set_context(self.context), None
            return Boolean(True).set_context(self.context), None
        else:
            return Boolean(False).set_context(self.context), None

    def get_comparison_ne(self, other):
        if isinstance(other, type(self)):
            for i in range(0, len(self.value)):
                if self.value[i].value != other.value[i].value:
                    return Boolean(True).set_context(self.context), None
            return Boolean(False).set_context(self.context), None
        else:
            return Boolean(True).set_context(self.context), None

    def __repr__(self):
        value_list = []
        for x in self.value:
            value_list.append(x.value)
        return f"{value_list}"
    #need pos_start / pos_end ?

class Boolean(Value):
    def __init__(self, value):
        self.value = value
        super().__init__()

    def get_comparison_eq(self, other):
        if isinstance(other, Boolean):
            return Boolean(bool(self.value == other.value)).set_context(self.context), None
        else:
            return Boolean(False).set_context(self.context), None
    def get_comparison_ne(self, other):
        if isinstance(other, Boolean):
            return Boolean(bool(self.value != other.value)).set_context(self.context), None
        else:
            return Boolean(True).set_context(self.context), None

    def anded_by(self, other):
        if bool(self.value) == False :
            return Boolean(False).set_context(self.context) , None
        return Boolean(bool(self.value and other.value)).set_context(self.context), None

    def ored_by(self, other):
            return Boolean(bool(self.value or other.value)).set_context(self.context), None
    def notted(self):
        return Boolean(True if self.value == False else False).set_context(self.context), None
    def copy(self):
        copy = Boolean(self.value)
        copy.set_pos(self.pos_start , self.pos_end)
        copy.set_context(self.context)
        return copy
    def __repr__(self):
        return f"{self.value}"

class Number(Value):
    def __init__(self, value):
        self.value = value
        super().__init__()

    def added_to(self, other):
        if isinstance(other , Number) or isinstance(other , Float):
            value_ = self.value + other.value
            if isinstance(value_ , int):
                return Number(value_).set_context(self.context), None
            else:
                return Float(value_).set_context(self.context), None
        else:
            return None , Value.illegal_operation(self, other)

    def subbed_by(self, other):
        if isinstance(other , Number) or isinstance(other , Float):
            value_ = self.value - other.value
            if isinstance(value_, int):
                return Number(value_).set_context(self.context), None
            else:
                return Float(value_).set_context(self.context), None
        else:
            return None , Value.illegal_operation(self, other)
    def multed_by(self, other):
        if isinstance(other , Number) or isinstance(other , Float):
            value_ = self.value * other.value
            if isinstance(value_, int):
                return Number(value_).set_context(self.context), None
            else:
                return Float(value_).set_context(self.context), None
        else:
            return None , Value.illegal_operation(self, other)
    def dived_by(self, other):
        if isinstance(other , Number) or isinstance(other , Float):
            value_ = self.value / other.value
            if isinstance(value_, int):
                return Number(value_).set_context(self.context), None
            else:
                return Float(value_).set_context(self.context), None
        else:
            return None , Value.illegal_operation(self, other)

    def get_comparison_lt(self, other):
        if isinstance(other, Number) or isinstance(other, Float):
            return Boolean(self.value < other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)
    def get_comparison_gt(self, other):
        if isinstance(other, Number) or isinstance(other, Float):
            return Boolean(self.value > other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)
    def get_comparison_lte(self, other):
        if isinstance(other, Number) or isinstance(other, Float):
            return Boolean(self.value <= other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)
    def get_comparison_gte(self, other):
        if isinstance(other, Number) or isinstance(other, Float):
            return Boolean(self.value >= other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)
    def anded_by(self, other):
        return None , self.illegal_operation(other)
    def ored_by(self, other):
        return None , self.illegal_operation(other)
    def notted(self):
        return None , self.illegal_operation()
    def copy(self):
        copy = Number(self.value)
        copy.set_pos(self.pos_start , self.pos_end)
        copy.set_context(self.context)
        return copy

    def __repr__(self):
        return str(self.value)

Number.null = Number(None)

class Float(Number):
    def __init__(self, value):
        super().__init__(value)

    def copy(self):
        copy = Float(self.value)
        copy.set_pos(self.pos_start , self.pos_end)
        copy.set_context(self.context)
        return copy

class Character(Value):
    def __init__(self, value):
        super().__init__()
        self.value = value

    def __repr__(self):
        return f"{self.value}"

    def copy(self):
        copy = Character(self.value)
        copy.set_pos(self.pos_start, self.pos_end)
        copy.set_context(self.context)
        return copy

    def added_to(self, other):
        if isinstance(other, String):
            list_  = [self] + other.value
            return String(list_).set_context(self.context), None
        elif isinstance(other, Character):
            list_ = [self, other]
            return String(list_).set_context(self.context), None
        else:
            return None , Value.illegal_operation(self, other)

class String(Array):
    def __init__(self, value):
        if isinstance(value , str):
            result = []
            for x in value:
                result.append(Character(x))
            super().__init__(result, type(Character(0)))
        else:
            super().__init__(value, type(Character(0)))
    def is_two_String_same(self,string2):
        if len(self.value) != len(string2.value):
            return False
        for i in range(0,len(self.value)):
            if self.value[i].value != string2.value[i].value:
                return False
        return True
    def added_to(self, other):
        if isinstance(other, String):
            self.value.extend(other.value)
            return String(self.value).set_context(self.context), None
        elif isinstance(other, Character):
            final_string = self.__repr__()+ str(other.value)
            return String(final_string).set_context(self.context), None
        else:
            return None , Value.illegal_operation(self, other)

    def multed_by(self, other):
        return None , Value.illegal_operation(self, other)

    def dived_by(self, other):
        return None , self.illegal_operation(other)


    def copy(self):
        copy = String(self.value)
        copy.set_pos(self.pos_start, self.pos_end)
        copy.set_context(self.context)
        return copy

    def __repr__(self):
        string_ = ""
        for x in self.value:
            string_ = string_ + x.value
        return f"{string_}"

class BaseFunction(Value):
    def __init__(self, name):
        super().__init__()
        self.name = name or '<anonymous>'

    def generate_new_context(self):
        new_context = Context(self.name, self.context, self.pos_start)
        new_context.symbol_table = SymbolTable(new_context.parent.symbol_table, self.name)
        return new_context

    def check_args(self, arg_names, args):
        res = RTResult()
        if len(args) > len(arg_names):
            return res.failure(
                RTError(
                    self.pos_start, self.pos_end,
                    f"{len(args) - len(arg_names)} too many args passed into {self.name}",
                    self.context
                )
            )

        if len(args) < len(arg_names):
            return res.failure(
                RTError(
                    self.pos_start, self.pos_end,
                    f"{len(arg_names) - len(args)} too few args passed into {self.name}",
                    self.context
                )
            )
        return res.success(None)

    def populate_args(self, arg_names , args, exec_ctx):
        for i in range(len(args)):
            arg_name = arg_names[i]
            arg_value = copy.deepcopy(args[i]) # without deep copy , python will link the arg_value to the mainprogram_context
            # update the context of each argument
            arg_value.set_context(exec_ctx)
            # create the varaibles, so that the parameter in the function will equal to the entered argument value now
            exec_ctx.symbol_table.set(arg_name, arg_value)

    def check_and_populate_args(self, arg_names , args, exec_ctx):
        res = RTResult()
        res.register(self.check_args(arg_names, args))
        if res.should_return() : return res
        self.populate_args(arg_names , args , exec_ctx)
        return res.success(None)


class Function(BaseFunction):
    def __init__(self, name, body_node , arg_name):
        super().__init__(name)
        self.body_node = body_node
        self.arg_name = arg_name
        self.value = f"<function> {self.name}"


    def execute(self, args):
        res = RTResult()
        interpreter = Interpreter()
        exec_ctx = self.generate_new_context()

        if self.name == 'Mainprogram':
            self.context.display_name = "Mainprogram"
            exec_ctx = self.context

        res.register(self.check_and_populate_args(self.arg_name, args , exec_ctx))
        if res.should_return(): return res

        value = res.register(interpreter.visit(self.body_node, exec_ctx))
        if res.should_return() and res.func_return_value == None : return res

        ret_value = res.func_return_value or Number.null
        return res.success(ret_value)

    def copy(self):
        copy = Function(self.name , self.body_node, self.arg_name)
        copy.set_context(self.context)
        copy.set_pos(self.pos_start , self.pos_end)
        return copy

    def __repr__(self):
        return f"<function> {self.name}"

class BuiltInFunction(BaseFunction):
    def __init__(self,name):
        super().__init__(name)

    def execute(self,args):
        res = RTResult()
        exec_ctx = self.generate_new_context()

        method_name = f"execute_{self.name}"
        method = getattr(self, method_name , self.no_visit_method)

        res.register(self.check_and_populate_args(method.arg_names, args, exec_ctx))
        if res.should_return(): return res

        return_value= res.register(method(exec_ctx))
        if res.should_return() : return res
        return res.success(return_value)

    def no_visit_method(self, node , context):
        raise Exception(f"No execute_{self.name} method defined")

    def copy(self):
        copy = BuiltInFunction(self.name)
        copy.set_context(self.context)
        copy.set_pos(self.pos_start , self.pos_end)
        return copy

    def __repr__(self):
        return f"<builit-in function {self.name}>"
    ###############################################

    def execute_Display(self, exec_ctx):
        #Create the path to output.txt
        cur_path = os.path.dirname(__file__)
        path = Path(cur_path)
        new_path = os.path.join(path.parent.absolute(), '..\\static\\output.txt')

        #write output to output.txt
        with open(new_path, "a") as f:
            f.write(str(exec_ctx.symbol_table.get('value'))+'\n')
        f.close()

        print("Displayed" + str(exec_ctx.symbol_table.get('value'))+'\n')
        return RTResult().success(Number.null)

    execute_Display.arg_names = ["value"]

    def execute_int(self, exec_ctx):
        value_ = exec_ctx.symbol_table.get('value')

        # First check the type of the value_
        if  type(value_) == String or type(value_) == Number or type(value_) == Float:

            # Double check , not all the strings can be turned into int
            try:
                return RTResult().success(
                    Number(int(value_.value))
                )
            except:

                # return error if strings is not number
                return RTResult().failure(
                    RTError(value_.pos_start, value_.pos_end, f"Cant convert '{value_.value}' to int", exec_ctx)
                )

        else:
            return RTResult().failure(
                RTError(value_.pos_start , value_.pos_end , f"Cant convert {type(value_)} to int", exec_ctx)
            )

    execute_int.arg_names = ["value"]
    def execute_str(self, exec_ctx):
        value_ = exec_ctx.symbol_table.get('value')

        # First check the type of the value_
        if  type(value_) == String or type(value_) == Number or type(value_) == Boolean  or type(value_) == Float:
            return RTResult().success(
                    String(str(value_.value))
                )

        else:
            return RTResult().failure(
                RTError(value_.pos_start , value_.pos_end , f"Cant convert {type(value_)} to str", exec_ctx)
            )

    execute_str.arg_names = ["value"]
    def execute_Get(self, exec_ctx):
        #Get the input from input.txt
        cur_path = os.path.dirname(__file__)
        path = Path(cur_path)
        new_path = os.path.join(path.parent.absolute(), '..\\static\\input.txt')

        #Get the input
        with open(new_path, "r") as f:
            input_queue = f.read().splitlines()
            try:
                text=input_queue[0]
            except IndexError:
                exit()
        f.close()

        #Overerite the input.txt in new order
        with open(new_path, 'w') as fout:
            for x in input_queue[1:]:
                fout.write(x + "\n")
        f.close()

        #Save down the input to the variable
        var_name = exec_ctx.symbol_table.get('variable_name').__repr__()
        if '.' in text:
            try:
                text = float(text)
            except:
                self.context.symbol_table.set(var_name, String(text))
                return RTResult().success(Number.null)
        else:
            try:
                text = int(text)
            except:
                self.context.symbol_table.set(var_name,String(text))
                return RTResult().success(Number.null)
        self.context.symbol_table.set(var_name , Number(text))
        return RTResult().success(Number.null)
    execute_Get.arg_names = ["variable_name"]

'''''
BuiltInFunction.Display      = BaseFunction("Display")
BuiltInFunction.Get      = BaseFunction("Get")
'''
##################################
# SYMBOL TABLE
##################################
class SymbolTable:
    def __init__(self, parent=None, name =None):
        self.symbols = {}
        self.parent = parent
        self.name = name
        # symbo table of a function with have a parent table (the global variables )

    #get a varibale from the table
    def get(self, name, context=None):
        #local variable priorty > global
        value = global_symbol_table.symbols.get(name)
        if self.symbols.get(name) != None: # if referecning itself inside function
            value = self.symbols.get(name)
        return value

    def initialize(self):
        self.symbols = {}
        self.set('NULL', Number.null)
        self.set("Display", BuiltInFunction('Display'))
        self.set("Get", BuiltInFunction('Get'))
        self.set("int", BuiltInFunction('int'))
        self.set("str", BuiltInFunction('str'))
    def remove(self, name):
        self.symbols.pop(name)

    def set(self, name, value):
        self.symbols[name] = value
        # set a dictionary of variables

#################################
# INTERPRETER
##################################
class Interpreter:
    def visit(self, node, context):
        method_name = f'visit_{type(node).__name__}'
        #"visit_BinOpNode"
        method = getattr(self ,method_name, self.no_visit_method)
        return method(node,context)

    def no_visit_method(self, node, context):
        raise Exception(f'No visit_{type(node).__name__} method defined ')

    def visit_NumberNode(self, node, context):
        return RTResult().success(
            Number(node.tok.value).set_context(context).set_pos(node.pos_start, node.pos_end)
        )

    def visit_FloatNode(self, node, context):
        return RTResult().success(
            Float(node.tok.value).set_context(context).set_pos(node.pos_start, node.pos_end)
        )
    # Number are always success (you cant get runtime error )
    def visit_VarAccessNode(self, node , context):
        res = RTResult()
        var_name = node.var_name_tok.value
        value = context.symbol_table.get(var_name, context)
        if not value:
            return res.failure(
                RTError(node.pos_start , node.pos_end, f"'{var_name}' is not defined", context)
            )
        value = value.copy().set_pos(node.pos_start , node.pos_end).set_context(context)
        # so that the pos is where we access it instead of where we assign it
        return res.success(value)

    def visit_VarAssignNode(self, node , context):
        res = RTResult()
        var_name = node.var_name_tok.value
        if type(node.value_node) == WhileNode or type(node.value_node) == FuncDefNode or type(node.value_node) == ForNode or type(node.value_node) == IfNode:
            return res.failure(
                RTError(node.pos_start, node.pos_end, f"'{type(node.value_node)}' cant be assigned to a variable", context)
            )
        value = res.register(self.visit(node.value_node, context))
        if res.should_return(): return res

        context.symbol_table.set(var_name , value)
        return res.success(Number.null)

    def visit_StringNode(self, node , context):
        string_ = node.tok.value
        Character_list = []

        for x in string_:
            Character_list.append(Character(x).set_pos(node.tok.pos_start, node.tok.pos_end))
        #A string
        if len(Character_list) > 1:
            return RTResult().success(
            String(Character_list).set_context(context).set_pos(node.pos_start , node.pos_end)
            )
        #A single Chracter
        elif len(Character_list) == 1 :
            return RTResult().success(
                Character_list[0].set_context(context).set_pos(node.pos_start, node.pos_end)
            )

        #Other weird case
        else:
            return RTResult().success(
                String(Character_list).set_context(context).set_pos(node.pos_start, node.pos_end)
            )

    def visit_BinOpNode(self, node, context):
        res = RTResult()
        left = res.register(self.visit(node.left_node, context))
        if res.should_return() : return res
        right = res.register(self.visit(node.right_node, context))
        if node.op_tok.matches(TT_KEYWORD, "AND") == False and node.op_tok.matches(TT_KEYWORD, "OR") == False:
            if res.should_return(): return res

            if left == None or right == None or right.value == None or left.value == None:
                return res.failure(
                    RTError(node.pos_start, node.pos_end, f"None type can't perform binary operation",
                            context)
                )

        '''
        Something need to do :
        pass node.pos_start and node.pos_end into the binop function so that error messaage will point to the node instend of the declaration of variabe
        '''
        if node.op_tok.type == TT_PLUS:
            result ,error = left.added_to(right)
        elif node.op_tok.type == TT_MINUS:
            result,error = left.subbed_by(right)
        elif node.op_tok.type == TT_MUL:
            result,error = left.multed_by(right)
        elif node.op_tok.type == TT_DIV:
            result,error = left.dived_by(right)
        elif node.op_tok.type == TT_EE:
            result, error = left.get_comparison_eq(right)
        elif node.op_tok.type == TT_NE:
            result, error = left.get_comparison_ne(right)
        elif node.op_tok.type == TT_LT:
            result, error = left.get_comparison_lt(right)
        elif node.op_tok.type == TT_GT:
            result, error = left.get_comparison_gt(right)
        elif node.op_tok.type == TT_LTE:
            result, error = left.get_comparison_lte(right)
        elif node.op_tok.type == TT_GTE:
            result, error = left.get_comparison_gte(right)
        elif node.op_tok.matches(TT_KEYWORD, 'AND'):
            if left.value == True:
                if res.should_return(): return res
            result, error = left.anded_by(right)
        elif node.op_tok.matches(TT_KEYWORD, 'OR'):
            if left.value == False:
                if res.should_return(): return res
            result, error = left.ored_by(right)
        if error:
            return res.failure(error)
        else:
            return res.success(result.set_pos(node.pos_start , node.pos_end))

    def visit_RepeatNode(self, node , context):
        res = RTResult()

        while True:
            # visit the body_node first
            res.register(self.visit(node.body_node, context))
            if res.should_return(): return res

            # visit the condition node
            condition = res.register(self.visit(node.condition_node, context))
            if res.should_return(): return res

            # Ensure the type of node is correct
            if type(condition) != Boolean:
                return res.failure(RTError(
                    node.condition_node.pos_start, node.condition_node.pos_end,
                    f"Expected a Boolean expression for condition", context))

            # if condition fail , break the loop
            if condition.value == True: break

        return res.success(Number.null)

    def visit_WhileNode(self, node, context):
        res = RTResult()

        while True:
            # visit the condition node
            condition = res.register(self.visit(node.condition_node , context))
            if res.should_return(): return res

            # Ensure the type of node is correct
            if type(condition) != Boolean:
                return res.failure(RTError(
                    node.condition_node.pos_start, node.condition_node.pos_end,
                    f"Expected a Boolean expression for condition", context))

            # if condition fail , break the loop
            if  condition.value == False : break

            # visit the body_node first
            res.register(self.visit(node.body_node, context))
            if res.should_return() : return res

        return res.success(Number.null)


    def visit_ForNode(self, node, context):
        res = RTResult()

        # Generate the nodes for start , end and step value
        start_value = res.register(self.visit(node.start_value_node , context))
        if res.should_return() : return res

        if isinstance(start_value, Number) == False or isinstance(start_value, Float):
            return res.failure(RTError(
                start_value.pos_start, start_value.pos_end,
                f"Expected an integer for the starting value of for loop", context))

        end_value = res.register(self.visit(node.end_value_node, context))
        if res.should_return(): return res

        if isinstance(end_value, Number) == False or isinstance(end_value, Float):
            return res.failure(RTError(
                end_value.pos_start, end_value.pos_end,
                f"Expected an integer for the ending value of for loop", context))


        step_value = res.register(self.visit(node.step_value_node, context))
        if res.should_return(): return res

        if isinstance(step_value, Number) == False or isinstance(step_value, Float):
            return res.failure(RTError(
                step_value.pos_start, step_value.pos_end,
                f"Expected an integer for the stepping value of for loop", context))

        i = start_value.value

        # Base on the sign of step value , generate the corresponding condition()
        if step_value.value >= 0:
            condition = lambda: i <= end_value.value
        else:
            condition = lambda: i >= end_value.value
        context.symbol_table.set(node.var_name_tok.value, Number(i))
        #Excute the for loop while condition is true
        while condition():

            # Update the local variable of for loop
            context.symbol_table.set(node.var_name_tok.value , Number(i))
            i += step_value.value

            # Excute the body nodes of the loop
            res.register(self.visit(node.body_node, context))
            if res.should_return() : return res

        context.symbol_table.remove(node.var_name_tok.value)
        return res.success(Number.null)

    def visit_BooleanNode(self, node , context):
        return RTResult().success(
            Boolean(node.tok.value).set_context(context).set_pos(node.pos_start, node.pos_end)
        )
    def visit_CasewhereNode(self, node, context):
        res = RTResult()
        var_value = res.register(self.visit(node.var_node, context))
        if res.should_return(): return res

        for condition_node , action_node in node.cases:
            # Generate the condition value
            if condition_node != None:
                condition_value = res.register(self.visit(condition_node, context))
            else:
                otherwise_action_node  = action_node
            if res.should_return(): return res

            # Check if the condition value match the varaible value
            if type(condition_value) == type(var_value):
                # cause string consist an array of character, need to check it with a method
                if (type(condition_value) == String and condition_value.is_two_String_same(
                        var_value) == True) or condition_value.value == var_value.value:
                    action = res.register(self.visit(action_node, context))
                    if res.should_return(): return res
                    return res.success(Number.null)

        otherwise_action = res.register(self.visit(otherwise_action_node , context))
        if res.should_return(): return res

        return res.success(Number.null)

    def visit_IfNode(self,node , context):
        res = RTResult()

        # Check all the condition in IF case or ELIF case
        for condition , expr in node.cases:
            condition_value  = res.register(self.visit(condition , context))
            if res.should_return(): return res

            # Check whether the condition is valid
            if type(condition_value) != Boolean:
                return res.failure(RTError(
                    condition.pos_start, condition.pos_end,
                    f"Expected a Boolean expression for condition", context))


            # if condition is true
            if condition_value.value == True:
                expr_value = res.register(self.visit(expr, context))
                if res.should_return(): return res
                return res.success(Number.null)

        #Check ELSE case if none of IF and ElEF cases are true
        if node.else_case:
            expr  = node.else_case
            else_value = res.register(self.visit(expr , context))
            if res.should_return(): return res
            return res.success(Number.null)

        #No Else case
        return res.success(Number.null)

    def visit_UnaryOpNode(self, node, context):
        res = RTResult()
        value = res.register(self.visit(node.node, context))
        error = None
        if res.should_return(): return res
        # Check weather it is a boolean value or not
        if isinstance(value, Boolean):
            if node.op_tok.matches(TT_KEYWORD, 'NOT'):
                value, error = value.notted()
                if error:return res.failure(error)
                return res.success(value.set_pos(node.pos_start, node.pos_end))
            else:
                return res.failure(RTError(node.pos_start, node.pos_end, f"unexpected operation token before a boolean value", context))
        # Check if it is a int or float
        if  isinstance(value.value , int) == False and  isinstance(value.value , float) == False:
            return res.failure(RTError(node.pos_start , node.pos_end, f"Not expected unary operation before a variable that is not int or float", context))
        if node.op_tok.type == TT_MINUS:
            value,error  = value.multed_by(Number(-1))
        elif node.op_tok.matches(TT_KEYWORD , 'NOT'):
            value , error = value.notted()
        if error:
            return res.failure(error)
        else:
            return res.success(value.set_pos(node.pos_start, node.pos_end))

    def visit_FuncDefNode(self, node , context):
        res = RTResult()

        func_name = node.var_name_toks.value if node.var_name_toks else None
        body_node = node.body_node
        arg_names = [arg_name.value for arg_name in node.args_name_tok]
        func_value = Function(func_name, body_node , arg_names).set_context(copy.deepcopy(context)).set_pos(node.pos_start , node.pos_end)

        if node.var_name_toks:
            if global_symbol_table.symbols.get(func_name) != None:
                return res.failure(
                    RTError(node.pos_start, node.var_name_toks.pos_end, f"Identifier {func_name} has already been defined ", context))
            context.symbol_table.set(func_name, func_value)

        return res.success(Number.null)

    def visit_CallNode(self, node ,context):
        res = RTResult()
        args = []
        # get the node that we are calling
        value_to_call = res.register(self.visit(node.node_to_call, context))

        if context.display_name == node.node_to_call.var_name_tok.value:
            return res.failure(RTError(
                node.pos_start, node.pos_end, f"Invalid call of function", context
            ))

        if res.should_return(): return res
        # so that it can show where errors came from , rather than where it define
        value_to_call = value_to_call.copy().set_pos(node.pos_start , node.pos_end)
        if type(value_to_call) != Function and type(value_to_call) != BaseFunction and type(value_to_call) != BuiltInFunction:
            return res.failure(RTError(
                node.pos_start, node.pos_end, f"Invalid call of function" , context
            ))
        for arg_node in node.arg_nodes:
            args.append(res.register(self.visit(arg_node , context)))
            if res.should_return(): return res

        return_value = res.register(value_to_call.execute(args))
        if res.should_return() : return res
        return_value = return_value.copy().set_pos(node.pos_start, node.pos_end).set_context(context)
        return res.success(return_value)

    def visit_ArrayDefNode(self, node , context):
        res = RTResult()
        value_list = []
        array_name = None
        array_type = None
        if node.var_name_toks == True:
            array_name = True
        elif node.var_name_toks:
            array_name = node.var_name_toks.value
        element_node_list = node.elements_node_list

        #Array inside an array

        if array_name == True:
            # Define the type of array using the first element of array
            if element_node_list:
                value = res.register(self.visit(element_node_list[0], context))
                if res.should_return(): return res
                value_list.append(value)
                array_type = type(value)

            # visit each node in the list
            for x in element_node_list[1:]:
                value = res.register(self.visit(x, context))
                if type(value) != array_type:
                    return res.failure(
                        RTError(node.pos_start, node.pos_end, f"Array can only consist one type of varaible", context))
                if res.should_return(): return res
                value_list.append(value)

            array = Array(value_list, array_type).set_context(context).set_pos(node.pos_start, node.pos_end)
            array.depth = node.array_depth
            if array_type == String:
                array.depth += 1

            return res.success(array)
        # Normal array
        elif array_name != None:
            _2d_array = False

            # Define the type of array using the first element of array
            if element_node_list:
                value = res.register(self.visit(element_node_list[0], context))
                if res.should_return(): return res
                value_list.append(value)
                array_type = type(value)

                #2d array type
                if array_type == type(Array(1,1)):
                    _2d_array = True
                    _2d_array_type = value.array_type

            # visit each node in the list
            for x in element_node_list[1:]:
                value = res.register(self.visit(x,context))
                #Check if same type
                if type(value) != array_type:
                    return res.failure(RTError(node.pos_start, node.pos_end, f"Array can only consist one type of varaible", context))
                #If is 2d array than check if array is same type
                if _2d_array == True:
                    if value.array_type != _2d_array_type:
                        return res.failure(
                            RTError(node.pos_start, node.pos_end, f"2D Array can only consist one type of array",
                                    context))

                if res.should_return() : return res
                value_list.append(value)

        # when there is no array_name , we assume ArrayDefNode is use for muti-line statement(Mainprogram)/2D array2d
        else:
            for x in element_node_list:
                value = res.register(self.visit(x,context))
                if res.should_return() : return res
                value_list.append(value)

        array = Array(value_list, array_type).set_context(context).set_pos(node.pos_start, node.pos_end)
        array.depth = node.array_depth
        if array_type == String:
            array.depth += 1

        if array_name : context.symbol_table.set(array_name, array)
        return res.success(array)

    def visit_AttributeAccessNode(self, node , context):
        res = RTResult()
        is_identifer = False

        if type(node.main_tok) == Token:
            var_name = node.main_tok.value
            is_identifer = True
        else:
            var_name = res.register(self.visit(node.main_tok, context))
        attribute_name = node.attribute_tok.value

        # check if it is a method or attribute
        if node.function_or_not == False:

            if attribute_name == 'length':
                if is_identifer == True:
                    var_ = context.symbol_table.get(var_name)
                    if var_ == None:
                        return res.failure(RTError(
                            node.pos_start, node.pos_end,
                            f"{var_name} is not defined",
                            context
                        ))

                else:
                    var_ = var_name

                if type(var_) == Array or type(var_) == String or type(var_) == Character :
                    return res.success(Number(len(var_.value)).set_context(context).set_pos(node.pos_start, node.pos_end))
                else:
                    return res.failure(RTError(
                        node.pos_start, node.pos_end,
                        f"'{type(var_)}' has no .length() method",
                        context
                    ))
            else:
                    return res.failure(RTError(
                        node.pos_start, node.pos_end,
                        f"{attribute_name} not defined yet",
                        context
                    ))
        else:
            # get attribute of a class
            return res.failure(RTError(
                node.pos_start, node.pos_end,
                f"havn't construct the method for it yet",
                context
            ))

    def visit_ReturnNode(self, node , context):
        res= RTResult()
        if type(node.node_to_return) == WhileNode or type(node.node_to_return) == FuncDefNode or type(node.node_to_return) == ForNode or type(node.node_to_return) == IfNode:
            return res.failure(
                RTError(node.pos_start, node.pos_end, f"'{type(node.value_node)}' cant be return", context)
            )
        if node.node_to_return:
            value = res.register(self.visit(node.node_to_return , context))
            if res.should_return(): return res
        else:
            value = Number.null

        return res.success_return(value)



    def visit_ArrayAccessNode(self, node , context):
        res = RTResult()
        var_name = node.var_name_tok.value
        index_list = []

        for index_node in node.index_node_list:
            index_ = res.register(self.visit(index_node, context))
            if res.should_return(): return res
            index_list.append(index_)

        #prevent unexpected error
        Array_ = context.symbol_table.get(var_name)

        #Check whether the variable exist
        if not Array_:
            return res.failure(RTError(
                node.pos_start, node.pos_end,
                f"'{var_name}' is not defined",
                context
            ))
        #Check whether the variable is array or string
        if type(Array_) != Array and type(Array_) != String:
            return res.failure(RTError(
                node.pos_start, node.pos_end,
                f"'{var_name}' is not an array",
                context
            ))
        #Check the depth of array
        if Array_.depth < len(index_list):
            return res.failure(RTError(
                node.pos_start, node.pos_end,
                f"'{var_name}' has array depth less than {len(index_list)}",
                context
            ))


        The_Array_ = "Array_.value"
        #Get the array user is referncing first

        rep_time = len(index_list)
        for index_node in index_list:
            rep_time= rep_time -1
            index = index_node.value
            if type(index_node) != Number:
                return res.failure(RTError(
                    index_node.pos_start, index_node.pos_end,
                    f"Array index must be a number ",
                    context
                ))

            if index >= 0 and eval(f"len({The_Array_}) <= abs(index)"):
                return res.failure(RTError(
                    node.pos_start, node.pos_end,
                    f"index exceed the length of '{var_name}', expected the magnitude of index less than {eval(f'len({The_Array_})')}",
                    context
                ))

            elif index < 0 and eval(f"len({The_Array_}) < abs(index)"):
                return res.failure(RTError(
                    node.pos_start, node.pos_end,
                    f"index exceed the length of '{var_name}', expected the magnitude of index greater than or equal {eval(f'len({The_Array_})')}",
                    context
                ))
            The_Array_ = The_Array_ + "[" + str(index) + "]"

            #when an array is inside the array
            if rep_time > 0:
                The_Array_ = The_Array_+ ".value"


        # Check whether user is changing the array or just calling the a variable in array
        if node.Check_value == True: #If changing array
            visited_node = res.register(self.visit(node.new_element_node, context)) if node.new_element_node != None else None

            # Check the array type
            if Array_.array_type != None and type(visited_node) != Array_.array_type:
                return res.failure(RTError(
                    node.pos_start, node.pos_end,
                    f"Array can only consist a single type of elements",
                    context
                ))
            # if no array type then change the array type
            elif Array_.array_type == None:
                Array_.array_type = type(visited_node)

            exec(f"{The_Array_} = visited_node")
            context.symbol_table.set(var_name, Array_)
            return res.success(Array_.value[index].set_pos(node.pos_start , node.pos_end))

        elif node.Check_value == False: #If referencing array
            return res.success(eval(f"{The_Array_}.set_pos(node.pos_start , node.pos_end)"))







##################################
# RUN
##################################
global_symbol_table = SymbolTable()
global_symbol_table.set('NULL' , Number.null)
global_symbol_table.set("Display", BuiltInFunction('Display'))
global_symbol_table.set("Get", BuiltInFunction('Get'))
global_symbol_table.set("int", BuiltInFunction('int'))
global_symbol_table.set("str", BuiltInFunction('str'))

def Execute_Mainprogram():
    res = RTResult()
    value = global_symbol_table.get('Mainprogram')
    if not value:
        return res.failure(RTError(
                None, None,f"Unable to find the Mainprogram",None
                ))
    global_symbol_table.symbols.pop('Mainprogram')
    return_value = res.register(value.execute([]))
    if res.should_return(): return res
    return res.success(return_value)


def run(text):

    #Initallize and get the absolute path of output file
    global_symbol_table.initialize()
    cur_path = os.path.dirname(__file__)
    path = Path(cur_path)
    new_path = os.path.join(path.parent.absolute(), '..\\static\\output.txt')
    open(new_path, 'w').close()

    # Generate tokens
    lexer = Lexer(text)
    tokens, error = lexer.make_tokens()
    if error: return None, error
    # Gnereate AST
    parser = Parser(tokens)
    ast = parser.statements_func_def()
    if ast.error : return None ,ast.error

    #Run program
    interpreter = Interpreter()
    context = Context('<Mainprogram>')
    context.symbol_table = global_symbol_table
    interpreter_result = interpreter.visit(ast.node, context)
    if interpreter_result.error: return interpreter_result.value , interpreter_result.error
    final_result =  Execute_Mainprogram()
    if final_result:
        return final_result.value, final_result.error
    else :
        return None , None

