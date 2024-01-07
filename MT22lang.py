from sly import Lexer, Parser

from AST import *

class MT22Lexer(Lexer):
    # tokens set
    tokens = {
        LPAREN, RPAREN, LBRACKET, RBRACKET, LBRACE, RBRACE, COMMA, SEMICOLON, COLON, ASSIGNMENT,
        PLUS, MINUS, MUL, DIV, MOD, NOT, AND, OR, EQUAL, NOTEQUAL, LESSTHAN, LEQ, GREATERTHAN, GEQ, DOUBLECOLON,
        AUTO, BREAK, BOOLEAN, DO, ELSE, FLOAT, FOR, FUNCTION, IF, INTEGER, RETURN, STRING, VOID, WHILE, OUT, CONTINUE, OF, INHERIT, ARRAY,
        INTLIT, FLOATLIT, BOOLEANLIT, STRINGLIT,ID
    }
    
    literals = {'.'}
    # ignore char
    ignore = ' \t\b\f\r\n'
    # ignored_seq = r'[ \t\b\f\r\n]+'
    
    # Regular expression rules for tokens
    BOOLEANLIT = r'true | false'
    
    @_(r'\"(.+?)\"')
    def STRINGLIT(self, t):
        t.value = t.value[1:-1]
        return t    
    
    PLUS = r'\+'
    MINUS = r'-'
    MUL = r'\*'
    DIV = r'/'
    MOD = r'%'
    NOT = r'!'
    AND = r'&&'
    OR = r'\|\|'
    EQUAL = r'=='
    NOTEQUAL = r'!='
    LESSTHAN = r'<'
    LEQ = r'<='
    GREATERTHAN = r'>'
    GEQ = r'>='
    DOUBLECOLON = r'::'
    
    LPAREN = r'\('
    RPAREN = r'\)'
    LBRACKET = r'\['
    RBRACKET = r'\]'
    LBRACE = r'\{'
    RBRACE = r'\}'
    COMMA = r','
    SEMICOLON = r';'
    COLON = r':'
    ASSIGNMENT = r'='
    
    @_(r'(([1-9][0-9]*(_[0-9]+)*)|[0])\.[0-9]*([Ee][+-]?[1-9][0-9]*)?')
    def FLOATLIT(self, t):
        t.value = t.value.replace('_','')
        return t
    
    @_(r'([1-9][0-9]*(_[0-9]+)*)|[0] ')
    def INTLIT(self, t):
        t.value = t.value.replace('_','')
        return t
    
    
    
    ID = r'[a-zA-Z_][a-zA-Z0-9_]*'
    ID['auto'] = AUTO
    ID['break'] = BREAK
    ID['boolean'] = BOOLEAN
    ID['do'] = DO
    ID['else'] = ELSE
    ID['float'] = FLOAT
    ID['for'] = FOR
    ID['function'] = FUNCTION
    ID['if'] = IF
    ID['integer'] = INTEGER
    ID['return'] = RETURN
    ID['string'] = STRING
    ID['void'] = VOID
    ID['while'] = WHILE
    ID['out'] = OUT
    ID['continue'] = CONTINUE
    ID['of'] = OF
    ID['inherit'] = INHERIT
    ID['array'] = ARRAY
    
class MT22Parser(Parser):    
    tokens = MT22Lexer.tokens
    
    # precedence (maybe don't need this because ambiguity is resolved with the grammar rules)
    # uniminus ?
    # precedence = (
    #     ('nonassoc', 'DOUBLECOLON'),
    #     ('nonassoc', 'EQUAL', 'NOTEQUAL', 'LESSTHAN', 'LEQ', 'GREATERTHAN', 'GEQ'),
    #     ('left', 'AND', 'OR'),
    #     ('left', 'PLUS', 'MINUS'),
    #     ('left', 'MUL', 'DIV', 'MOD'),
    #     ('right', 'NOT'),
    #     ('right', 'SIGN'), # don't have a token for this precedence
    #     ('left', 'LBRACKET', 'RBRACKET')
    # )
    
    debugfile = 'parser.out'
    
    # program: decllist EOF;
    @_('decllist')
    def program(self, ctx):
        return Program(ctx.decllist)
    
    # decllist : decl decllist | decl;
    @_('decl')
    def decllist(self, ctx):
        return ctx.decl
    
    @_('decl decllist')
    def decllist(self, ctx):
        return ctx.decl + ctx.decllist
    
    
    # decl : vardecl | funcdecl;
    @_('vardecl')
    def decl(self, ctx):
        return ctx.vardecl
    
    @_('funcdecl')
    def decl(self, ctx):
        return [ctx.funcdecl]
    
    # vardecl: idlist COLON typeof SEMICOLON | ID subvardecl exp0 SEMICOLON;
    
    @_('ID COLON typeof SEMICOLON')
    def vardecl(self, ctx):
        ids = [ctx.ID] 
        typeof = ctx.typeof
        return list(map(lambda id:VarDecl(id , typeof, None), ids))
    
    @_('ID subvardecl')
    def vardecl(self, ctx):
        ids, exps, typeof = ctx.subvardecl
        ids = [ctx.ID] + ids
        return list(map(lambda id:VarDecl(id , typeof, None), ids))
    
    
    
    @_('ID subvardecl exp0 SEMICOLON')
    def vardecl(self, ctx):
        ids, exps, typeof = ctx.subvardecl
        ids = [ctx.ID] + ids
        exps = exps + [ctx.exp0]
        return list(map(lambda id, exp: VarDecl(id, typeof, exp), ids, exps))
    
    
    
    # subvardecl: COMMA ID subvardecl exp0 COMMA | COLON typeof ASSIGNMENT;
    # fix chỗ này
    @_('COMMA ID subvardecl exp0 COMMA')
    def subvardecl(self, ctx):
        print('here')
        ids, exps, typeof = ctx.subvardecl
        return [ctx.ID] + ids, exps + [ctx.exp0], typeof
    
    @_('COLON typeof ASSIGNMENT')
    def subvardecl(self, ctx):
        return [], [], ctx.typeof
    
    @_('COMMA ID subvardecl')
    def subvardecl(self, ctx):
        print('here')
        ids, exps, typeof = ctx.subvardecl
        return [ctx.ID] + ids, exps , typeof
    
    @_('COLON typeof SEMICOLON')
    def subvardecl(self, ctx):
        return [], [], ctx.typeof
    
    # idlist: ID COMMA idlist | ID;
    @_('ID COMMA idlist')
    def idlist(self, ctx):
        return [ctx.ID] + ctx.idlist
    
    @_('ID')
    def idlist(self, ctx):
        return [ctx.ID]
    
    # typeof: AUTO | atotype | arrtype;
    @_('AUTO')
    def typeof(self, ctx):
        return AutoType()
    
    @_('atotype')
    def typeof(self, ctx):
        return ctx.atotype
    
    @_('arrtype')
    def typeof(self, ctx):
        return ctx.arrtype
    
    # atotype: BOOLEAN | INTEGER | FLOAT | STRING;
    @_('BOOLEAN')
    def atotype(self, ctx):
        return BooleanType()
    
    @_('INTEGER')
    def atotype(self, ctx):
        return IntegerType()
    
    @_('FLOAT')
    def atotype(self, ctx):
        return FloatType()
    
    @_('STRING')
    def atotype(self, ctx):
        return StringType()
    
    # arrtype: ARRAY LBRACKET dime RBRACKET OF atotype;
    @_('ARRAY LBRACKET dime RBRACKET OF atotype')
    def arrtype(self, ctx):
        return ArrayType(ctx.dime, ctx.atotype)
    
    # dime: INTLIT COMMA dime | INTLIT;
    @_('INTLIT COMMA dime')
    def dime(self, ctx):
        return [int(ctx.INTLIT)] + ctx.dime
    
    @_('INTLIT')
    def dime(self, ctx):
        return [int(ctx.INTLIT)]
    
    # returntype: AUTO | atotype | VOID | arrtype;
    @_('AUTO')
    def returntype(self, ctx):
        return AutoType()
    
    @_('VOID')
    def returntype(self, ctx):
        return VoidType()
    
    @_('atotype')
    def returntype(self, ctx):
        return ctx.atotype
    
    @_('arrtype')
    def returntype(self, ctx):
        return ctx.arrtype
    
    # funcdecl: ID COLON FUNCTION returntype paramdecl (INHERIT ID)? blockstmt;
    @_('ID COLON FUNCTION returntype paramdecl blockstmt')
    def funcdecl(self, ctx):
        name = ctx.ID
        inher = None
        return FuncDecl(name, ctx.returntype, ctx.paramdecl, inher, ctx.blockstmt)
    
    @_('ID COLON FUNCTION returntype paramdecl INHERIT ID blockstmt')
    def funcdecl(self, ctx): 
        name = ctx.ID1
        inher = ctx.ID2

        return FuncDecl(name, ctx.returntype, ctx.paramdecl, inher, ctx.blockstmt)
    
    # paramdecl: LPAREN paramlist RPAREN;
    @_('LPAREN paramlist RPAREN')
    def paramdecl(self, ctx):
        return ctx.paramlist
    
    # paramlist: paramprime | ;
    @_('paramprime')
    def paramlist(self, ctx):
        return ctx.paramprime
    
    @_('empty')
    def paramlist(self, ctx):
        return []
    
    # paramprime: param COMMA paramprime | param;
    @_('param COMMA paramprime')
    def paramprime(self, ctx):
        return [ctx.param] + ctx.paramprime
    
    @_('param')
    def paramprime(self, ctx):
        return [ctx.param]
    
    # param: INHERIT? OUT? ID COLON typeof;
    @_('INHERIT OUT ID COLON typeof')
    def param(self, ctx):
        inher = True 
        out = True 
        return ParamDecl(ctx.ID, ctx.typeof, out, inher)
    
    @_('ID COLON typeof')
    def param(self, ctx):
        return ParamDecl(ctx.ID, ctx.typeof, False, False)
    
    @_('INHERIT ID COLON typeof')
    def param(self, ctx):
        return ParamDecl(ctx.ID, ctx.typeof, False, True)
    
    @_('OUT ID COLON typeof')
    def param(self, ctx):
        return ParamDecl(ctx.ID, ctx.typeof, True, False)
    
    # stmtlist: stmtprime | ;
    @_('stmtprime')
    def stmtlist(self, ctx):
        return ctx.stmtprime
    
    @_('empty')
    def stmtlist(self, ctx):
        return []
    
    # stmtprime: (stmt | vardecls) stmtlist | (stmt | vardecls);
    @_('stmt stmtlist')
    def stmtprime(self, ctx):
        return [ctx.stmt] + ctx.stmtlist
    
    @_('vardecl stmtlist')
    def stmtprime(self, ctx):
        return ctx.vardecl + ctx.stmtlist
    
    @_('vardecl')
    def stmtprime(self, ctx):
        return ctx.vardecl
    
    @_('stmt')
    def stmtprime(self, ctx):
        return [ctx.stmt]
    
    # stmt: matchstmt | unmatchif;
    @_('unmatchif')
    def stmt(self, ctx):
        return ctx.unmatchif
    
    @_('matchstmt')
    def stmt(self, ctx):
        return ctx.matchstmt
    
    # matchstmt: assigstmt | matchif | forstmt | whilestmt | dowhilestmt | breakstmt | contistmt | retstmt | callstmt | blockstmt;
    @_('assigstmt')
    def matchstmt(self, ctx):
        return ctx.assigstmt
    
    @_('matchif')
    def matchstmt(self, ctx):
        return ctx.matchif
    
    @_('forstmt')
    def matchstmt(self, ctx):
        return ctx.forstmt
    
    @_('whilestmt')
    def matchstmt(self, ctx):
        return ctx.whilestmt
    
    @_('dowhilestmt')
    def matchstmt(self, ctx):
        return ctx.dowhilestmt
    
    @_('breakstmt')
    def matchstmt(self, ctx):
        return ctx.breakstmt
    
    @_('contistmt')
    def matchstmt(self, ctx):
        return ctx.contistmt
    
    @_('retstmt')
    def matchstmt(self, ctx):
        return ctx.retstmt
    
    @_('callstmt')
    def matchstmt(self, ctx):
        return ctx.callstmt
    
    @_('blockstmt')
    def matchstmt(self, ctx):
        return ctx.blockstmt
    
    @_('ID LBRACKET expprime RBRACKET ASSIGNMENT exp0 SEMICOLON')
    def assigstmt(self, ctx):
        return AssignStmt(ArrayCell(ctx.ID, ctx.expprime), ctx.exp0)
    
    @_('ID ASSIGNMENT exp0 SEMICOLON')
    def assigstmt(self, ctx):
        return AssignStmt(ctx.ID, ctx.exp0)
    
    @_('IF LPAREN exp0 RPAREN matchstmt ELSE matchstmt')
    def matchif(self, ctx):
        return IfStmt(ctx.exp0, ctx.matchstmt1, ctx.matchstmt2)
    
    @_('IF LPAREN exp0 RPAREN matchstmt ELSE unmatchif')
    def unmatchif(self, ctx):
        return IfStmt(ctx.exp0, ctx.matchstmt, ctx.unmatchif)
    
    @_('IF LPAREN exp0 RPAREN stmt')
    def unmatchif(self, ctx):
        return IfStmt(ctx.exp0, ctx.stmt)
    
    @_('FOR LPAREN scaladecl COMMA condifor COMMA updatefor RPAREN stmt')
    def forstmt(self, ctx):
        return ForStmt(ctx.scaladecl, ctx.condifor, ctx.updatefor, ctx.stmt)
    
    @_('scalavar ASSIGNMENT exp0')
    def scaladecl(self, ctx):
        return AssignStmt(ctx.scalavar, ctx.exp0)
    
    @_('ID')
    def scalavar(self, ctx):
        return Id(ctx.ID)
    
    @_('exp0')
    def condifor(self, ctx):
        return ctx.exp0
    
    @_('exp0')
    def updatefor(self, ctx):
        return ctx.exp0
        
    @_('WHILE whilecondi stmt')
    def whilestmt(self, ctx):
        return WhileStmt(ctx.whilecondi, ctx.stmt)
    
    @_('LPAREN exp0 RPAREN')
    def whilecondi(self, ctx):
        return ctx.exp0
    
    @_('DO blockstmt WHILE LPAREN exp0 RPAREN SEMICOLON')
    def dowhilestmt(self, ctx):
        return DoWhileStmt(ctx.exp0, ctx.blockstmt)
    
    @_('BREAK SEMICOLON')
    def breakstmt(self, ctx):
        return BreakStmt()
    
    @_('CONTINUE SEMICOLON')
    def contistmt(self, ctx):
        return ContinueStmt()
    
    @_('RETURN exp0 SEMICOLON')
    def retstmt(self, ctx):
        return ReturnStmt(ctx.exp0)
    
    @_('RETURN SEMICOLON')
    def retstmt(self, ctx):
        return ReturnStmt()
        
    @_('ID LPAREN explist RPAREN SEMICOLON')
    def callstmt(self, ctx):
        return CallStmt(ctx.ID, ctx.explist)
    
    @_('LBRACE stmtlist RBRACE')
    def blockstmt(self, ctx):
        return BlockStmt(ctx.stmtlist)
    
    @_('ID LPAREN explist RPAREN')
    def funcall(self, ctx):
        return FuncCall(ctx.ID, ctx.explist)
    
    @_('LBRACE explist RBRACE')
    def arraylit(self, ctx):
        return ArrayLit(ctx.explist)
    
    @_('expprime')
    def explist(self, ctx):
        return ctx.expprime
    
    @_('empty')
    def explist(self, ctx):
        return []
        
    @_('exp0 COMMA expprime')
    def expprime(self, ctx):
        return [ctx.exp0] + ctx.expprime
    
    @_('exp0')
    def expprime(self, ctx):
        return [ctx.exp0]
    
    @_('exp1 DOUBLECOLON exp1')
    def exp1(self, ctx):
        left = ctx.exp11
        right = ctx.exp12
        return BinExpr(ctx.DOUBLECOLON, left, right)
    
    @_('exp1')
    def exp0(self, ctx):
        return ctx.exp1
    
    @_('exp2 GEQ exp2')
    def exp1(self, ctx):
        left = ctx.exp21
        right = ctx.exp22
        return BinExpr(ctx.GEQ, left, right)
    
    @_('exp2 GREATERTHAN exp2')
    def exp1(self, ctx):
        left = ctx.exp21
        right = ctx.exp22
        return BinExpr(ctx.GREATERTHAN, left, right)
    
    @_('exp2 LEQ exp2')
    def exp1(self, ctx):
        left = ctx.exp21
        right = ctx.exp22
        return BinExpr(ctx.LEQ, left, right)
    
    @_('exp2 LESSTHAN exp2')
    def exp1(self, ctx):
        left = ctx.exp21
        right = ctx.exp22
        return BinExpr(ctx.LESSTHAN, left, right)
    
    @_('exp2 NOTEQUAL exp2')
    def exp1(self, ctx):
        left = ctx.exp21
        right = ctx.exp22
        return BinExpr(ctx.NOTEQUAL, left, right)
    
    @_('exp2 EQUAL exp2')
    def exp1(self, ctx):
        left = ctx.exp21
        right = ctx.exp22
        return BinExpr(ctx.EQUAL, left, right)
    
    @_('exp2')
    def exp1(self, ctx):
        return ctx.exp2
    
    @_('exp2 OR exp3')
    def exp2(self, ctx):
        return BinExpr(ctx.OR, ctx.exp2, ctx.exp3)
    
    @_('exp2 AND exp3')
    def exp2(self, ctx):
        return BinExpr(ctx.AND, ctx.exp2, ctx.exp3)
    
    @_('exp3')
    def exp2(self, ctx):
        return ctx.exp3
    
    @_('exp3 MINUS exp4')
    def exp4(self, ctx):
        return BinExpr(ctx.MINUS, ctx.exp3, ctx.exp4)
    
    @_('exp3 PLUS exp4')
    def exp4(self, ctx):
        return BinExpr(ctx.PLUS, ctx.exp3, ctx.exp4)
    
    @_('exp4')
    def exp3(self, ctx):
        return ctx.exp4
    
    @_('exp4 MOD exp5')
    def exp4(self, ctx):
        return BinExpr(ctx.MOD, ctx.exp4, ctx.exp5)
    
    @_('exp4 DIV exp5')
    def exp4(self, ctx):
        return BinExpr(ctx.DIV, ctx.exp4, ctx.exp5)
    
    @_('exp4 MUL exp5')
    def exp4(self, ctx):
        return BinExpr(ctx.MUL, ctx.exp4, ctx.exp5)
    
    @_('exp5')
    def exp4(self, ctx):
        return ctx.exp5
    
    @_('NOT exp5')
    def exp5(self, ctx):
        expr = ctx.exp5
        return UnExpr(ctx.NOT, expr)
    
    @_('exp6')
    def exp5(self, ctx):
        return ctx.exp6
    
    @_('MINUS exp6')
    def exp6(self, ctx):
        expr = ctx.epx6
        return UnExpr(ctx.MINUS, expr)
    
    @_('exp7')
    def exp6(self, ctx):
        return ctx.exp7
    
    @_('ID LBRACKET expprime RBRACKET')
    def exp7(self, ctx):
        id_name = ctx.ID
        exprs = ctx.expprime
        return ArrayCell(id_name,exprs)
    
    @_('exp8')
    def exp7(self, ctx):
        return ctx.exp8
    
    @_('funcall')
    def exp8(self, ctx):
        return ctx.funcall
    @_('exp9')
    def exp8(self, ctx):
        return ctx.exp9
    
    @_('INTLIT')
    def exp9(self, ctx):

        return IntegerLit(int(ctx.INTLIT))
    
    @_('FLOATLIT')
    def exp9(self, ctx):
        return FloatLit(float(ctx.FLOATLIT))
    
    @_('BOOLEANLIT')
    def exp9(self, ctx):
        return BooleanLit(ctx.BOOLEANLIT == 'true')
    
    @_('STRINGLIT')
    def exp9(self, ctx):
        return StringLit(ctx.STRINGLIT)
    
    @_('ID')
    def exp9(self, ctx):
        return Id(ctx.ID)
    
    @_('arraylit')
    def exp9(self, ctx):
        return ctx.arraylit
    
    @_('exp10')
    def exp9(self, ctx):
        return ctx.exp10
    
    @_('LPAREN exp0 RPAREN')
    def exp10(self, ctx):
        return ctx.exp0
    
    @_('')
    def empty(self, p):
        pass
    

if __name__ == '__main__':
    data = """voidA: function integer(n: integer){
                return n%10;
            }
            voidB: function void (out n: integer, delta: integer){
                n = n + voidA(delta);
            }
            main: function void () {
                delta: integer = 5;
                voidB(x,delta);
                printInt(x);
            }
        """
    lexer = MT22Lexer()
    parser = MT22Parser()
    tks = lexer.tokenize(data)
    # for t in tks:
    #     print(t)
    result = parser.parse(tks)
    print(result)