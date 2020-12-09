import ply.lex as lex
import ply.yacc as yacc
import MeCab as mecab

# トークンのリスト
tokens = (
    'ID',
    'TASU',
    'HIKU',
    'NUMBER',
    'VAR',
    'NI',
    'WO',
    'HA',
    'NO',
    'KARA',
)

# 各トークンの定義
t_ignore = ' \t'
t_VAR = r'[一-龥ぁ-んァ-ン_][一-龥ぁ-んァ-ン0-9_]*'

def t_ID(t):
    r'[一-龥ぁ-んァ-ン_][一-龥ぁ-んァ-ン0-9_]*'
    t.type = reserved.get(t.value,'VAR')
    return t

def t_NUMBER(t) :
    r'[0-9]+'
    t.value = int(t.value)
    return t

def t_newline(t):
  r'\n+'
  t.lexer.lineno += len(t.value)

def t_error(t):
  print("Invalid Token:",t.value[0])
  t.lexer.skip(1)

# 予約後
reserved = {
    '足す' : 'TASU',
    '引く' : 'HIKU',
    'に' : 'NI',
    'を' : 'WO',
    'は' : 'HA',
    'の' : 'NO',
    'から' : 'KARA',
}

# lexerの構築
lexer = lex.lex()

names = {}

def p_prog(p):
    'prog : sentence'
    p[0] = p[1]

def p_sentence(p):
    '''sentence : formula
        | assign
        | against'''
    p[0] = p[1]

def p_assign1(p):
    'assign : VAR HA formula'
    if p[1] not in names:
        names[p[1]] = {}
    names[p[1]] = p[3]
    p[0] = names[p[1]]

def p_assign2(p):
    'assign : VAR NO VAR HA formula'
    if p[1] not in names:
        names[p[1]] = {}
    if p[3] not in names[p[1]]:
        names[p[1]][p[3]] = {}
    names[p[1]][p[3]] = p[5]
    p[0] = names[p[1]][p[3]]

def p_against1(p):
    '''against : VAR NI formula WO TASU
        | VAR KARA formula WO HIKU'''
    if p[5] == '足す':
        names[p[1]] += p[3]
    elif p[5] == '引く':
        names[p[1]] -= p[3]
    p[0] = names[p[1]]

def p_against2(p):
    '''against : VAR NO VAR NI formula WO TASU
        | VAR NO VAR KARA formula WO HIKU'''
    if p[7] == '足す':
        names[p[1]][p[3]] += p[5]
    elif p[7] == '引く':
        names[p[1]][p[3]] -= p[5]
    p[0] = names[p[1]][p[3]]

def p_formula(p):
    'formula : term'
    p[0] = p[1]

def p_term(p):
    '''term : term TASU factor
        | term HIKU factor
        | factor'''
    if len(p) < 3:
        p[0] = p[1]
    elif p[2] == '足す':
        p[0] = p[1] + p[3]
    elif p[2] == '引く':
        p[0] = p[1] - p[3]

def p_factor_num(p):
    'factor : NUMBER'
    p[0] = p[1]

def p_factor_var1(p):
    'factor : VAR'
    p[0] = names[p[1]]

def p_factor_var2(p):
    '''factor : VAR NO VAR'''
    p[0] = names[p[1]][p[3]]

def p_error(p):
    print(p)
    print("Syntax error in input!")

parser = yacc.yacc()

def w_separator(line):
    tagger = mecab.Tagger()
    words = tagger.parse(line).split('\n')
    wordsList = []
    code = ""
    for v in words:
        w = v.split()
        if w == ['EOS']:
            break
        word = {'value': w[0], 'kind': w[1].split(',')[0]}
        wordsList.append(word)
    for i in range(len(wordsList)-2, -1, -1):
        if wordsList[i+1]["value"] in reserved:
            continue
        if wordsList[i+1]["kind"] == '名詞' and wordsList[i]["kind"] == '名詞':
            wordsList[i]["value"] += wordsList[i+1]["value"]
            wordsList.pop(i+1)
    for v in wordsList:
        code += v["value"] + ' '
    return code


def main():
    while True:
        try:
            line = input('>')
        except EOFError:
            break
        if not line:
            continue
        code = w_separator(line)
        res = parser.parse(code)
        print(res)
        # print(names)

main()
