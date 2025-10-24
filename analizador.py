import re
from pathlib import Path

PALABRAS_RESERVADAS = {
    "PROGRAM", "END", "IF", "THEN", "ELSE", "ENDIF",
    "INTEGER", "REAL", "PRINT", "READ"
}

TOKENS = [
    ("NUMERO",      r"\d+(\.\d+)?"),
    ("IDENT",       r"[A-Z][A-Z0-9]*"),
    ("RELOP",       r"\.(EQ|NE|GT|LT|GE|LE)\."),  # ya definido, se usará en v4
    ("ASIGNACION",  r"="),
    ("SUMA",        r"\+"),
    ("RESTA",       r"-"),
    ("MULT",        r"\*"),
    ("DIV",         r"/"),
    ("LPAREN",      r"\("),
    ("RPAREN",      r"\)"),
    ("COMA",        r","),
    ("COMENTARIO",  r"^[C\].$"),
    ("ESPACIO",     r"[ \t\n\r\f\v]+"),
    ("DESCONOCIDO", r"."),
]

escaner = re.compile("|".join(f"(?P<{n}>{r})" for n, r in TOKENS), re.MULTILINE)

def lexer(texto: str):
    tokens_encontrados = []
    for m in escaner.finditer(texto.upper()):
        tipo = m.lastgroup
        lexema = m.group().strip()
        if not lexema or tipo in ("ESPACIO", "COMENTARIO"):
            continue
        if tipo == "IDENT" and lexema in PALABRAS_RESERVADAS:
            tipo = lexema
        elif tipo == "DESCONOCIDO":
            raise SyntaxError(f"Símbolo no reconocido: {lexema}")
        tokens_encontrados.append((tipo, lexema))
    tokens_encontrados.append(("EOF", ""))
    return tokens_encontrados

def main():
    script_dir = Path(_file_).resolve().parent
    ruta = script_dir / "entrada.txt"
    if not ruta.exists():
        print(f"[ERROR] No se encontró el archivo: {ruta}")
        return
    with open(ruta, encoding="utf-8") as f:
        codigo = f.read()

    print("\n=== ETAPA 1: ANÁLISIS LÉXICO ===")
    for t in lexer(codigo):
        print(t)

if _name_ == "_main_":
    main()