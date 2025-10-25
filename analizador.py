import re
from pathlib import Path

PALABRAS_RESERVADAS = {
    "PROGRAM", "END", "IF", "THEN", "ELSE", "ENDIF",
    "INTEGER", "REAL", "PRINT", "READ"
}

TOKENS = [
    ("NUMERO",      r"\d+(\.\d+)?"),
    ("IDENT",       r"[A-Z][A-Z0-9]*"),
    ("RELOP",       r"\.(EQ|NE|GT|LT|GE|LE)\."),  # se usará en v4
    ("ASIGNACION",  r"="),
    ("SUMA",        r"\+"),
    ("RESTA",       r"-"),
    ("MULT",        r"\*"),
    ("DIV",         r"/"),
    ("LPAREN",      r"\("),
    ("RPAREN",      r"\)"),
    ("COMA",        r","),
    ("COMENTARIO",  r"^[C\*].*$"),
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

class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.pos = 0
        self.actual = self.tokens[self.pos]

    def avanzar(self):
        self.pos += 1
        if self.pos < len(self.tokens):
            self.actual = self.tokens[self.pos]

    def esperar(self, tipo):
        if self.actual[0] == tipo:
            self.avanzar()
        else:
            raise SyntaxError(f"Se esperaba '{tipo}' pero se encontró '{self.actual[1]}'.")

    def parsear(self):
        print("\n=== ETAPA 2: ANÁLISIS SINTÁCTICO ===")
        print("Regla inicial: <programa>")
        self.programa()
        if self.actual[0] != "EOF":
            raise SyntaxError("Existen tokens extra después de END.")
        print("\nAnálisis sintáctico completado correctamente.\n")

    # <programa> → PROGRAM IDENT <sentencias> END
    def programa(self):
        print("→ Reconociendo: PROGRAM IDENT ... END")
        self.esperar("PROGRAM")
        prog_name = self.actual[1]
        self.esperar("IDENT")
        print(f"  Nombre del programa: {prog_name}")
        self.sentencias(nivel=1)
        self.esperar("END")
        print(f"Programa '{prog_name}' finalizado correctamente.")

    # <sentencias> → <sentencia> <sentencias> | ε
    def sentencias(self, nivel):
        while self.actual[0] in {"IDENT"}:  # sin IF todavía
            self.sentencia(nivel)

    # <sentencia> → <asignacion>
    def sentencia(self, nivel):
        sangria = "  " * nivel
        if self.actual[0] == "IDENT":
            print(f"{sangria}Sentencia: Asignación detectada")
            self.asignacion(nivel + 1)
        else:
            raise SyntaxError(f"Sentencia inesperada: {self.actual}")

    # <asignacion> → IDENT = <expresion>
    def asignacion(self, nivel):
        sangria = "  " * nivel
        ident = self.actual[1]
        self.esperar("IDENT")
        self.esperar("ASIGNACION")
        print(f"{sangria}{ident} ← (expresión)")
        self.expresion(nivel + 1)

    # <expresion> → {IDENT | NUMERO} {op {IDENT | NUMERO}}*
    def expresion(self, nivel):
        sangria = "  " * nivel
        if self.actual[0] in {"IDENT", "NUMERO"}:
            print(f"{sangria}Operando: {self.actual[1]}")
            self.avanzar()
            while self.actual[0] in {"SUMA", "RESTA", "MULT", "DIV"}:
                operador = self.actual[1]
                self.avanzar()
                if self.actual[0] in {"IDENT", "NUMERO"}:
                    print(f"{sangria}Operador: {operador}  Operando: {self.actual[1]}")
                    self.avanzar()
                else:
                    raise SyntaxError("Expresión inválida después de operador.")
        else:
            raise SyntaxError("Expresión inválida: se esperaba número o identificador.")

def main():
    script_dir = Path(__file__).resolve().parent
    ruta = script_dir / "entrada.txt"
    if not ruta.exists():
        print(f"[ERROR] No se encontró el archivo: {ruta}")
        return
    with open(ruta, encoding="utf-8") as f:
        codigo = f.read()

    print("\n=== ETAPA 1: ANÁLISIS LÉXICO ===")
    tokens = lexer(codigo)
    for t in tokens:
        print(t)

    parser = Parser(tokens)
    try:
        parser.parsear()
    except SyntaxError as e:
        print(f"\nError sintáctico: {e}\n")

if __name__ == "__main__":
    main()