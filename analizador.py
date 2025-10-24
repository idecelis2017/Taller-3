import re
from pathlib import Path

# ============================================================
# === ETAPA 1: ANALIZADOR LÉXICO ============================
# ============================================================

PALABRAS_RESERVADAS = {
    "PROGRAM", "END", "IF", "THEN", "ELSE", "ENDIF",
    "INTEGER", "REAL", "PRINT", "READ"
}

TOKENS = [
    ("NUMERO",      r"\d+(\.\d+)?"),
    ("IDENT",       r"[A-Z][A-Z0-9]*"),                 # sin '_', estilo FORTRAN77
    ("RELOP",       r"\.(EQ|NE|GT|LT|GE|LE)\."),        # .EQ. .NE. .GT. .LT. .GE. .LE.
    ("ASIGNACION",  r"="),
    ("SUMA",        r"\+"),
    ("RESTA",       r"-"),
    ("MULT",        r"\*"),
    ("DIV",         r"/"),
    ("LPAREN",      r"\("),
    ("RPAREN",      r"\)"),
    ("COMA",        r","),
    ("COMENTARIO",  r"^[C\*].*$"),                      # línea que empieza con C o *
    ("ESPACIO",     r"[ \t\n\r\f\v]+"),
    ("DESCONOCIDO", r"."),
]

# Compilación del patrón unificado (anclado a multilínea para comentarios)
escaner = re.compile("|".join(f"(?P<{n}>{r})" for n, r in TOKENS), re.MULTILINE)

def lexer(texto: str):
    """
    Convierte el código fuente en una lista de tokens (tipo, lexema).
    - Normaliza a MAYÚSCULAS (FORTRAN77 es case-insensitive).
    - Omite espacios y comentarios.
    - Lanza SyntaxError ante símbolos desconocidos.
    """
    tokens_encontrados = []
    for m in escaner.finditer(texto.upper()):
        tipo = m.lastgroup
        lexema = m.group().strip()

        if not lexema or tipo in ("ESPACIO", "COMENTARIO"):
            continue

        if tipo == "IDENT" and lexema in PALABRAS_RESERVADAS:
            tipo = lexema  # p. ej., IF, END, PROGRAM, etc.
        elif tipo == "DESCONOCIDO":
            raise SyntaxError(f"Símbolo no reconocido: {lexema}")

        tokens_encontrados.append((tipo, lexema))

    tokens_encontrados.append(("EOF", ""))  # Fin de archivo
    return tokens_encontrados


# ============================================================
# === ETAPA 2: ANALIZADOR SINTÁCTICO ========================
# ============================================================

class Parser:
    """
    Analizador sintáctico recursivo descendente para un subconjunto de FORTRAN 77:
    - PROGRAM ... END
    - Asignaciones: IDENT = expresión
    - IF (condición) THEN [sentencias] [ELSE sentencias] ENDIF
    - condición: IDENT RELOP (IDENT | NUMERO)   ; RELOP = .EQ. .NE. .GT. .LT. .GE. .LE.
    """
    def __init__(self, tokens):
        self.tokens = tokens
        self.pos = 0
        self.actual = self.tokens[self.pos]

    # --- utilidades ---
    def avanzar(self):
        self.pos += 1
        if self.pos < len(self.tokens):
            self.actual = self.tokens[self.pos]

    def esperar(self, tipo):
        if self.actual[0] == tipo:
            self.avanzar()
        else:
            raise SyntaxError(f"Se esperaba '{tipo}' pero se encontró '{self.actual[1]}'.")

    # --- gramática (comentada) ---
    # <programa>   → PROGRAM IDENT <sentencias> END
    # <sentencias> → <sentencia> <sentencias> | ε
    # <sentencia>  → <asignacion> | <if>
    # <asignacion> → IDENT = <expresion>
    # <if>         → IF ( <condicion> ) THEN <sentencias> [ELSE <sentencias>] ENDIF
    # <condicion>  → IDENT RELOP (IDENT | NUMERO)
    # <expresion>  → {IDENT | NUMERO} { ( + | - | * | / ) {IDENT | NUMERO} }*

    def parsear(self):
        print("\n=== ETAPA 2: ANÁLISIS SINTÁCTICO ===")
        print("Regla inicial: <programa>")
        self.programa()
        if self.actual[0] != "EOF":
            raise SyntaxError("Existen tokens extra después de END.")
        print("\nAnálisis sintáctico completado correctamente.\n")

    def programa(self):
        print("→ Reconociendo: PROGRAM IDENT ... END")
        self.esperar("PROGRAM")
        prog_name = self.actual[1]
        self.esperar("IDENT")
        print(f"  Nombre del programa: {prog_name}")
        self.sentencias(nivel=1)
        self.esperar("END")
        print(f"Programa '{prog_name}' finalizado correctamente.")

    def sentencias(self, nivel):
        while self.actual[0] in {"IDENT", "IF"}:
            self.sentencia(nivel)

    def sentencia(self, nivel):
        sangria = "  " * nivel
        if self.actual[0] == "IDENT":
            print(f"{sangria}Sentencia: Asignación detectada")
            self.asignacion(nivel + 1)
        elif self.actual[0] == "IF":
            print(f"{sangria}Sentencia: Estructura IF detectada")
            self.if_stmt(nivel + 1)
        else:
            raise SyntaxError(f"Sentencia inesperada: {self.actual}")

    def asignacion(self, nivel):
        sangria = "  " * nivel
        ident = self.actual[1]
        self.esperar("IDENT")
        self.esperar("ASIGNACION")
        print(f"{sangria}{ident} ← (expresión)")
        self.expresion(nivel + 1)

    def if_stmt(self, nivel):
        sangria = "  " * nivel
        self.esperar("IF")
        self.esperar("LPAREN")
        print(f"{sangria}Condición IF:")
        self.condicion(nivel + 1)
        self.esperar("RPAREN")
        self.esperar("THEN")
        print(f"{sangria}Bloque THEN:")
        self.sentencias(nivel + 1)
        if self.actual[0] == "ELSE":
            self.avanzar()
            print(f"{sangria}Bloque ELSE:")
            self.sentencias(nivel + 1)
        self.esperar("ENDIF")
        print(f"{sangria}Fin del IF")

    def condicion(self, nivel):
        sangria = "  " * nivel
        izq = self.actual[1]
        self.esperar("IDENT")
        op = self.actual[1]
        self.esperar("RELOP")              # exige .EQ. .NE. .GT. .LT. .GE. .LE.
        if self.actual[0] in {"IDENT", "NUMERO"}:
            der = self.actual[1]
            self.avanzar()
        else:
            raise SyntaxError("Se esperaba IDENT o NUMERO en la condición.")
        print(f"{sangria}Comparación: {izq} {op} {der}")

    def expresion(self, nivel):
        # Implementación simple (sin precedencia estricta). Válida para nuestro objetivo.
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


# ============================================================
# === ETAPA 3: INTERFAZ PRINCIPAL ===========================
# ============================================================

def main():
    # Siempre toma 'entrada.txt' desde la MISMA CARPETA del script
    script_dir = Path(__file__).resolve().parent
    ruta = script_dir / "entrada.txt"

    if not ruta.exists():
        print(f"[ERROR] No se encontró el archivo: {ruta}")
        print("Crea 'entrada.txt' en la misma carpeta que 'analizador.py'.")
        return

    with open(ruta, encoding="utf-8") as f:
        codigo = f.read()

    print(f"\n=== ETAPA 1: ANÁLISIS LÉXICO ===")
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
; y que tu entrada.txt sea asi; PROGRAM PRUEBA
A = 5
B = A + 3
END
