# Analizador léxico y sintáctico para un subconjunto de FORTRAN 77

Proyecto de la asignatura **INFO1148 – Tarea 3 (Semestre II-2025)**.  
Implementa en **Python 3** un analizador **léxico** y **sintáctico (LL(1), descenso recursivo)** para un **subconjunto** de FORTRAN 77.

---

## ✨ Alcance (subconjunto soportado)

- **Estructura de programa**: `PROGRAM <ident> ... END`
- **Asignaciones**: `IDENT = <expresion>`
- **Condicional**: `IF (<condicion>) THEN <sentencias> [ELSE <sentencias>] ENDIF`
- **Expresiones aritméticas** con `+ - * /`
- **Condiciones** con operadores relacionales de FORTRAN 77:
  - `.EQ.  .NE.  .GT.  .LT.  .GE.  .LE.`

> **Importante**: el analizador **exige** los operadores relacionales con puntos (p. ej., `A .EQ. B`), no `A = B`.

---

## 🧩 Léxico (tokens principales)

- **Palabras reservadas**: `PROGRAM`, `END`, `IF`, `THEN`, `ELSE`, `ENDIF`, `INTEGER`, `REAL`, `PRINT`, `READ`
- **Identificadores**: `[A-Z][A-Z0-9]*` (sin `_`, estilo FORTRAN 77)
- **Números**: enteros o reales con punto (`\d+(\.\d+)?`)
- **Relacionales**: `.(EQ|NE|GT|LT|GE|LE).`
- **Aritméticos**: `+ - * /`
- **Paréntesis/coma/asignación**: `(` `)` `,` `=`
- **Comentarios**: líneas que **comienzan** con `C` o `*` (modo multilinea)

El código normaliza la entrada a **mayúsculas**.

---

## 🧠 Gramática (informal)

