# Buscando el Nirvana Funcional: Implementando Lox en Haskell

*Una crónica de tipos, mónadas y cómo reescribir la herencia de objetos usando matemáticas.*

Como programador que viene de lenguajes como **Rust** y **Go**, mi relación con la programación funcional (FP) ha sido siempre de admiración distante. Entiendo los conceptos, uso `.map` y `.filter`, y Rust me ha enseñado a amar (y temer) al sistema de tipos. Pero **Haskell** siempre fue esa torre de marfil en la colina: puro, perezoso y lleno de terminología matemática intimidante.

Para romper esa barrera, decidí implementar **Lox**, el lenguaje del excelente libro [*Crafting Interpreters*](https://craftinginterpreters.com/) de Robert Nystrom. El libro usa Java para su primera implementación (`jlox`), un lenguaje puramente orientado a objetos. Mi reto: traducir esa arquitectura OOP a los paradigmas de Haskell.

Lo que encontré no fue solo una traducción de código, sino una forma completamente distinta de pensar en datos, errores y estado.

## 1. El Entorno: Reproducibilidad con Nix

Antes de escribir una línea de Haskell, necesitaba un entorno sólido. Viniendo de `cargo` (Rust) o `npm` (JS), la gestión de dependencias de Haskell (Cabal/Stack) puede ser confusa. Aquí es donde **Nix Flakes** brilla.

No solo gestiona el compilador GHC y las dependencias del proyecto, sino que me permitió integrar la **suite de tests oficial de Lox** (escrita en Dart) directamente en mi flujo de trabajo.

En mi `flake.nix`, definí un script que descarga el repositorio original de `craftinginterpreters`, configura Dart, y ejecuta sus tests contra mi binario compilado de Haskell:

```nix
# flake.nix (extracto)
crafting-interpreters-script = interpreter:
  pkgs.writers.writeHaskellBin "crafting-interpreters-script" { ... } ''
    -- Script que descarga los tests de Dart y los lanza contra mi intérprete
    dart "tool/bin/test.dart" target "--interpreter" "${pkgs.lib.getExe interpreter}" ...
  '';
```

Esto me dio una confianza absoluta: si mi intérprete pasaba los tests oficiales, mi lógica era correcta, independientemente del lenguaje.

## 2. Adiós Visitor, Hola Pattern Matching

En la implementación Java, Nystrom usa el **Visitor Pattern** para recorrer el Árbol de Sintaxis Abstracta (AST). Es la solución clásica de OOP para separar los algoritmos (interpretación, resolución) de las clases de datos.

En Haskell, esto es innecesario. Los datos se modelan con **Tipos de Datos Algebraicos (ADTs)** y el comportamiento con **Pattern Matching**.

Mi AST de expresiones (`Expression.hs`) se ve así:

```haskell
data Expression (p :: Phase)
  = Literal Literal
  | BinaryOperation Int BinaryOperator (Expression p) (Expression p)
  | VariableExpr Int String (ResolutionInfo p)
  -- ...
```

Y el intérprete (`evaluateExpr` en `Interpreter.hs`) es simplemente una función que hace match sobre estos constructores:

```haskell
evaluateExpr :: Expression 'Resolved -> Interpreter Value
evaluateExpr (Literal lit) = pure $ evalLiteral lit
evaluateExpr (BinaryOperation line op e1 e2) = executeBinary line op e1 e2
-- ...
```

Es más conciso y, en mi opinión, mucho más legible que docenas de clases `visitBinary`, `visitGrouping`, etc.

## 3. El Gran Reto: "Resolution Distance" y Árboles que Crecen

Uno de los desafíos más interesantes fue el capítulo de **Resolución de Variables**. En Lox, para manejar cierres (closures) correctamente, necesitamos saber a cuántos "saltos" (scopes) de distancia está definida una variable.

### La solución Java: Side-Tables
En Java, el Resolver recorre el árbol y guarda esta información en un `Map<Expr, Integer>` separado. El AST no cambia. Esto funciona en Java porque cada objeto `Expr` tiene identidad (dirección de memoria) y puede usarse como clave en un mapa.

### El problema en Haskell
En Haskell, los valores son inmutables y estructurales. `Variable "a"` es idéntico a otro `Variable "a"`. No tienen "identidad" por defecto. Para usar un `Map`, necesitaría adjuntar IDs únicos a cada nodo o usar trucos inseguros.

### La solución Haskell: Tipos Dependientes de la Fase
En lugar de una tabla lateral, decidí que la información de resolución debía vivir **dentro** del propio AST. Pero el Parser no conoce esa información todavía. ¿Cómo definimos un tipo que a veces tiene resolución y a veces no?

Usé una técnica inspirada en "Trees That Grow", utilizando **Data Kinds** y **Type Families**:

```haskell
-- Definimos las fases del compilador
data Phase = Unresolved | Resolved

-- Una familia de tipos que cambia según la fase
type family ResolutionInfo (p :: Phase) :: Type where
  ResolutionInfo 'Unresolved = NotResolved  -- Un tipo vacío
  ResolutionInfo 'Resolved = Resolution     -- La distancia calculada (Global | Local Int)

-- El AST usa esta familia de tipos
data Expression (p :: Phase)
  = VariableExpr
      Int                -- Línea
      String             -- Nombre
      (ResolutionInfo p) -- ¡Cambia según la fase!
  -- ...
```

Esto es **Type-Driven Development** puro.
1. El `Parser` produce un `Program 'Unresolved`.
2. El `Resolver` toma un `Program 'Unresolved` y devuelve un `Program 'Resolved`.
3. El `Interpreter` **solo** acepta `Program 'Resolved`.

```haskell
-- Interpreter.hs
programInterpreter :: Program 'Unresolved -> Interpreter ()
programInterpreter prog = do
  let (resolvedProg, errors) = runResolver (programResolver prog)
  if null errors
    then interpretProgram resolvedProg -- Aquí GHC sabe que es seguro
    else throwError (Resolve errors)
```

Si intento ejecutar un programa sin resolver, ¡el código ni siquiera compila! Es una garantía de seguridad que Java no puede ofrecer tan fácilmente.

## 4. Gestionando el Estado: Monad Transformers

En Java, el intérprete es una clase con campos mutables (`environment`). Si hay un error, lanza una excepción.

En Haskell, la mutabilidad y los efectos deben ser explícitos. Mi "pila" de mónadas (`InterpreterT`) evolucionó varias veces (como se ve en mi historia de `git`) hasta llegar a esto:

```haskell
newtype InterpreterT m a = Interpreter
  { runInterpreterT :: StateT (ProgramState Value) (ExceptT InterpreterError m) a
  }
```

Es una cebolla de efectos:
1.  **IO**: Para imprimir en consola (`putStrLn`).
2.  **ExceptT**: Para manejar errores (`throwError`). Reemplaza a las excepciones de Java.
3.  **StateT**: Para llevar el entorno (variables, funciones).

Un detalle elegante es cómo se manejan los scopes. En Java se usa `try-finally` para asegurar que, al salir de un bloque, el entorno anterior se restaura. En Haskell, uso `catchError` junto con las primitivas de estado:

```haskell
executeBlock decls = do
  modify pushScope        -- Entrar en nuevo scope
  -- Si 'go decls' falla, ejecutamos el handler que hace popScope y relanza el error
  r <- catchError (go decls) (\e -> modify popScope >> throwError e)
  modify popScope         -- Salir del scope (caso exitoso)
  pure r
```

## 5. Errores como Valores

Una de las diferencias filosóficas más grandes es el manejo de errores. En lugar de que el flujo de control salte mágicamente (Exceptions), los errores son valores (`Left InterpreterError`) que se propagan.

Aunque `ExceptT` hace que parezca imperativo (puedes hacer short-circuiting), te obliga a ser consciente de en qué partes del código pueden fallar las cosas. No hay "Unchecked Exceptions" aquí.

## Conclusión

Implementar Lox en Haskell ha sido un ejercicio de traducción cultural.

*   Donde OOP ve **Comportamiento encapsulado con Datos**, FP ve **Datos puros y Funciones transformadoras**.
*   Donde OOP usa **Identidad de Objetos**, FP usa **Tipos Estructurales**.
*   Donde OOP usa **Excepciones y Mutabilidad**, FP usa **Mónadas y Transformadores**.

No sé si he alcanzado el "Nirvana", pero arquitecturas como la **Resolución basada en Tipos** me han mostrado un poder de expresividad y seguridad que difícilmente quiero abandonar para volver al mundo de `void foo()`.

El código completo está disponible en el repositorio. ¡Feliz hacking funcional!
