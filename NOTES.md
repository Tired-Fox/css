# Notes

## Prelude

### CSS Document

Made up of `style rules`, `qualified rules`, and `at-rules`.

- Qualified Rule: `prelude` followed by a `{}` block containing a list of `definitions`
- Style Rule: A `qualified rule` with a prelude being a `selector`
    ```css
    p > a {
        /* ... */
    }
    ```
- At-Rule: Start with `@` code point followed by their `name` as a css keyword. At rules contents after this point are up to the rule. However, their pattern is usually a list of CSS ending with a semicolon, or a block (`{}`) containing more CSS.

### Escaping

Any unicode code point can be inside an identifier or a quoted string by escaping it.

Starts with `\` and is followed by: 
    - Any unicode code point that isn't hex digits or a newline; replace escape with code point.
    - One to six hex digits followed by an optional whitespace. The escape is replaced with the given unicode code point provided by the hex digits.

### Error Handling

Errors attempt a graceful recovery and throw away the least amount of css as possible. This allows for an old parser to be able to handle new syntax or unrecognized syntax.

- Top Level: <at-keyword-token> starts an at-rule. Anything else starts a qualified rule and is part of it's prelude. This could make invalid selectors and rules but that is not a problem for the parser.
- When an at-rule starts everything is valid, the rule ends with a semi colon while a `{` starts the rules body. The body ends when a balanced matching `}` is found.
- Qualified rules: everything is valid ans is part of it's prelude. Semicolons do not end the rule instead it becomes part of the prelude. The first block (`{}`) is it's body and is always parsed as a declaration list.
- List of Declarations: When an invalid token is parsed it discards the current declaration and seeks forward until it finds the end of the current declaration and starts fresh.
- When the stylesheet is finished anything that is still open is automatically closed. This may cause incomplete statements, but that is a problem for the grammer checker.

After each construct: declaration, style rule, at-rule) is parsed, it is validated with it's grammer. If it does not match it is tossed out.

### User Agent

The parser processing the text/css. Parses the text/css as to the specification regardless of the grammer. Only when a construct is finished will it consult the grammer. When a `parse error` occurs it should act as the specification specifies or abort immediatly.

### Conformance Checker

Must report at least one error if one or more errors exist, and report none if no errors exist. Conformance checkers are not required to recover from `parse errors` but if they do the recovery should match the user agents recovery.


## Parsing Model

- Input is a stream of unicode code points, which is passed through a tokenization phase followed by a tree construction phase. Output is a CSSStylesheet object.

### Input Stream

User agent must decode a stream of bytes to the proper encoding. The user agent may determine the fallback encoding and let a `fallback` be the result, or decode the stylesheet with a fallback encoding and return the result.  

To determine the encoding, it can either be passed to the user agent from a known source like a `Content-Type` http header or parsed from the first 1024 bytes from the input stream. If the bytes begin with `40 63 68 61 72 73 65 74 20 22 XX* 22 3B` where `XX*` is 0-21 base 16 inclusive or 23-7F base 16 inclusive. Transform all `XX` bytes as a string to get the encoding.
