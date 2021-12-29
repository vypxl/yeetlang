# The yeet programming language specification *WIP*

Yeet is a beautifully simple functional language based on the ingenious [lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus).

## Syntax

In yeet, only the following symbols are needed to write code, everything else (whitespace, other characters or invalid keywords/identifiers) are unnecessary and can thus be used as comments, if the programmer fears their inability to compoehend their own code the day after.

- yeet: `yeet` is the only reserved keyword in **yeet**. It is used in function declarations.
- identifiers: `yee..et` (more than 2 e's) are **yeet** identifiers.
- numbers: `YEeEeEet` (capital `Y`, then `e` or `E` for binary `0` and `1`) may be used as a shorthand for numbers, if the programmer whishes to shorten his sophisticated arithmetic code. These literals must contain at least two `e`'s or `E`'s, so to represent a 0 would be `Yeet` and a 1 would be `YeEt`.

A function definition in **yeet** starts with `yeet` and is followed by zero or more identifiers (the function parameters), then another `yeet`, then the function body and finally a closing `yeet`.

Functions are [curried](https://en.wikipedia.org/wiki/Currying) by default.

```yeet
yeet <parameters> yeet <body> yeet
```

To call a function, put its identifier followed by its arguments.

### Examples

A binary function that evaulates to its first argument:

```yeet
yeet yeeet yeeeet yeet yeeet yeet
```

A  binary function that evaulates to its second argument:

```yeet
yeet yeeet yeeeet yeet yeeeet yeet
```

A binary function that calls its first parameter with its second parameter:

```yeet
yeet yeeet yeeeet yeet yeeet yeeeet yeet
```

A unary function that calls its first parameter with the number 2:

```yeet
yeet yeeet yeet yeeet YEet yeet
```

Note again that the whitespace (and other text not forming an uninterrupted yeet) is optional, the following code has the same meaning as the first example:

```yeet
yeetyeeetyeeeetyeetyeeetyeet
```

## Semantics

In **yeet**, everything is a function. There are no variables, no statements, no mutability. Only functions. Every valid **yeet** expression is a function call. In such a function call, one function is applied to its parameters. If the programmer wanted to somehow compute some of the parameters, they must encapsulate their logic into a function without parameters.

A **yeet** program is a function. A valid **yeet** program is a function that takes a parameter as input and evaluates a function as output. Both of these functions must be 'lists' as defined below in **conventions**.

The number literal (starting with a capital `Y`) is syntactic sugar for numbers as defined in **conventions**.

The input and output lists are lists of numbers. The input list will contain the [ascii](https://en.wikipedia.org/wiki/ASCII) representations of the standard input. The output list must also be a list of numbers. Each number in this list is converted into the corresponding ascii character and printed to the standard output.

## Conventions

The conventions explained in the following sections were adopted from the [lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus#Encoding_datatypes).

### Numbers

Numbers in **yeet** are defined like the Church Numerals. As there are no parenthesis in **yeet**, the programmer may use functions without parameters to encapsulate expressions.

```yeet
0 = Yeet = yeet yeeet yeeeet yeet yeeeet yeet
1 = YeEt = yeet yeeet yeeeet yeet yeeet yeeeet yeet
2 = YEet = yeet yeeet yeeeet yeet yeeet yeet yeet yeeet yeeeet yeet yeet
3 = YEEt = yeet yeeet yeeeet yeet yeeet yeet yeet yeeet yeet yeet yeeet yeeeet yeet yeet yeet
...
```

### Lists and Pairs

To define a linked list in yeet, some building blocks are needed:

- TRUE:   `yeet yeeet yeeeet yeet yeeet yeet` (Returns its first argument)
- FALSE:  `yeet yeeet yeeeet yeet yeeeet yeet` (Returns its second argument)
- PAIR:   `yeet yeeet yeeeet yeeeeet yeet yeeeeet yeeeet yeeet yeet`
- FIRST:  `yeet yeeet yeet yeeet TRUE yeet`
- SECOND: `yeet yeeet yeet yeeet FALSE yeet`
- NIL:    `yeet yeeet yeet TRUE yeet`

A pair would now be `PAIR <first> <second>`.

The empty list is `NIL` and a non-empty list is a pair of an element and a smaller list, for example: `PAIR YeEt NIL` (this is the list `[1]`).

Note that, in this paragraph, I substituted the actual functions with names to make this section shorter, which is not possible in real yeet. An alternative can be found in [Program structure](#program-structure).

### Program structure

A yeet program must be a function returning a list of numbers.

The programmer may feel the need to bind often used functions to descriptive names. Such superflous features obviously do not exist in yeet. Instead, the programmer can write a function receiving the function to bind as a parameter:

```yeet
Suppose we wanted to write a yeet program that returns its input
save for the first two characters.
We would need to use the SECOND function from above two times.
Here is an example of how this could be accomplished.

yeet yeeet < this is the input yeet
  yeet yeeeet < this will be SECOND yeet
    yeeeet yeeeet yeeet
  yeet We now put the definition of SECOND: 
    yeet yeeeet yeet yeeeet yeet yeeeeet yeeeeeet yeet yeeeeeet yeet yeet
yeet
```

