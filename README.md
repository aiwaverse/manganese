
# Manganese: Interpretador e parser da linguagem L1
- [Manganese, construções e especificação da linguagem](#manganese-construções-e-especificação-da-linguagem)
  * [O que é?](#o-que-é)
  * [Tipos](#tipos)
  * [Construções](#construções)
  * [Açúcares Sintáticos](#açúcares-sintáticos)
- [Rodando o projeto](#rodando-o-projeto)
  * [Build](#build)
  * [Usando o parser](#usando-o-parser)
  * [Usando o typeinfer](#usando-o-typeinfer)
  * [Testando o projeto](#testando-o-projeto)

<small><i><a href='http://ecotrust-canada.github.io/markdown-toc/'>Table of contents generated with markdown-toc</a></i></small>


# Manganese, construções e especificação da linguagem
## O que é?
Manganese é um interpretador com checagem de tipos para a linguagem L1, adicionamente contém um parser, capaz de transformar uma string da linguagem de alto nível na representação interna (que então é checada e interpretada).
## Tipos
| Tipo  | Representação Interna  | Representação em alto nível  |
| ------------ | ------------ | ------------ |
| Inteiros  | Int  | int  |
| Booleanos  | Bool  | bool  |
| Funções  | Func(T1, T2)  | T1 -> T2  |
| Pares  | Pair(T1, T2)  | (T1, T2)  |
| Listas |  List(T) | [T]  |
| Maybe/Option | Maybe(T) | maybe T

## Construções
As possíveis construções da linguagem, com exemplos em alto nível:
| Tipo  | Exemplo  |
| ------------ | ------------ |
| Números  | ... -1, 0, 1, ..  |
| Booleanos |  true, false |
| Condicional  | if x = 0 then false else true  |
| Funções anônimas  | fn x : int => x + 1  |
| Aplicação  |  (fn x : int => x + 1) 2 |
| Aplicação de operadores[^1] | 2 > 3 |
| Introdução de escopos | let x : int = 1 in x + 1 |
| Introdução de escopos recursivos[^2] | let rec fat : int -> int = fn x : int => if (x = 0) then 1 else x * fat (x - 1) in fat 5
| Variáveis[^3] | x |
| Pares ordenados | (1, x)
| First | fst (1, x) |
| Second | snd (1, x) |
| Nil (lista vazia) | nil : int[^4] |
| Cons | 1 :: 2 :: (nil : int) |
| Head | hd (1 :: nil : int) |
| Tail | tl (1 :: nil : int) |
| Pattern matching em listas | match x with nil => false | x :: xs => x = 0 |
| Just | just 3 |
| Nothing | nothing : int |
| Pattern matching em maybe | match x with nothing => false | just x => x = 0 |
## Açúcares Sintáticos
Atualmente, o parser possuí trếs açúcares sintáticos para facilitar a programação: funções de múltiplos argumentos, literais de listas e introdução de escopos recursivos simplifcado, conforme exemplos:
| Açúcar Sintático | Exemplo |
|--|--|
| Funções de múltiplos argumentos[^5] [^6] [^7]| fn (x : int) (y : int) => x + y |
| Literais de listas | [1, 2, 3] : [int] |
| Escopos recursivos simplificados | let rec fat (x : int) : int = if (x = 0) then 1 else x * fat (x - 1) in fat 5 |

## Precedência de Operadores
A precedência dos operadores da linguagem a princípio segue a seguinte ordem: 

 1. Construção de listas
 2. Aplicação de funções
 3. First, Second, Tail e Head.
 4. Operadores aritiméticos * e div
 5. Operadores aritiméticos + e -
 6. Operadores booleanos

Os operadores e funções são todos associativos a direita, com exceção do operador ::, para tornar mais fácil a criação de listas. Pode ser necessário o uso de parênteses nas expressões para garantir a ordem desejada.

# Rodando o projeto
## Build
Para buildar o projeto, usamos a ferramenta [dune](https://github.com/ocaml/dune) e o gerenciador de pacotes [opam](https://opam.ocaml.org/) para instalação da biblioteca de testes:

    opam install ounit2
    dune build
  Atualmente o projeto não tem nada para uma main, então todas as interações são feitas através do [utop](https://opam.ocaml.org/blog/about-utop/).
  

    dune utop

## Usando o parser
O parser está localizado no namespace `Interp`, em espcial, a função que, dada uma string, retorna uma expressão da linguagem, é `Interp.Main.parse`.
```
utop # Interp.Main.parse "1 + 2";;
- : Manganese.Expr.AppOp (Manganese.Expr.Add, Manganese.Expr.Number 1, Manganese.Expr.Number 2)
```
## Usando o typeinfer
WIP
## Testando o projeto
Após instalar a biblioteca de testes [ounit2](https://opam.ocaml.org/packages/ounit2/), basta usar o seguinte comando:
```
dune runtest
...............................................................................................
Ran: 95 tests in: 0.11 seconds.
OK
```
[^1]:  Os operadores da linguagem são: >, <, >=, <=, =, &&, ||, +, -, * e div.
[^2]: Por conta de um "quirk" no parser, o tipo do argumento da função interna (x no caso), é ignorado, ele é sempre o mesmo tipo T1 da função declarada no let rec (int no caso), mas ainda é necessário escrever um tipo, o parser só ignora qual exatamente é. Recomenda-se o uso da versão com açucar sintático conforme [Açúcares Sintáticos](#açúcares-sintáticos).
[^3]: Identificadores válidos da linguagem sempre começam com uma letra, e podem conter letras, números e underscores (_).
[^4]: Para facilitar a inferência de tipos, nil e nothing são tipados, e foi optado por usar o tipo interno, ou seja, em uma expressão do tipo [int], o nil é tipado como int, a mesma lógica vale pare o nothing. 
[^5]: Nessa versão, cada argumento obrigatóriamente precisa estar envolto em parênteses.
[^6]: Essa versão **não** pode ser usada dentro do let rec sem açúcar sintático.
[^7]: Ao aplicar uma função assim, se faz necessário o uso de parênteses, exemplo: `((fn (x : int) (y : int) => x + y) 2 ) 3)`.
