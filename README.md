# PFL - TP2

## **Authors**
- [João Macedo Lima](up202108891@fe.up.pt) - up202108891 (50%)
- [Tomás Eiras Silva Martins](up202108776@fe.up.pt) - up202108776 (50%)
  
<div style="text-align: justify;">
<p>

## **Problem Description**
Este projeto consistiu em duas partes onde tínhamos que implementar uma *low level machine* e um compilador para *imperative language*, respetivamente.
Na primeira parte, o objetivo foi implementar uma máquina que recebesse como input uma lista de instruções e atualizava a sua stack e o seu estado consoante as diferetes instruções que lhe eram fornecidas. Tivemos de implementar as seguintes instruções:
- add, mult, sub (operações airtméticas)
- eq, le, true, false (operações boleanas)
- push-n, fetch-x, store-x, noop (operações da stack)
- branch(c1,c2), loop(c1,c2) (*control flow*)

Relativamente à parte 2, o principal objetivo, como já foi anteriormente referido, foi desenvolver um compilador para *imperative language*. Este aceitava expressões aritméticas e booleanas, *assignments*, sequências, *if-then-else* e ciclos *while*.

## **Parte 1**
Na parte 1, a nossa abordagem consistiu em inicialmente definir os tipos *Inst*, *Code*, *State* e *Stack*. Depois começamos a desenvolver a parte principal do projeto onde foram implementadas as diversas instruções (juntamente com as instruções de *Control Flow*). Por fim, desenvolvemos a função `run` que é a responsável em executar o código.
Decisões tomadas:
- Para representar a Stack, escolhemos uma lista de valores `Inst`;
- O estado foi representado por uma lista de tuplos onde cada tuplo continha o nome da variável e o respetivo valor;
- Usamos também *Pattern matching* na função `run` de forma a ser possível processar todas as instruções.

## **Parte 2**
Na parte 2 estendemos a linguagem de forma a que esta incluísse expressões aritméticas e booleanas, declarações e programas. Por isso, introduzimos os tipos *Aexp*, *Bexp* e *Stm* para representar as expressões aritméticas, as expressões booleanas e as declarações (*Statements*), respetivamente. Posteriormente, a função `compile` foi desenvolvida para ser possível gerar código para essas construções.
Decisões tomadas:
- A função compile foi projetada para traduzir declarações de alto nível em código de baixo nível.
- O uso de tokens e análise foram implementadas para facilitar a tradução do código-fonte para a representação interna.

## **Exemplo**
Tenhamos como exemplo este input : "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);"
1) As variáveis `i` e `fact` são inicializadas, criamos uma pilha vazia e um estado de máquina inicial, sendo neste caso uma lista vazia
2) O nosso compilador vai traduzir o programa imperativo para uma sequência de instruções:
    [Push 10,Store "i",Push 1,Store "fact",Loop [Fetch "i",Push 1,Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]]
3) A função `run` vai ser chamada com a sequência de instruções, uma pilha vazia e o estado inicial e, para além disso, as instruções vão ser executadas passo a passo, manipulando a pilha e o estado conforme necessário.
4) A máquina entra no loop e a condição vai ser avaliada. Caso esta seja verdadeira, o corpo do loop vai ser executado.
5) No fim, quando a variável `i` tiver o valor 1, o ciclo para e a variável `fact` vai conter o resultado final que vai corresponder ao fatorial de 10 tal como se pode ver na imagem abaixo:

![Alt text](image.png)

## **conclusões**
Com este trabalho prático, conseguimos consolidar os nossos conhecimentos sobre a linguagem de programação *Haskell* e obter uma melhor compreensão sobre novos conceitos desta linguagem. Quanto ao projeto em si, achamos que devido a alguns constrangimentos temporais gerados por outros trabalhos, não conseguimos desenvolver o projeto da melhor forma possível. No entanto, conseguimos atingir a maior parte dos objetivos, sendo que apesar de passar na maior parte dos testes, naqueles que existiam statments após um else, nos quais teriam de ser executados, independetemente da condição do if, não conseguimos obter o resultado esperado. Verificamos que esses statments eram adicionados à branch do else em vez de estarem fora do if mas mesmo assim não conseguimos resolver o problema. Apesar disso, achamos que o projeto se encontra solido e bem estruturado.
</div>
