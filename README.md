# ProLog2425
SIGHT

### Regras do jogo:

**1. Objetivo do Jogo**

  - Forçar o oponente a ficar sem jogadas válidas.

**2. Condições Iniciais**

 - Tabuleiro vazio (5x5).
 - Jogador com peças brancas começa.

**3. Definições Importantes**

 - Singleton: Peça única no tabuleiro.
 - Stack: Pilha de duas ou mais peças da mesma cor.
 - Linha de Visão (Sight): Conexão entre peças amigáveis na mesma linha, coluna ou diagonal, sem obstrução.

**4. Tipos de Ações**

 - Colocação de Peças (Placement):
    a. Permitida apenas quando não há stacks disponíveis para o jogador.
    b. Escolher qualquer posição vazia (empty).
    c. Atualizar peças amigáveis na linha de visão com +1 peça.

 - Movimento de Stacks (Stack Movement):
    a. Permitido quando há stacks disponíveis para o jogador.
    b. Escolher o stack mais alto.
    c. Mover o topo do stack para uma posição adjacente e vazia.
    d. Atualizar peças amigáveis na linha de visão com +1 peça.

**5. Condições de Finalização**

 - Um jogador perde se:
    a. Não pode colocar peças (não há espaços vazios).
    b. Não pode mover stacks (não há espaços vazios adjacentes para stacks disponíveis).
