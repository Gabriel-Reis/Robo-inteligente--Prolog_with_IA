# Robo inteligente (Prolog para inteligência artificial)
### O objetivo deste progrma é implementar um robô que consiga andar pelo ambiente, pegar blocos e soltá-los, manualmente ou com a possibilidade de se utilizar buscas autómaticas, sendo que cada ação possui um custo associado.

# Objetivo:
  Levar os blocos um de cada vez até a sala de depósito (Sala 30).

# Posições iniciais:
  * Robo - Sala 2
  * Bloco1 - sala 3
  * Bloco2 - Sala 14
  * Bloco3 - Sala 23

## Buscas:
1. aleatória
2. largura
3. profundidade
4. menor custo
5. melhor estimativa
6. otima (ou A*)

## Custos:
* Verticalmente sem utilizar escadas = 1
* Horizontalmente = 2
* Verticalmente utilizando escadas = 3

## Blocos:
1. Vermelho
2. Amarelo
3. Verde

## Mapa
  O mapa está disposto em uma "matriz" 4x8, onde as salas são numeradas e o robo não pode atravesar paredes.
  exemplo: as passagens de 1 -> 5 não é permitida, assim como 6 - > 10 também não é permitida, a seguir está a disposição das salas
  
    | 01/4A | 02/4C | 03/4E | 04/4G |
    | 05/4B | 06/4D | 07/4F | 08/4H |
    | 09/3A | 10/3C | 11/3E | 12/3G |
    | 13/3B | 14/3D | 15/3F | 16/3H |
    | 17/2A | 18/2C | 19/2E | 20/2G |
    | 21/2B | 22/2D | 23/2F | 24/2H |
    | 25/1A | 26/1C | 27/1E | 28/1G |
    | 29/1B | 30/1D | 31/1F | 32/1H |

## Instruções
Para iniciar o programa, tenha um compilador de Prolog instalado execute o arquivo agente.pl e em seguida execute o comando que desejar.

## Comandos:
* agente() - Inicia o robo para modo manual.
  * ande(X) - Anda para a sala X (utilizar o número da sala).
  * pegue(X) - Ao estar na sala com o bloco, pega o bloco corespondeda cor correspondente.
  * solte(X) - Solta o bloco da cor X na sala na qual o robo está.
* busca(X) - Realiza uma das buscas programas, sendo X o número correspondente a busca.

## Alterar posições iniciais ou objetivo
  #### Altere nas seguintes linhas, a posição inicial do objeto desejado (Trocar em todas suas ocorrências).
    sala(2,X4,Y4),                   %AGENTE (substituir o número 2)
    sala(3,X1,Y1),                   %BLOCO1 (substituir o número 3)
    sala(14,X2,Y2),                  %BLOCO2 (substituir o número 14)
    sala(23,X3,Y3),                  %BLOCO3 (substituir o número 23)
    assert(pos(agente,2)),           %AGENTE (substituir o número 2)
    inicial([2,3,14,23,30,0,0,0,0]). %,AGENTE(2),BLOCO1(3),BLOCO2(14),BLOCO3(23),DEPOSITO(30)
 
 #### Altere nas seguinte linha, a posição final do objeto desejado (Trocar em todas suas ocorrências).
    meta([_,3,14,23,30,0,1,1,1]).    %_,BLOCO1,BLOCO2,BLOCO3,DEPOSITO,0,1,1,1
## Sistema de testes:
  * Swi-Prolog
  * Intel I5
  * 8GB DDr3
  * Windows 10 64x
  * Nvidia 930m
###### Um hardware básico tem capacidade de executar este software.
