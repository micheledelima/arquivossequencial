      $set sourceformat"free"

      *>Divisão de identificação do programa
       identification division.
       program-id. "lista11exercicio1v2".
       author. "Michele de Lima".
       installation. "PC".
       date-written. 16/07/2020.
       date-compiled. 16/07/2020.



      *>Divisão para configuração do ambiente
       environment division.
       configuration section.
           special-names. decimal-point is comma.

      *>-----Declaração dos recursos externos
       input-output section.
       file-control.
      *>   Declaração do arquivo
           select arqTemperaturas assign to "arqTemperaturas.dat"       *>assosiando arquivo lógico (nome dado ao arquivo dentro do pmg vom o arquivo fisico)
           organization is line sequential                              *>forma de organização dos dados
           access mode is sequential                                    *>forma de acesso aos dados
           lock mode is automatic                                       *>tratamento de dead lock - evita perda de dados em ambiemtes multi-usuários
           file status is ws-fs-arqTemperaturas.                        *>file status (o status da ultima operação)


       i-o-control.

      *>Declaração de variáveis
       data division.

      *>----Variaveis de arquivos
       file section.
       fd arqTemperaturas.
       01 fd-temperaturas.
          05 fd-temp                               pic 9(02).
          05 fd-dia                                pic 9(02).


      *>----Variaveis de trabalho
       working-storage section.
       77  ws-fs-arqTemperaturas                   pic 9(02).


       01 ws-temperaturas occurs 30.
          05 ws-temp                               pic 9(02).
          05 ws-dia                                pic 9(02).

       01 indices.
           05 ws-ind-temp                          pic 9(04).
           05 ws-ind-dia                           pic 9(04).

       01 ws-uso-comum.
          05 ws-sair                               pic x(01).
          05 ws-msn                                pic x(50).
          05 ws-msn-erro.
             10 ws-msn-erro-ofsset                 pic 9(04).
             10 filler                             pic x(01) value "-".
             10 ws-msn-erro-cod                    pic 9(02).
             10 filler                             pic x(01) value space.
             10 ws-msn-erro-text                   pic x(42).


       77 ws-media-temp                            pic 9(04).
       77 ws-temp-total                            pic 9(04).
       77 ws-dia-informado                         pic 9(02).

      *>----Variaveis para comunicação entre programas
       linkage section.


      *>----Declaração de tela
       screen section.

      *>Declaração do corpo do programa
       procedure division.


           perform inicializa.
           perform processamento.
           perform finaliza.

      *>------------------------------------------------------------------------
      *>  Procedimentos de inicialização
      *>------------------------------------------------------------------------
       inicializa section.

      *>    inicialização da tabela de estados

           open input arqTemperaturas.
           if ws-fs-arqTemperaturas <> 0 then
               move 1                                     to ws-msn-erro-ofsset
               move ws-fs-arqTemperaturas                 to ws-msn-erro-cod
               move "Erro ao abrir arq. arqTemperaturas " to ws-msn-erro-text
               perform finaliza-anormal
           end-if

           perform varying ws-ind-temp from 1 by 1 until
                                               ws-fs-arqTemperaturas = 10
                                               or ws-ind-temp > 30
               *> lê o arquivo
               read arqTemperaturas into ws-temperaturas(ws-ind-temp)
               if  ws-fs-arqTemperaturas <> 0
               and ws-fs-arqTemperaturas <> 10 then
                   move 2                                     to ws-msn-erro-ofsset
                   move ws-fs-arqTemperaturas                 to ws-msn-erro-cod
                   move "Erro ao ler arq. arqTemperaturas "   to ws-msn-erro-text
                   perform finaliza-anormal
               end-if


           end-perform
          *>finaliza arquivo
           close arqTemperaturas.
           if ws-fs-arqTemperaturas <> 0 then
               move 3                                 to ws-msn-erro-ofsset
               move ws-fs-arqTemperaturas             to ws-msn-erro-cod
               move "Erro ao fechar arq. arqEstados " to ws-msn-erro-text
               perform finaliza-anormal
           end-if


           .
       inicializa-exit.
           exit.


      *>------------------------------------------------------------------------
      *>  Processamento principal
      *>------------------------------------------------------------------------
       processamento section.

      *>   chamando rotina de calculo da média de temp.
           perform calc-media-temp


      *>    menu do sistema
           perform until ws-sair = "S"
                      or ws-sair = "s"
               display erase



               display "Dia a ser testado: "
               accept ws-dia-informado

      *>       percorre a tabela para achar o dia digitado
               perform varying ws-ind-temp from 1 by 1 until ws-ind-temp > 30
      *>           se a linha da tabela conter o dia informado então
                   if ws-dia(ws-ind-temp) = ws-dia-informado then
      *>              guarda a posição da linha está no ws-ind-dia
                      move ws-ind-temp to ws-ind-dia

               end-perform

               if  ws-dia(ws-ind-dia) >= 1
               and ws-dia(ws-ind-dia) <= 30 then
      *>           se a temperatura da posição x na tabela for maior que a media então
                   if ws-temp(ws-ind-dia) > ws-media-temp then

                       display "A temperatura do dia " ws-dia(ws-ind-dia) " esta acima da media"
                   else
                   if ws-temp(ws-ind-dia) < ws-media-temp then

                       display "A temperatura do dia " ws-dia(ws-ind-dia) " esta abaixo da media"
                   else
                       display "A temperatura esta na media"
                   end-if
                   end-if
               else
                   display "Dia fora do intervalo valido (1 - 30)"
               end-if

               display "'T'estar outra temperatura"
               display "'S'air"
               accept ws-sair
           end-perform

           .
       processamento-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Calculo da média de temperatura
      *>------------------------------------------------------------------------
       calc-media-temp section.

           move 0 to ws-temp-total
           perform varying ws-ind-temp from 1 by 1 until ws-ind-temp > 30
               compute ws-temp-total = ws-temp-total + ws-temp(ws-ind-temp)
           end-perform

           compute ws-media-temp = ws-temp-total/30

           .
       calc-media-temp-exit.
           exit.

       finaliza-anormal section.
           display erase
           display ws-msn-erro.
           Stop run
           .
       finaliza-anormal-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Finalização
      *>------------------------------------------------------------------------
       finaliza section.
           Stop run
           .
       finaliza-exit.
           exit.













