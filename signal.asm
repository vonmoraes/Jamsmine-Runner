; Lucas de Souza Moraes - 538464
; Breno Aroeira Cosenza - 576463
;***********************************************************************************************
; ALTERACOES: CRIADA ROTINA DE TESTE
;              ADICIONADO ATRIBUICAO DE VALOR 0 AO FLAG
;              ADICIONADO CLRF AO FLAG ANTES DE CADA LOOP DE SINAL
;              ADICIOANADO VERIFICACAO DE FLAG EM CADA SINAL DO CICLO PRINCIPAL
;***********************************************************************************************
    LIST   P=PIC16F628A
#INCLUDE <P16F628A.INC>         ;P16F628A
    __CONFIG _INTRC_OSC_NOCLKOUT & _WDT_OFF & _PWRTE_ON & _BODEN_OFF & _LVP_OFF & _CP_OFF & _MCLRE_OFF & _DATA_CP_OFF
    ;ou __CONFIG H'3F10'
  #DEFINE BANK0 BCF STATUS,RP0                       ;SET BANK 0
  #DEFINE BANK1 BSF STATUS,RP0                       ;SET BANK 1
  #DEFINE BUTTON PORTA,D'2'                          ;BOTAO EM RA2 (0 H 1 L)
    CBLOCK    0x20        ;ENDERECO INICIAL DA MEMORIA
  D1              ;DELAY1
  D2              ;DELAY2
  D3              ;DELAY3
  D4              ;DELAY4
  AUX             ;AUXILIAR
  FLAG              ;VALOR AUXILIAR DE TROCA DE CICLO
    ENDC                        ;FIM DO BLOCO DE MEMORIA
    ORG    0x00                    ;ENDERECO INICIAL DE PROCESSAMENTO
    GOTO    INICIO
;***********************************************************************************************
INICIO:
        CLRF PORTA                 ;LIMPA A PORTA
        CLRF PORTB                 ;LIMPA A PORTB
        CLRF TRISA
        CLRF TRISB
        BANK1
            MOVLW    B'00000100'
            MOVWF TRISA                 ;DEFINE RA2 ENTRADA DE PORTA E O RESTO SAIDAS
            MOVLW    B'00000000'
            MOVWF TRISB                 ;DEFINE SAIDAS DE PORTB
            MOVLW B'00000000'
            MOVWF INTCON       ;DESLIGAR INTERRUPCOES
        BANK0
            MOVLW B'00000111'
            MOVWF CMCON        ;COMPARADOR ANALOGICO
;***********************************************************************************************
;TESTE DOS LEDS E DO BOTAO
;GOTO TESTE7SEGUNDOS
;COMENTAR TESTE CASO NAO ESTEJA EM TESTE
;***********************************************************************************************
LOOP
    MOVLW D'7'
    MOVWF AUX                ;NOVO VALOR AUX
    CLRF  FLAG                ;LIMPA FLAG
    MOVLW B'00001100'        ;S1 VERDE,S2 VERMELHO
    MOVWF PORTB
FOR1
    CALL DELAY
    BTFSC FLAG,0
    GOTO CICLO_1            ;IR PARA O CICLO1
    DECFSZ AUX,1
    GOTO FOR1                ;FIM FOR1
    MOVLW    D'3'
    MOVWF AUX                ;NOVO VALOR AO AUX
    CLRF  FLAG                ;LIMPA FLAG
    MOVLW    B'00001010'        ;S1 AMARELO,S2 VERMELHO
    MOVWF    PORTB
FOR2
    CALL DELAY
    BTFSC FLAG,0
    GOTO CICLO_2			;IR PARA O CICLO2
    DECFSZ AUX,F
    GOTO FOR2				;FIM FOR2
    MOVLW D'2'
    MOVWF AUX                ;NOVO VALOR AO AUX
    CLRF  FLAG                ;LIMPA FLAG
    MOVLW    B'00100001'        ;S1 VERMELHO,S2 VERDE
    MOVWF    PORTB
FOR3
    CALL DELAY
    BTFSC FLAG,0
    GOTO CICLO_3				;IR PARA O CICLO3
    DECFSZ AUX,F
    GOTO FOR3				;FIM FOR3
    MOVLW D'2'
    MOVWF AUX                ;NOVO VALOR AO AUX
    CLRF  FLAG                ;LIMPA FLAG
    CALL DELAY
    BTFSC FLAG,0
    GOTO CICLO_3				;IR PARA O CICLO3
    MOVLW    B'00010001'        ;S1 VERMELHO, S2 AMARELO
    MOVWF    PORTB
FOR4
    CALL DELAY
    BTFSC FLAG,0
    GOTO CICLO_3			;IR PARA O CICLO3
    DECFSZ AUX,F
    GOTO FOR4				;FIM FOR4
    GOTO LOOP
;***********************************************************************************************
;SUBROTINAS
;***********************************************************************************************
TESTE
    CLRF FLAG       ;LIMPAR FLAG, FLAG = 0
    CALL DELAY      ;CHAMA DELAY DE 1 SEGUNDO
    BTFSC FLAG,0    ;COMPARA SE FLAG !=0
    CALL ALLHIGH    ;SE FOR != ACENDE TODOS OS LEDS
	CALL ALLLOW		;SE FOR = 0 APAGA OS LEDS
ALLHIGH
    MOVLW B'11111111' ;ACENDE TODOS OS LEDS (RB - 0,1,2,3,4,5,X,X)
    MOVWF PORTB
	CALL DELAY
    RETURN
ALLLOW
    MOVLW B'00000000' ;APAGATODOS OS LEDS (RB - 0,1,2,3,4,5,X,X)
    MOVWF PORTB
    GOTO TESTE
TESTE7SEGUNDOS
    MOVLW B'00000000' ;APAGA TODOS OS LEDS (RB - 0,1,2,3,4,5,X,X)
    MOVWF PORTB
	MOVLW D'7'
	MOVWF AUX
FORTESTE
	CALL DELAY
	BTFSC FLAG,0
	GOTO ALLHIGH
	DECFSZ AUX,F
	GOTO FORTESTE
	GOTO TESTE7SEGUNDOS ; LOOP
;***********************************************************************************************
;FIM ROTINA DE TESTE E INICIO DAS ROTINAS DOS CICLOS
;***********************************************************************************************
CICLO_1
	MOVLW    D'3'
    MOVWF    AUX
    MOVLW    B'00001010'        ;S1 AMARELO,S2 VERMELHO
    MOVWF    PORTB
FORC1_2
    CALL DELAY
    DECFSZ AUX,F
    GOTO FORC1_2
    MOVLW    D'3'
    MOVWF    AUX
	MOVLW    B'00100001'        ;S1 VERMELHO,S2 VERDE
    MOVWF    PORTB
FORC1_3
    CALL DELAY
    DECFSZ AUX,F
    GOTO FORC1_3
    CALL DELAY
    MOVLW    D'3'
    MOVWF    AUX
    MOVLW    B'00010001'        ;S1 VERMELHO, S2 AMARELO
    MOVWF    PORTB
FORC1_4
    CALL DELAY
    DECFSZ AUX,F
    GOTO FORC1_4
    MOVLW    D'4'
    MOVWF    AUX
    MOVLW    B'00001100'        ;S1 VERDE,S2 VERMELHO
    MOVWF    PORTB
FORC1_1
    CALL DELAY
    DECFSZ AUX,F
    GOTO FORC1_1
    GOTO LOOP
CICLO_2
    MOVLW    D'3'
    MOVWF    AUX
    MOVLW    B'00100001'        ;S1 VERMELHO,S2 VERDE
    MOVWF    PORTB
FORC2_3
    CALL DELAY
    DECFSZ AUX,F
    GOTO FORC2_3
    MOVLW    D'3'
    MOVWF    AUX
    CALL DELAY                         ;CORRECAO DO ATRASO
    MOVLW    B'00010001'        ;S1 VERMELHO, S2 AMARELO
    MOVWF    PORTB
FORC2_4
    CALL DELAY
    DECFSZ AUX,F
    GOTO FORC2_4
    MOVLW    D'4'
    MOVWF    AUX
    MOVLW    B'00001100'        ;S1 VERDE,S2 VERMELHO
    MOVWF    PORTB
FORC2_1
    CALL DELAY
    DECFSZ AUX,F
    GOTO FORC2_1
    GOTO LOOP
CICLO_3
    MOVLW    D'3'
    MOVWF    AUX
    MOVLW    B'00100001'        ;S1 VERMELHO,S2 VERDE
    MOVWF    PORTB
FORC3_3
    CALL DELAY
    DECFSZ AUX,F
    GOTO FORC3_3
    CALL DELAY
    MOVLW    D'3'
    MOVWF    AUX
    MOVLW    B'00010001'        ;S1 VERMELHO, S2 AMARELO
    MOVWF    PORTB
FORC3_4
    CALL DELAY
    DECFSZ AUX,F
    GOTO FORC3_4
    MOVLW    D'4'
    MOVWF    AUX
    MOVLW    B'00001100'        ;S1 VERDE,S2 VERMELHO
    MOVWF    PORTB
FORC3_1
    CALL DELAY
    DECFSZ AUX,F
    GOTO FORC3_1
    GOTO LOOP
ON                              ;ALTERA VALOR DA FLAG CASO O BOTAO SEJA APERTADO
    MOVLW D'1'
    MOVWF FLAG
    RETURN
;***********************************************************************************************
; DELAY: REALIZA DELAY COM 4 LOOPS DIFERENTES, ONDE O PRIMEIRO LOOP
;***********************************************************************************************
DELAY
    MOVLW D'25'
    MOVWF D1
LOOP1
    MOVLW D'1';D'5';D'17'
    MOVWF D2
LOOP2
    MOVLW D'55';D'39';D'33'
    MOVWF D3
LOOP3
    MOVLW D'241';D'67';D'1'
    MOVWF D4
LOOP4
    DECFSZ D4,F
    GOTO LOOP4
    DECFSZ D3,F
    GOTO LOOP3
    DECFSZ D2,F
    GOTO LOOP2
    BTFSS BUTTON
    CALL ON
    DECFSZ D1,F
    GOTO LOOP1
    RETURN
    END
