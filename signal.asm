; Lucas de Souza Moraes - 538464
; Luana Duarte Santana Farias -
LIST   P=PIC16F628A
#INCLUDE <P16F628A.INC> 		;P16F628A
	__CONFIG _INTRC_OSC_NOCLKOUT & _WDT_OFF & _PWRTE_ON & _BODEN_OFF & _LVP_OFF & _CP_OFF & _MCLRE_OFF & _DATA_CP_OFF
	;ou __CONFIG H'3F10'
  #DEFINE BANK0 BFC STATUS,RP0  ;SET BANK 0
  #DEFINE BANK1 BSF STATUS,RP0  ;SET BANK 1
  #DEFINE BUTTON PORTA,2         ;0 H 1 L
	CBLOCK	0x20		;ENDERECO INICIAL DA MEMORIA
  D1              ;DELAY1
  D2              ;DELAY2
  D3              ;DELAY3
  AUX             ;AUXILIAR
	ENDC						;FIM DO BLOCO DE MEMORIA
	ORG	0x00				;ENDERECO INICIAL DE PROCESSAMENTO
	GOTO	INICIO
;*******************************************************************
INICIO:
	    CLRF	PORTA				 ;LIMPA A PORTA
	    CLRF	PORTB				 ;LIMPA A PORTB
	    BANK1
			MOVLW	B'00000100'
			MOVLW TRISA				 ;DEFINE RA2 ENTRADA DE PORTA E O RESTO SAIDAS
			MOVLW	B'00000000'
	    MOVLW TRISB				 ;DEFINE SAIDAS DE PORTB
	    BANK0
;*******************************************************************
LOOP:
		CALL TESTE
		GOTO LOOP
;LOOP:
MAIN
      MOVLW	D'7'
      MOVWF	AUX
      FOR1: MOVLW	B'00001100'		;S1 VERDE,S2 VERMELHO
            MOVWF	PORTB
            CALL DELAY
						BTFSC BUTTON
						CALL CICLO_1
            DECFSZ AUX,F
            GOTO FOR1
      MOVLW	D'3'
      MOVWF	AUX
      FOR2: MOVLW	B'00001010'		;S1 AMARELO,S2 VERMELHO
            MOVWF	PORTB
            CALL DELAY
						BTFSC BUTTON
						CALL CICLO_2
            DECFSZ AUX,F
            GOTO FOR2
      MOVLW	D'2'
      MOVWF	AUX
      FOR3: MOVLW	B'00100001'		;S1 VERMELHO,S2 VERDE
            MOVWF	PORTB
            CALL DELAY
						BTFSC BUTTON
						CALL CICLO_3
            DECFSZ AUX,F
            GOTO FOR3
      MOVLW	D'2'
      MOVWF	AUX
      FOR4: MOVLW	B'00010001'		;S1 VERMELHO, S2 AMARELO
            MOVWF	PORTB
            CALL DELAY
						BTFSC BUTTON
						CALL CICLO_3
            DECFSZ AUX,F
            GOTO FOR4
		 RETURN
;GOTO LOOP
;*******************************************************************
;SUBROTINAS
;*******************************************************************
CICLO_1: MOVLW	D'3'
         MOVWF	AUX
         FOR2: MOVLW	B'00001010'		;S1 AMARELO,S2 VERMELHO
               MOVWF	PORTB
               CALL DELAY
               DECFSZ AUX,F
               GOTO FOR2
         MOVLW	D'3'
         MOVWF	AUX
         FOR3: MOVLW	B'00100001'		;S1 VERMELHO,S2 VERDE
               MOVWF	PORTB
               CALL DELAY
               DECFSZ AUX,F
               GOTO FOR3
         CALL DELAY
         MOVLW	D'3'
         MOVWF	AUX
         FOR4: MOVLW	B'00010001'		;S1 VERMELHO, S2 AMARELO
               MOVWF	PORTB
               CALL DELAY
               DECFSZ AUX,F
               GOTO FOR4
         MOVLW	D'4'
         MOVWF	AUX
         FOR1: MOVLW	B'00001100'		;S1 VERDE,S2 VERMELHO
               MOVWF	PORTB
               CALL DELAY
               DECFSZ AUX,F
               GOTO FOR1
               RETURN
CICLO_2: MOVLW	D'3'
         MOVWF	AUX
         FOR3: MOVLW	B'00100001'		;S1 VERMELHO,S2 VERDE
               MOVWF	PORTB
               CALL DELAY
               DECFSZ AUX,F
               GOTO FOR3
         MOVLW	D'3'
         MOVWF	AUX
         NOP
         FOR4: MOVLW	B'00010001'		;S1 VERMELHO, S2 AMARELO
               MOVWF	PORTB
               CALL DELAY
               DECFSZ AUX,F
               GOTO FOR4
        MOVLW	D'4'
        MOVWF	AUX
        FOR1: MOVLW	B'00001100'		;S1 VERDE,S2 VERMELHO
              MOVWF	PORTB
              CALL DELAY
              DECFSZ AUX,F
              GOTO FOR1
              RETURN
CICLO_3: MOVLW	D'3'
         MOVWF	AUX
         FOR3: MOVLW	B'00100001'		;S1 VERMELHO,S2 VERDE
               MOVWF	PORTB
               CALL DELAY
               DECFSZ AUX,F
               GOTO FOR3
         CALL DELAY
         MOVLW	D'3'
         MOVWF	AUX
         FOR4: MOVLW	B'00010001'		;S1 VERMELHO, S2 AMARELO
               MOVWF	PORTB
               CALL DELAY
               DECFSZ AUX,F
               GOTO FOR4
         MOVLW	D'4'
         MOVWF	AUX
         FOR1: MOVLW	B'00001100'		;S1 VERDE,S2 VERMELHO
               MOVWF	PORTB
               CALL DELAY
               DECFSZ AUX,F
               GOTO FOR1
        RETURN
;*******************************************************************
; DELAY: REALIZA DELAY DE ACORDO COM CYCLES (RECALCULAR)
; ACRECENTANDO VALORES A REGISTRADOR
; E DIMUINDO ELES ATE 0
; NOTE: MOVLW+1CYCLE
;				MOVWF+1CYCLE
;				DECFSZ+2CYCLE (AO PULAR, SEMPRE 1 CYCLE)
;				BDSFC +2 CYCLES
;*******************************************************************
DELAY:     ;999990 CYCLES
            MOVLW	0x07 ;D'7'
  	        MOVWF	D1
					  MOVLW 0x2F ;D'47'
  	        MOVWF	D2
  	        MOVLW	0x03 ;D'3'
  	        MOVWF	D3
  DELAY_0:  DECFSZ	D1, f
          	GOTO	$+2
          	DECFSZ	D2, f
          	GOTO	$+2
          	DECFSZ	D3, f
          	GOTO	Delay_0
            ;6 CYCLES
          	GOTO	$+1
          	GOTO	$+1
          	GOTO	$+1
            ;4 CYCLES
  	        RETURN
TESTE: MOVLW	D'7'
		   MOVWF	AUX
		   FOR1: MOVLW	B'10000100'		;S1 VERDE,S2 VERMELHO
						MOVWF	PORTB
		 				CALL DELAY
						BTFSC BUTTON
						CALL CICLO_1
						DECFSZ AUX,F
						GOTO FOR1

END
