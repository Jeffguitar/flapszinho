   DIM ICODE(15), E(30), POISS(30), H(40), K1(15), K2(15), SDCRIT(15), SCOMPAC(15)
   DIM NDIVV(15), COESAO(15), FI(15), GAMA(15), X(12), Y(12), K3(15), MAT$(15)
   '
   '********************
   '*  MENU PRINCIPAL  *
   '********************
10 CLS : KEY OFF
   '
   'SCREEN 9, 1, 0, 0
   ' Quadro da Tela
   'PSET (5, 3), 15
   'COLOR 15, 3
   'DRAW "M633,3 M633,345 M5,345 M5,3"
   'PSET (7, 5), 15
   'COLOR 15, 3
   'DRAW "M631,5 M631,343 M7,343 M7,5"
   '
   COLOR 5, 3
   LOCATE 3, 37: PRINT "PAVEDAT"
   'PSET (280, 23), 5
   'COLOR 5, 3
   'DRAW "M350,23 M350,45 M280,45 M280,23"
   'PAINT (282, 25), 7, 5
   'FOR I = 282 TO 348: PAINT (I, 33), 7, 5: NEXT I
   COLOR 11, 3: LOCATE 5, 18
   PRINT "Edicao dos Dados para as Analises pelo FLAPS"
   '
   IF (ICARGA = 0) THEN ICOR = 8 ELSE ICOR = 4
        COLOR ICOR, 3: LOCATE 9, 35: PRINT "Carregar Arquivo"
        IF (ICARGA = 0) THEN ICOR = 15 ELSE ICOR = 4
        COLOR ICOR, 3: LOCATE 9, 31: PRINT "(1)"
   IF (IEDITA = 0) THEN ICOR = 8 ELSE ICOR = 4
        COLOR ICOR, 3: LOCATE 11, 35: PRINT "Editar Dados"
        IF (IEDITA = 0) THEN ICOR = 15 ELSE ICOR = 4
        COLOR ICOR, 3: LOCATE 11, 31: PRINT "(2)"
   IF (ISALVA = 0) THEN ICOR = 8 ELSE ICOR = 4
        COLOR ICOR, 3: LOCATE 13, 35: PRINT "Salvar Arquivo"
        IF (ISALVA = 0) THEN ICOR = 15 ELSE ICOR = 4
        COLOR ICOR, 3: LOCATE 13, 31: PRINT "(3)"
   IF (ILAY = 0) THEN ICOR = 8 ELSE ICOR = 4
        COLOR ICOR, 3: LOCATE 15, 35: PRINT "Dados para o LAYMOD"
        IF (ILAY = 0) THEN ICOR = 15 ELSE ICOR = 4
        COLOR ICOR, 3: LOCATE 15, 31: PRINT "(4)"
   ICOR = 8: COLOR ICOR, 3
        LOCATE 17, 35: PRINT "Finalizar"
        COLOR 15, 3: LOCATE 17, 31: PRINT "(5)"
        '
        COLOR 15, 3: LOCATE 23, 70: PRINT "A"
        COLOR 8, 3: LOCATE 23, 71: PRINT "juda"
   '
   Opcao$ = ""
   WHILE Opcao$ = "": Opcao$ = INKEY$: WEND
   SELECT CASE Opcao$
          CASE "1"
                        ICARGA = 1
                        GOSUB 1000
          CASE "2"
                        IEDITA = 1
                        GOSUB 2000
          CASE "3"
                        ISALVA = 1
                        GOSUB 3000
          CASE "4"
                        IF (ICARGA = 1 OR IEDITA = 1) THEN
                                                           ILAY = 1
                                                           GOSUB 4000
                                                      ELSE
                                                           BEEP
                                                           COLOR 12, 3
                                                           LOCATE 23, 10
                                                           PRINT "Deve-se entrar na Opcao (1) ou (2) antes."
                                                           FOR IV = 1 TO 200000: NEXT IV
                                                           GOTO 10
                        END IF
          CASE "5"
                        END
          CASE "A", "a"
                        GOSUB 500
          CASE ELSE
                    FOR IV = 1 TO 2
                        LOCATE 23, 5
                        COLOR 15, 3
                        PRINT "ERRO: Codigo nao valido"
                        BEEP: COLOR 3, 3: LOCATE 23, 7
                        PRINT "                       "
                    NEXT IV
   END SELECT
   GOTO 10
   '
   '
   '
500 '**************************
    '* AJUDA NA PRIMEIRA TELA *
    '*      (Subrotina)       *
    '**************************
    CLS
    'SCREEN 9, 1, 0, 0
    ' Quadro da Tela
            'PSET (5, 3), 15
            'COLOR 15, 3
            'DRAW "M633,3 M633,345 M5,345 M5,3"
            'PSET (7, 5), 15
            'COLOR 15, 3
            'DRAW "M631,5 M631,343 M7,343 M7,5"
            '
            COLOR 5, 3
            LOCATE 3, 37: PRINT "PAVEDAT"
            'PSET (280, 23), 5
            'COLOR 5, 3
            'DRAW "M350,23 M350,45 M280,45 M280,23"
            'PAINT (282, 25), 7, 5
            'FOR I = 282 TO 348: PAINT (I, 33), 7, 5: NEXT I
            COLOR 11, 3: LOCATE 5, 18
            PRINT "Edicao dos Dados para as Analises pelo FLAPS"
            '
    'COLOR 9, 3
    'LOCATE 7, 7: PRINT "                        SGP Engenharia Ltda"
    COLOR 8, 3
    LOCATE 9, 4: PRINT "    Este programa e' a interface de entrada de dados para o programa FLAPS."
    LOCATE 10, 4: PRINT "Entre na opcao (2) se os dados serao inseridos do inicio ao fim e grave-os"
    LOCATE 11, 4: PRINT "em um arquivo na opcao (3). A opcao (1) deve ser utilizada quando se dese-"
    LOCATE 12, 4: PRINT "jar alterar um arquivo de dados pre'-existente."
    COLOR 14, 3
    LOCATE 23, 5: PRINT "Pressione qualquer tecla para retornar ao Menu Principal"
510 B$ = INKEY$: IF (B$ = "") THEN 510
    RETURN
    '
    '
    '
1000 '******************************
     '* CARREGAR ARQUIVO EXISTENTE *
     '*        (Subrotina)         *
     '******************************
     '
      CLS
      'SCREEN 9, 1, 0, 0
      ' Quadro da Tela
        'PSET (5, 3), 15
        'COLOR 15, 3
        'DRAW "M633,3 M633,345 M5,345 M5,3"
        'PSET (7, 5), 15
        'COLOR 15, 3
        'DRAW "M631,5 M631,343 M7,343 M7,5"
        '
      COLOR 8, 3: LOCATE 10, 15
      PRINT "Codigo de Identificacao : "
      COLOR 11, 3: LOCATE 11, 15
      PRINT "(max. de 8 caracteres)"
      COLOR 11, 3: LOCATE 10, 41
      INPUT ARQCARGA$
      OPEN ARQCARGA$ FOR INPUT AS #1
      COLOR 4, 3: LOCATE 23, 10
      PRINT "Lendo o arquivo ..."
      '
      INPUT #1, NC, NA, NPONTOS, INCRMAX
      INPUT #1, LA, BA, PRESMAX, DT, DL
      FOR I = 1 TO NC
          INPUT #1, MAT$(I)
          INPUT #1, ICODE(I), E(I), POISS(I), H(I), K1(I), K2(I), K3(I), SDCRIT(I)
          INPUT #1, NDIVV(I), COESAO(I), FI(I), GAMA(I), SCOMPAC(I)
      NEXT I
      FOR I = 2 TO NPONTOS
          INPUT #1, X(I), Y(I)
      NEXT I
      IF (ICODE(NC) = 4) THEN
                              INPUT #1, SL1, SL2, SL3, SL4
                              INPUT #1, UMID1, UMID2, ZTOP
      END IF
      CLOSE #1
      RETURN
     '
     '
     '
2000 '****************
     '* EDITAR DADOS *
     '* (Subrotina)  *
     '****************
     '
     '- - - - - - - - - -
     '-  MENU PRINCIPAL -
     '- - - - - - - - - -
2010 CLS : KEY OFF
     '
     'SCREEN 9, 1, 0, 0
     ' Quadro da Tela
       'PSET (5, 3), 15
       'COLOR 15, 3
       'DRAW "M633,3 M633,345 M5,345 M5,3"
       'PSET (7, 5), 15
       'COLOR 15, 3
       'DRAW "M631,5 M631,343 M7,343 M7,5"
       '
       COLOR 5, 3
       LOCATE 3, 34: PRINT "EDICAO DE DADOS"
       'PSET (255, 23), 5
       'COLOR 5, 3
       'DRAW "M390,23 M390,45 M255,45 M255,23"
       'PAINT (256, 25), 7, 5
       'FOR I = 256 TO 388: PAINT (I, 33), 7, 5: NEXT I
       '
     IF (IGERAIS = 0) THEN ICOR = 8 ELSE ICOR = 4
        COLOR ICOR, 3: LOCATE 9, 34: PRINT "Dados Gerais"
        IF (IGERAIS = 0) THEN ICOR = 15 ELSE ICOR = 4
        COLOR ICOR, 3: LOCATE 9, 30: PRINT "(G)"
     IF (ICARGAS = 0) THEN ICOR = 8 ELSE ICOR = 4
        COLOR ICOR, 3: LOCATE 11, 34: PRINT "Cargas do Trafego"
        IF (ICARGAS = 0) THEN ICOR = 15 ELSE ICOR = 4
        COLOR ICOR, 3: LOCATE 11, 30: PRINT "(C)"
     IF (IPAVE = 0) THEN ICOR = 8 ELSE ICOR = 4
        COLOR ICOR, 3: LOCATE 13, 34: PRINT "Camadas do Pavimento"
        IF (IPAVE = 0) THEN ICOR = 15 ELSE ICOR = 4
        COLOR ICOR, 3: LOCATE 13, 30: PRINT "(P)"
     ICOR = 8: COLOR ICOR, 3
        LOCATE 15, 34: PRINT "Retornar ao Menu Principal"
        COLOR 15, 3: LOCATE 15, 30: PRINT "(R)"
        '
        COLOR 15, 3: LOCATE 23, 70: PRINT "A"
        COLOR 8, 3: LOCATE 23, 71: PRINT "juda"
     '
     OPDADOS$ = ""
     WHILE OPDADOS$ = "": OPDADOS$ = INKEY$: WEND
     OPDADOS$ = UCASE$(OPDADOS$)
     SELECT CASE OPDADOS$
            CASE "G"
                        IGERAIS = 1
                        GOSUB 2100
            CASE "C"
                        ICARGAS = 1
                        GOSUB 2200
            CASE "P"
                        IPAVE = 1
                        GOSUB 2300
            CASE "R"
                        RETURN
            CASE "A", "a"
                        GOSUB 2400
            CASE ELSE
                    FOR IV = 1 TO 2
                        LOCATE 23, 5
                        COLOR 15, 3
                        PRINT "ERRO: Codigo nao valido"
                        BEEP: COLOR 3, 3: LOCATE 23, 7
                        PRINT "                       "
                    NEXT IV
     END SELECT
     GOTO 2010
     '
     '
     '
2100 '******************
     '*  DADOS GERAIS  *
     '*  (Subrotina)   *
     '******************
     '
      CLS
      'SCREEN 9, 1, 0, 0
      ' Quadro da Tela
        'PSET (5, 3), 15
        'COLOR 15, 3
        'DRAW "M633,3 M633,345 M5,345 M5,3"
        'PSET (7, 5), 15
        'COLOR 15, 3
        'DRAW "M631,5 M631,343 M7,343 M7,5"
        '
       COLOR 5, 3
       LOCATE 3, 35: PRINT "DADOS GERAIS"
       'PSET (260, 23), 5
       'COLOR 5, 3
       'DRAW "M378,23 M378,45 M260,45 M260,23"
       'PAINT (262, 25), 7, 5
       'FOR I = 262 TO 376: PAINT (I, 33), 7, 5: NEXT I
       '
      COLOR 8, 3
      LOCATE 6, 10: PRINT "Numero de camadas do pavimento      = "
      LOCATE 7, 10: PRINT "Numero de cargas de roda            = "
      LOCATE 8, 10: PRINT "Numero de incrementos de carga      = "
      '
      COLOR 14, 3: LOCATE 6, 48: PRINT NC
      LOCATE 21, 5: PRINT "Entre com novo valor:            "
      COLOR 11, 3: LOCATE 21, 27: INPUT NC
      COLOR 8, 3: LOCATE 6, 48: PRINT NC
      '
      COLOR 14, 3: LOCATE 7, 48: PRINT NA
      LOCATE 21, 5: PRINT "Entre com novo valor:            "
      COLOR 11, 3: LOCATE 21, 27: INPUT NA
      COLOR 8, 3: LOCATE 7, 48: PRINT NA
      '
      COLOR 14, 3: LOCATE 8, 48: PRINT INCRMAX
      LOCATE 21, 5: PRINT "Entre com novo valor:            "
      COLOR 11, 3: LOCATE 21, 27: INPUT INCRMAX
      COLOR 8, 3: LOCATE 8, 48: PRINT INCRMAX
      '
      LOCATE 9, 10: PRINT "Numero de pontos (x,y) para analise = "
       ' Inclusao de um ponto (x,y) no centro de uma roda
         LOCATE 10, 10: PRINT "  (Alem do ponto no centro de uma roda)"
      COLOR 14, 3: LOCATE 9, 48: PRINT NPONTOS
      LOCATE 21, 5: PRINT "Entre com novo valor:            "
      COLOR 11, 3: LOCATE 21, 27: INPUT NPONTOS
      NPONTOS = NPONTOS + 1
      COLOR 8, 3
      LOCATE 9, 10: PRINT "Numero total de pontos (x,y)       =  "
         LOCATE 10, 10: PRINT "                                       "
      COLOR 8, 3: LOCATE 9, 48: PRINT NPONTOS
      '
      FOR I = 2 TO NPONTOS
          COLOR 8, 3: LOCATE 11, 10: PRINT "Ponto No.:     "
          COLOR 15, 3: LOCATE 11, 21: PRINT I
          COLOR 8, 3: LOCATE 12, 23: PRINT "x =         "
                      LOCATE 13, 23: PRINT "y =         "
          COLOR 14, 3: LOCATE 12, 27: PRINT X(I)
          LOCATE 21, 5: PRINT "Entre com novo valor:            "
          COLOR 11, 3: LOCATE 21, 27: INPUT X(I)
          COLOR 3, 3: LOCATE 12, 27: PRINT "               "
          COLOR 8, 3: LOCATE 12, 27: PRINT X(I)
          '
          COLOR 14, 3: LOCATE 13, 27: PRINT Y(I)
          LOCATE 21, 5: PRINT "Entre com novo valor:            "
          COLOR 11, 3: LOCATE 21, 27: INPUT Y(I)
          COLOR 3, 3: LOCATE 13, 27: PRINT "                  "
          COLOR 8, 3: LOCATE 13, 27: PRINT Y(I)
          '
      NEXT I
      RETURN
     '
     '
     '
2200 '***********************
     '*  CARGAS DO TRAFEGO  *
     '*     (Subrotina)     *
     '***********************
     '
      CLS
      'SCREEN 9, 1, 0, 0
      ' Quadro da Tela
        'PSET (5, 3), 15
        'COLOR 15, 3
        'DRAW "M633,3 M633,345 M5,345 M5,3"
        'PSET (7, 5), 15
        'COLOR 15, 3
        'DRAW "M631,5 M631,343 M7,343 M7,5"
        '
       COLOR 5, 3
       LOCATE 3, 34: PRINT "CARGAS DO TRAFEGO"
       'PSET (252, 23), 5
       'COLOR 5, 3
       'DRAW "M410,23 M410,45 M252,45 M252,23"
       'PAINT (254, 25), 7, 5
       'FOR I = 254 TO 409: PAINT (I, 33), 7, 5: NEXT I
       '
      COLOR 8, 3
      LOCATE 6, 10: PRINT "Carga por Roda (kgf)                = "
      LOCATE 8, 10: PRINT "Caso (R)odoviario ou (A)eroportuario:"
      COLOR 11, 3: LOCATE 6, 48: INPUT CARGA
                   LOCATE 8, 48: INPUT CASO$
      CASO$ = UCASE$(CASO$)
      IF (CASO$ = "R") THEN
                            COLOR 8, 3: LOCATE 10, 10
                            PRINT "Pressao do pneu e' conhecida (S/N):"
                            COLOR 11, 3: LOCATE 10, 46: INPUT PRESC$
                            PRESC$ = UCASE$(PRESC$)
                       ELSE
                            PRESC$ = "S"
      END IF
      IF (PRESC$ = "S") THEN
                             COLOR 8, 3: LOCATE 12, 10
                             PRINT "Pressao de Inflacao do Pneu (kgf/cm2) = "
                             COLOR 11, 3: LOCATE 12, 50: INPUT PRESMAX
                        ELSE
                             COLOR 9, 3: LOCATE 12, 10
                             PRINT "Tipo de Eixo:"
                             COLOR 8, 3: LOCATE 13, 23: PRINT "(ESRS)  Eixo Simples de Rodas Simples"
                             LOCATE 14, 23: PRINT "(ESRD)  Eixo Simples de Rodas Duplas"
                             LOCATE 15, 23: PRINT "(ETDRD) Eixo Tandem Duplo de Rodas Duplas"
                             LOCATE 16, 23: PRINT "(ETTRD) Eixo Tandem Triplo de Rodas Duplas"
                             COLOR 11, 3: LOCATE 12, 23: INPUT TIPOEIXO$
                             TIPOEIXO$ = UCASE$(TIPOEIXO$)
                             SELECT CASE TIPOEIXO$
                                    CASE "ESRS"
                                                 QEIXO = CARGA * 2 / 1000
                                                 Q1 = .5: Q2 = 7!
                                                 SELECT CASE QEIXO
                                                        CASE IS < Q1: Q = Q1
                                                        CASE IS > Q2: Q = Q2
                                                        CASE ELSE:    Q = QEIXO
                                                 END SELECT
                                                 PRESMAX = 22.413 * EXP(.2317 * Q)
                                    CASE "ESRD"
                                                 QEIXO = CARGA * 4 / 1000
                                                 Q1 = 2.3: Q2 = 15!
                                                 SELECT CASE QEIXO
                                                        CASE IS < Q1: Q = Q1
                                                        CASE IS > Q2: Q = Q2
                                                        CASE ELSE:    Q = QEIXO
                                                 END SELECT
                                                 PRESMAX = 10 + (50 / 6) * Q
                                    CASE "ETDRD"
                                                 QEIXO = CARGA * 8 / 1000
                                                 Q1 = 8.5: Q2 = 30.5
                                                 SELECT CASE QEIXO
                                                        CASE IS < Q1: Q = Q1
                                                        CASE IS > Q2: Q = Q2
                                                        CASE ELSE:    Q = QEIXO
                                                 END SELECT
                                                 PRESMAX = (80 + 60 * Q) / 14
                                    CASE "ETTRD"
                                                 QEIXO = CARGA * 12 / 1000
                                                 Q1 = 20: Q2 = 45
                                                 SELECT CASE QEIXO
                                                        CASE IS < Q1: Q = Q1
                                                        CASE IS > Q2: Q = Q2
                                                        CASE ELSE:    Q = QEIXO
                                                 END SELECT
                                                 PRESMAX = (-100 + 20 * Q) / 6
                                    CASE ELSE:   PRINT "ERRO": STOP
                             END SELECT
                             PRESMAX = PRESMAX * .0703
      END IF
      AREA = CARGA / PRESMAX
      LA = AREA ^ .5
      BA = LA
      SELECT CASE NA
             CASE 1
             CASE 2
                       COLOR 8, 3: LOCATE 18, 10
                       PRINT "Distancia entre rodas (cm) = "
                       COLOR 14, 3: LOCATE 18, 40: PRINT DT
                       LOCATE 21, 5: PRINT "Entre com novo valor:            "
                       COLOR 11, 3: LOCATE 21, 27: INPUT DT
                       COLOR 8, 3: LOCATE 18, 40: PRINT DT
             CASE 4, 6
                       COLOR 8, 3: LOCATE 18, 10
                       PRINT "Distancia entre rodas (cm) = "
                       LOCATE 19, 10
                       PRINT "Separacao entre eixos (cm) = "
                       '
                       COLOR 14, 3: LOCATE 18, 40: PRINT DT
                       LOCATE 21, 5: PRINT "Entre com novo valor:            "
                       COLOR 11, 3: LOCATE 21, 27: INPUT DT
                       COLOR 8, 3: LOCATE 18, 40: PRINT DT
                       '
                       COLOR 14, 3: LOCATE 19, 40: PRINT DL
                       LOCATE 21, 5: PRINT "Entre com novo valor:            "
                       COLOR 11, 3: LOCATE 21, 27: INPUT DL
                       COLOR 8, 3: LOCATE 19, 40: PRINT DL
                       '
             CASE ELSE
                       BEEP: COLOR 15, 3: LOCATE 23, 10
                       PRINT "ERRO: Numero de cargas inadmissivel."
                       FOR IV = 1 TO 200000: NEXT IV
                       COLOR 3, 3: LOCATE 23, 10
                       PRINT "                                    "
      END SELECT
      RETURN
     '
     '
     '
2300 '**************************
     '*  CAMADAS DO PAVIMENTO  *
     '*      (Subrotina)       *
     '**************************
     '
      FOR ILAYER = 1 TO NC
      '
          CLS
          'SCREEN 9, 1, 0, 0
          ' Quadro da Tela
            'PSET (5, 3), 15
            'COLOR 15, 3
            'DRAW "M633,3 M633,345 M5,345 M5,3"
            'PSET (7, 5), 15
            'COLOR 15, 3
            'DRAW "M631,5 M631,343 M7,343 M7,5"
            '
            COLOR 5, 3
            LOCATE 3, 32: PRINT "CAMADAS DO PAVIMENTO"
            'PSET (235, 23), 5
            'COLOR 5, 3
            'DRAW "M420,23 M420,45 M235,45 M235,23"
            'PAINT (237, 25), 7, 5
            'FOR I = 237 TO 418: PAINT (I, 33), 7, 5: NEXT I
            '
          COLOR 8, 3: LOCATE 5, 5: PRINT "Camada No.:      "
          COLOR 4, 3: LOCATE 5, 17: PRINT ILAYER
          COLOR 8, 3
          LOCATE 7, 10: PRINT "Espessura da Camada (cm)        =     "
          LOCATE 8, 10: PRINT "Coeficiente de Poisson          =     "
          LOCATE 9, 10: PRINT "Tipo de Material:"
          COLOR 15, 3
          LOCATE 10, 28: PRINT "(ASF)"
          LOCATE 11, 28: PRINT "(CIM)"
          LOCATE 12, 28: PRINT "(GRA)"
          LOCATE 13, 28: PRINT "(SOL)"
          COLOR 8, 3
          LOCATE 10, 34: PRINT "Mistura asfaltica"
          LOCATE 11, 34: PRINT "Material cimentado"
          LOCATE 12, 34: PRINT "Materiais granulares"
          LOCATE 13, 34: PRINT "Solos"
          COLOR 8, 3
          LOCATE 14, 10: PRINT "Tipo de Comportamento Resiliente:     "
          COLOR 15, 3
          LOCATE 15, 44: PRINT "(1)"
          LOCATE 16, 44: PRINT "(2)"
          LOCATE 17, 44: PRINT "(3)"
          LOCATE 18, 44: PRINT "(4)"
          LOCATE 19, 44: PRINT "(5)"
          LOCATE 20, 44: PRINT "(6)"
          COLOR 8, 3
          LOCATE 15, 48: PRINT "Granular"
          LOCATE 16, 48: PRINT "Coesivo"
          LOCATE 17, 48: PRINT "Linear"
          LOCATE 18, 48: PRINT "Modelo de Fossberg"
          LOCATE 19, 48: PRINT "Modelo Geral"
          LOCATE 20, 48: PRINT "Modelo Combinado"
          '
          COLOR 14, 3: LOCATE 7, 44: PRINT H(ILAYER)
          LOCATE 23, 5: PRINT "Entre com novo valor:            "
          COLOR 11, 3: LOCATE 23, 27: INPUT H(ILAYER)
          COLOR 3, 3: LOCATE 7, 44: PRINT "                 "
          COLOR 8, 3: LOCATE 7, 44: PRINT H(ILAYER)
          '
          COLOR 14, 3: LOCATE 8, 44: PRINT POISS(ILAYER)
          LOCATE 23, 5: PRINT "Entre com novo valor:            "
          COLOR 11, 3: LOCATE 23, 27: INPUT POISS(ILAYER)
          COLOR 3, 3: LOCATE 8, 44: PRINT "                "
          COLOR 8, 3: LOCATE 8, 44: PRINT POISS(ILAYER)
          '
          IF (MAT$(ILAYER) = "") THEN MAT$(ILAYER) = "XXX"
          COLOR 14, 3: LOCATE 9, 28: PRINT MAT$(ILAYER)
          LOCATE 23, 5: PRINT "Entre com novo valor:            "
          COLOR 11, 3: LOCATE 23, 27: INPUT MAT$(ILAYER)
          MAT$(ILAYER) = UCASE$(MAT$(ILAYER))
          COLOR 8, 3: LOCATE 9, 28: PRINT MAT$(ILAYER)
          '
          COLOR 14, 3: LOCATE 14, 44: PRINT ICODE(ILAYER)
          LOCATE 23, 5: PRINT "Entre com novo valor:            "
          COLOR 11, 3: LOCATE 23, 27: INPUT ICODE(ILAYER)
          COLOR 8, 3: LOCATE 14, 44: PRINT ICODE(ILAYER)
          '
          SELECT CASE ICODE(ILAYER)
                 CASE 1
                          COLOR 8, 3: LOCATE 16, 5: PRINT "Parametro K1 = "
                          LOCATE 18, 5: PRINT "Parametro K2 = "
                          LOCATE 20, 5: PRINT "Tensao residual da compactacao = "
                          '
                          COLOR 14, 3: LOCATE 16, 20: PRINT K1(ILAYER)
                          LOCATE 23, 5: PRINT "Entre com novo valor:            "
                          COLOR 11, 3: LOCATE 23, 27: INPUT K1(ILAYER)
                          COLOR 8, 3: LOCATE 16, 20: PRINT K1(ILAYER)
                          '
                          COLOR 14, 3: LOCATE 18, 20: PRINT K2(ILAYER)
                          LOCATE 23, 5: PRINT "Entre com novo valor:            "
                          COLOR 11, 3: LOCATE 23, 27: INPUT K2(ILAYER)
                          COLOR 8, 3: LOCATE 18, 20: PRINT K2(ILAYER)
                          '
                          COLOR 14, 3: LOCATE 20, 38: PRINT SCOMPAC(ILAYER)
                          LOCATE 23, 5: PRINT "Entre com novo valor:            "
                          COLOR 11, 3: LOCATE 23, 27: INPUT SCOMPAC(ILAYER)
                          COLOR 8, 3: LOCATE 20, 38: PRINT SCOMPAC(ILAYER)
                          '
                          E(ILAYER) = K1(ILAYER)
                 CASE 2, 6
                          COLOR 8, 3: LOCATE 16, 5
                          PRINT "Parametro K =              "
                          LOCATE 18, 5
                          PRINT "Parametro n =              "
                          LOCATE 20, 5
                          PRINT "Tensao desvio minima =         "
                          '
                          COLOR 14, 3: LOCATE 16, 19: PRINT K1(ILAYER)
                          LOCATE 23, 5: PRINT "Entre com novo valor:            "
                          COLOR 11, 3: LOCATE 23, 27: INPUT K1(ILAYER)
                          COLOR 8, 3: LOCATE 16, 19: PRINT K1(ILAYER)
                          '
                          COLOR 14, 3: LOCATE 18, 19: PRINT K2(ILAYER)
                          LOCATE 23, 5: PRINT "Entre com novo valor:            "
                          COLOR 11, 3: LOCATE 23, 27: INPUT K2(ILAYER)
                          COLOR 8, 3: LOCATE 18, 19: PRINT K2(ILAYER)
                          '
                          COLOR 14, 3: LOCATE 20, 28: PRINT SDCRIT(ILAYER)
                          LOCATE 23, 5: PRINT "Entre com novo valor:            "
                          COLOR 11, 3: LOCATE 23, 27: INPUT SDCRIT(ILAYER)
                          COLOR 8, 3: LOCATE 20, 28: PRINT SDCRIT(ILAYER)
                          '
                          E(ILAYER) = K1(ILAYER)
                 CASE 3
                          COLOR 8, 3: LOCATE 21, 10
                          PRINT "Modulo de Resiliencia (kgf/cm2) = "
                          COLOR 14, 3: LOCATE 21, 44: PRINT E(ILAYER)
                          LOCATE 23, 5: PRINT "Entre com novo valor:            "
                          COLOR 11, 3: LOCATE 23, 27: INPUT E(ILAYER)
                          COLOR 8, 3: LOCATE 21, 44: PRINT E(ILAYER)
                 CASE 4
                          COLOR 15, 3: BEEP:
                          LOCATE 23, 10: PRINT "Nao disponivel"
                          FOR IV = 1 TO 200000: NEXT IV
                          COLOR 3, 3: LOCATE 23, 10
                          PRINT "                       "
                          '    INPUT #1, SL1, SL2, SL3, SL4
                          '    INPUT #1, UMID1, UMID2, ZTOP
                 CASE 5
                          COLOR 8, 3
                          LOCATE 16, 5: PRINT "Parametro K1 = "
                          LOCATE 17, 5: PRINT "Parametro K2 = "
                          LOCATE 18, 5: PRINT "Parametro K3 = "
                          LOCATE 19, 5: PRINT "Tensao desvio minima = "
                          LOCATE 20, 5: PRINT "Tensao residual da compactacao = "
                          '
                          COLOR 14, 3: LOCATE 16, 20: PRINT K1(ILAYER)
                          LOCATE 23, 5: PRINT "Entre com novo valor:            "
                          COLOR 11, 3: LOCATE 23, 27: INPUT K1(ILAYER)
                          COLOR 8, 3: LOCATE 16, 20: PRINT K1(ILAYER)
                          '
                          COLOR 14, 3: LOCATE 17, 20: PRINT K2(ILAYER)
                          LOCATE 23, 5: PRINT "Entre com novo valor:            "
                          COLOR 11, 3: LOCATE 23, 27: INPUT K2(ILAYER)
                          COLOR 8, 3: LOCATE 17, 20: PRINT K2(ILAYER)
                          '
                          COLOR 14, 3: LOCATE 18, 20: PRINT K3(ILAYER)
                          LOCATE 23, 5: PRINT "Entre com novo valor:            "
                          COLOR 11, 3: LOCATE 23, 27: INPUT K3(ILAYER)
                          COLOR 8, 3: LOCATE 18, 20: PRINT K3(ILAYER)
                          '
                          COLOR 14, 3: LOCATE 19, 28: PRINT SDCRIT(ILAYER)
                          LOCATE 23, 5: PRINT "Entre com novo valor:            "
                          COLOR 11, 3: LOCATE 23, 27: INPUT SDCRIT(ILAYER)
                          COLOR 8, 3: LOCATE 19, 28: PRINT SDCRIT(ILAYER)
                          '
                          COLOR 14, 3: LOCATE 20, 38: PRINT SCOMPAC(ILAYER)
                          LOCATE 23, 5: PRINT "Entre com novo valor:            "
                          COLOR 11, 3: LOCATE 23, 27: INPUT SCOMPAC(ILAYER)
                          COLOR 8, 3: LOCATE 20, 38: PRINT SCOMPAC(ILAYER)
                          '
                          E(ILAYER) = K1(ILAYER)
                 CASE ELSE
                           BEEP: COLOR 15, 3: LOCATE 23, 10
                           PRINT "ERRO: Codigo inadmissivel."
                           FOR IV = 1 TO 200000: NEXT IV
                           COLOR 3, 3: LOCATE 23, 10
                           PRINT "                          "
          END SELECT
      NEXT ILAYER
          FOR I = 1 TO NC
              IF (I < NC) THEN NDIVV(I) = 5 ELSE NDIVV(I) = 8
              COESAO(I) = 0!
              FI(I) = 35!
              GAMA(I) = .0018
          NEXT I
      RETURN
     '
     '
     '
2400 '**************************************
     '*  AJUDA NA TELA DE EDICAO DE DADOS  *
     '*             (Subrotina)            *
     '**************************************
      CLS
      'SCREEN 9, 1, 0, 0
     ' Quadro da Tela
       'PSET (5, 3), 15
       'COLOR 15, 3
       'DRAW "M633,3 M633,345 M5,345 M5,3"
       'PSET (7, 5), 15
       'COLOR 15, 3
       'DRAW "M631,5 M631,343 M7,343 M7,5"
       '
       COLOR 5, 3
       LOCATE 3, 34: PRINT "EDICAO DE DADOS"
       'PSET (255, 23), 5
       'COLOR 5, 3
       'DRAW "M390,23 M390,45 M255,45 M255,23"
       'PAINT (256, 25), 7, 5
       'FOR I = 256 TO 388: PAINT (I, 33), 7, 5: NEXT I
       '
       'COLOR 8, 3
       'LOCATE 9, 4: PRINT "    Nesta tela,  "
       COLOR 14, 3
       LOCATE 23, 5: PRINT "Pressione qualquer tecla para retornar ao Menu Principal"
       B$ = ""
       WHILE B$ = "": B$ = INKEY$: WEND
       RETURN
       '
       '
       '
3000 '****************************
     '* SALVAR OS DADOS EDITADOS *
     '*       (Subrotina)        *
     '****************************
     '
      CLS
      'SCREEN 9, 1, 0, 0
      ' Quadro da Tela
        'PSET (5, 3), 15
        'COLOR 15, 3
        'DRAW "M633,3 M633,345 M5,345 M5,3"
        'PSET (7, 5), 15
        'COLOR 15, 3
        'DRAW "M631,5 M631,343 M7,343 M7,5"
        '
      COLOR 8, 3: LOCATE 10, 15
      PRINT "Codigo de Identificacao : "
      COLOR 11, 3: LOCATE 11, 15
      PRINT "(max. de 8 caracteres)"
      COLOR 11, 3: LOCATE 10, 41
      INPUT ARQSALVA$
      OPEN ARQSALVA$ FOR OUTPUT AS #2
      COLOR 4, 3: LOCATE 23, 10
      PRINT "Escrevendo no arquivo ..."
      '
      WRITE #2, NC, NA, NPONTOS, INCRMAX
      PRINT #2, USING "##########.####"; LA, BA, PRESMAX, DT, DL
      FOR I = 1 TO NC
          PRINT #2, "'"; MAT$(I); "'"
          PRINT #2, ICODE(I), USING "#############.####"; E(I), POISS(I), H(I), K1(I), K2(I), K3(I), SDCRIT(I)
          PRINT #2, NDIVV(I), USING "#############.####"; COESAO(I), FI(I), GAMA(I), SCOMPAC(I)
      NEXT I
      FOR I = 2 TO NPONTOS
          PRINT #2, USING "########.####"; X(I), Y(I)
      NEXT I
      IF (ICODE(NC) = 4) THEN
                              PRINT #2, USING "#########.####"; SL1, SL2, SL3, SL4
                              PRINT #2, USING "#########.####"; UMID1, UMID2, ZTOP
      END IF
      CLOSE #2
      RETURN
     '
     '
     '
4000 '***********************
     '* DADOS PARA O LAYMOD *
     '*     (Subrotina)     *
     '***********************
     '
      CLS
      'SCREEN 9, 1, 0, 0
      ' Quadro da Tela
        'PSET (5, 3), 15
        'COLOR 15, 3
        'DRAW "M633,3 M633,345 M5,345 M5,3"
        'PSET (7, 5), 15
        'COLOR 15, 3
        'DRAW "M631,5 M631,343 M7,343 M7,5"
        '
      ARQFAIXAS$ = "FAIXAS.DAT"
      OPEN ARQFAIXAS$ FOR OUTPUT AS #3
      COLOR 4, 3: LOCATE 2, 15
      PRINT "DADOS PARA O PROGRAMA LAYMOD"
      FOR I = 1 TO NC
          COLOR 8, 3: LOCATE 9, 10
          PRINT "Camada No.:     "
          COLOR 5, 3: LOCATE 9, 22: PRINT I
          SELECT CASE ICODE(I)
                 CASE 1
                           COLOR 8, 3: LOCATE 11, 25
                           PRINT "Valor Minimo de K1 =            "
                           LOCATE 13, 25
                           PRINT "Valor Maximo de K1 =            "
                           LOCATE 15, 25
                           PRINT "Valor Minimo de K2 =            "
                           LOCATE 17, 25
                           PRINT "Valor Maximo de K2 =            "
                           COLOR 11, 3
                           LOCATE 11, 46: INPUT K1MIN
                           LOCATE 13, 46: INPUT K1MAX
                           LOCATE 15, 46: INPUT K2MIN
                           LOCATE 17, 46: INPUT K2MAX
                           LOCATE 11, 25: PRINT "                                "
                           LOCATE 13, 25: PRINT "                                "
                           LOCATE 15, 25: PRINT "                                "
                           LOCATE 17, 25: PRINT "                                "
                           WRITE #3, I, ICODE(I), K1MIN, K1MAX, K2MIN, K2MAX
                 CASE 2, 6
                           COLOR 8, 3: LOCATE 11, 25
                           PRINT "Valor Minimo de K =            "
                           LOCATE 13, 25
                           PRINT "Valor Maximo de K =            "
                           LOCATE 15, 25
                           PRINT "Valor Minimo de n =            "
                           LOCATE 17, 25
                           PRINT "Valor Maximo de n =            "
                           COLOR 11, 3
                           LOCATE 11, 45: INPUT K1MIN
                           LOCATE 13, 45: INPUT K1MAX
                           LOCATE 15, 45: INPUT K2MIN
                           LOCATE 17, 45: INPUT K2MAX
                           LOCATE 11, 25: PRINT "                                "
                           LOCATE 13, 25: PRINT "                                "
                           LOCATE 15, 25: PRINT "                                "
                           LOCATE 17, 25: PRINT "                                "
                           WRITE #3, I, ICODE(I), K1MIN, K1MAX, K2MIN, K2MAX
                 CASE 3
                           COLOR 8, 3: LOCATE 11, 25
                           PRINT "Valor Minimo de E =            "
                           LOCATE 13, 25
                           PRINT "Valor Maximo de E =            "
                           COLOR 11, 3
                           LOCATE 11, 45: INPUT EMIN
                           LOCATE 13, 45: INPUT EMAX
                           LOCATE 11, 25: PRINT "                                       "
                           LOCATE 13, 25: PRINT "                                       "
                           WRITE #3, I, ICODE(I), EMIN, EMAX
                 CASE 5
                           COLOR 8, 3: LOCATE 11, 25
                           PRINT "Valor Minimo de K1 =            "
                           LOCATE 13, 25
                           PRINT "Valor Maximo de K1 =            "
                           LOCATE 15, 25
                           PRINT "Valor Minimo de K2 =            "
                           LOCATE 17, 25
                           PRINT "Valor Maximo de K2 =            "
                           LOCATE 19, 25
                           PRINT "Valor Minimo de K3 =            "
                           LOCATE 21, 25
                           PRINT "Valor Maximo de K3 =            "
                           COLOR 11, 3
                           LOCATE 11, 46: INPUT K1MIN
                           LOCATE 13, 46: INPUT K1MAX
                           LOCATE 15, 46: INPUT K2MIN
                           LOCATE 17, 46: INPUT K2MAX
                           LOCATE 19, 46: INPUT K3MIN
                           LOCATE 21, 46: INPUT K3MAX
                           LOCATE 11, 25: PRINT "                                "
                           LOCATE 13, 25: PRINT "                                "
                           LOCATE 15, 25: PRINT "                                "
                           LOCATE 17, 25: PRINT "                                "
                           LOCATE 19, 25: PRINT "                                "
                           LOCATE 21, 25: PRINT "                                "
                           WRITE #3, I, ICODE(I), K1MIN, K1MAX, K2MIN, K2MAX, K3MIN, K3MAX
                 CASE ELSE
                           PRINT "ERRO"
                           STOP
          END SELECT
      NEXT I
      CLOSE #3
      RETURN
     '
     '
     '

