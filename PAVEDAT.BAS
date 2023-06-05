   
DIM ICODE(15), E(30), POISS(30), H(40), K1(15), K2(15), SDCRIT(15), SCOMPAC(15)
DIM NDIVV(15), COESAO(15), FI(15), GAMA(15), X(12), Y(12), K3(15), MAT$(15)
   
'********************
'*  MENU PRINCIPAL  *
'********************
10 CLS: KEY OFF
   
COLOR 15: LOCATE 3, 34: PRINT "P A V E D A T"
COLOR 7: LOCATE 5, 18
PRINT "Edicao dos Dados para as Analises pelo FLAPS"
   
IPAD = 3
IF (ICARGA = 0) THEN ICOR = IPAD ELSE ICOR = 4
COLOR ICOR: LOCATE 11, 35: PRINT "Carregar Arquivo"
IF (ICARGA = 0) THEN ICOR = 15 ELSE ICOR = 4
COLOR ICOR: LOCATE 11, 31: PRINT "(1)"
IF (IEDITA = 0) THEN ICOR = IPAD ELSE ICOR = 4
COLOR ICOR: LOCATE 13, 35: PRINT "Editar Dados"
IF (IEDITA = 0) THEN ICOR = 15 ELSE ICOR = 4
COLOR ICOR: LOCATE 13, 31: PRINT "(2)"
IF (ISALVA = 0) THEN ICOR = IPAD ELSE ICOR = 4
COLOR ICOR: LOCATE 15, 35: PRINT "Salvar Arquivo"
IF (ISALVA = 0) THEN ICOR = 15 ELSE ICOR = 4
COLOR ICOR: LOCATE 15, 31: PRINT "(3)"
ICOR = IPAD: COLOR ICOR
LOCATE 17, 35: PRINT "Finalizar"
COLOR 15: LOCATE 17, 31: PRINT "(4)"
        
COLOR 15: LOCATE 23, 70: PRINT "A"
COLOR IPAD: LOCATE 23, 71: PRINT "juda"
   
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
        COLOR 7
        CLS
        SYSTEM
    CASE "A", "a"
        GOSUB 500
    CASE ELSE
        FOR IV = 1 TO 2
            LOCATE 23, 5
            COLOR 15
            PRINT "ERRO: Codigo nao valido"
            BEEP: COLOR 3: LOCATE 23, 7
            PRINT "                       "
        NEXT IV
END SELECT
GOTO 10
   
   
500 '**************************
'* AJUDA NA PRIMEIRA TELA *
'*      (Subrotina)       *
'**************************
CLS
COLOR 15: LOCATE 3, 36: PRINT "P A V E D A T"
COLOR 7: LOCATE 5, 20
COLOR 2
LOCATE 9, 4: PRINT "    Este programa e' a interface de entrada de dados para o programa FLAPS."
LOCATE 11, 4: PRINT "Entre na opcao (2) se os dados serao inseridos do inicio ao fim e grave-os"
LOCATE 13, 4: PRINT "em um arquivo na opcao (3). A opcao (1) deve ser utilizada quando se dese-"
LOCATE 15, 4: PRINT "jar alterar um arquivo de dados pre'-existente."
COLOR 13
LOCATE 23, 5: PRINT "Pressione qualquer tecla para retornar ao Menu Principal"
510 B$ = INKEY$: IF (B$ = "") THEN 510
RETURN
    
    
1000 '******************************
'* CARREGAR ARQUIVO EXISTENTE *
'*        (Subrotina)         *
'******************************
     
CLS
COLOR 15: LOCATE 10, 15
PRINT "Codigo de Identificacao : "
COLOR 11: LOCATE 12, 15
PRINT "(max. de 8 caracteres)"
COLOR 11: LOCATE 10, 41
INPUT ARQCARGA$
OPEN ARQCARGA$ FOR INPUT AS #1
COLOR 4: LOCATE 23, 10
PRINT "Lendo o arquivo ..."
      
INPUT #1, NC, NA, NPONTOS, INCRMAX
INPUT #1, LA, BA, PRESMAX, DT, DL
CARGA = PRESMAX * LA * BA
FOR I = 1 TO NC
    INPUT #1, MAT$(I)
    INPUT #1, ICODE(I), E(I), POISS(I), H(I), K1(I), K2(I), K3(I), SDCRIT(I)
    INPUT #1, NDIVV(I), COESAO(I), FI(I), GAMA(I), SCOMPAC(I)
NEXT I
FOR I = 2 TO NPONTOS
    INPUT #1, X(I), Y(I)
NEXT I
CLOSE #1
RETURN
     
     
2000 '****************
'* EDITAR DADOS *
'* (Subrotina)  *
'****************
     
'- - - - - - - - - -
'-  MENU PRINCIPAL -
'- - - - - - - - - -
2010 CLS: KEY OFF
COLOR 15: LOCATE 5, 28: PRINT "E D I C A O   D E   D A D O S"
     
IDAD = 3
IF (IGERAIS = 0) THEN ICOR = IDAD ELSE ICOR = 4
COLOR ICOR: LOCATE 11, 32: PRINT "Dados Gerais"
IF (IGERAIS = 0) THEN ICOR = 15 ELSE ICOR = 4
COLOR ICOR: LOCATE 11, 28: PRINT "(G)"
IF (ICARGAS = 0) THEN ICOR = IDAD ELSE ICOR = 4
COLOR ICOR: LOCATE 13, 32: PRINT "Cargas do Trafego"
IF (ICARGAS = 0) THEN ICOR = 15 ELSE ICOR = 4
COLOR ICOR: LOCATE 13, 28: PRINT "(C)"
IF (IPAVE = 0) THEN ICOR = IDAD ELSE ICOR = 4
COLOR ICOR: LOCATE 15, 32: PRINT "Camadas do Pavimento"
IF (IPAVE = 0) THEN ICOR = 15 ELSE ICOR = 4
COLOR ICOR: LOCATE 15, 28: PRINT "(P)"
ICOR = IDAD: COLOR ICOR
LOCATE 17, 32: PRINT "Retornar ao Menu Principal"
COLOR 15: LOCATE 17, 28: PRINT "(R)"
     
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
    CASE ELSE
        FOR IV = 1 TO 2
            LOCATE 23, 5
            COLOR 15
            PRINT "ERRO: Codigo nao valido"
            BEEP: COLOR 3, 3: LOCATE 23, 7
            PRINT "                       "
        NEXT IV
END SELECT
GOTO 2010
     
     
2100 '******************
'*  DADOS GERAIS  *
'*  (Subrotina)   *
'******************
     
CLS
COLOR 15: LOCATE 3, 30: PRINT "D A D O S   G E R A I S"
      
COLOR 3: IL = 6
LOCATE IL, 10: PRINT "Numero de camadas do pavimento      = "
COLOR 14: LOCATE IL, 48: PRINT NC
LOCATE 21, 5: PRINT "Entre com novo valor:            "
COLOR 11: LOCATE 21, 27: INPUT NC
COLOR 7: LOCATE IL, 48: PRINT NC
      
COLOR 3: IL = IL + 1
LOCATE IL, 10: PRINT "Tipo de Eixo:"
LOCATE IL + 1, 23: PRINT "(ESRS)  Eixo Simples de Rodas Simples"
LOCATE IL + 2, 23: PRINT "(ESRD)  Eixo Simples de Rodas Duplas"
LOCATE IL + 3, 23: PRINT "(TDRD)  Eixo Tandem Duplo de Rodas Duplas"
LOCATE IL + 4, 23: PRINT "(TTRD)  Eixo Tandem Triplo de Rodas Duplas"
LOCATE IL + 5, 23: PRINT "(TDRS)  Eixo Tandem Duplo de Rodas Simples"
LOCATE IL + 6, 23: PRINT "(TTRS)  Eixo Tandem Triplo de Rodas Simples"
LOCATE IL + 7, 23: PRINT "(TDM)   Eixo Tandem Duplo Misto"
LOCATE 21, 5: PRINT "Entre com novo valor:            "
COLOR 11: LOCATE 21, 27: INPUT TIPOEIXO$
TIPOEIXO$ = UCASE$(TIPOEIXO$)
COLOR 7: LOCATE IL, 49: PRINT TIPOEIXO$

IL = IL + 8: COLOR 3
LOCATE IL, 10: PRINT "Analise (L)inear ou (N)ao linear    = "
2110 IF INCRMAX = 1 THEN TIPOANALISE$ = "L"
IF INCRMAX > 1 THEN TIPOANALISE$ = "N"
IF INCRMAX = 0 THEN TIPOANALISE$ = "L"
COLOR 14: LOCATE IL, 49: PRINT TIPOANALISE$
LOCATE 21, 5: PRINT "Entre com novo valor:            "
COLOR 11: LOCATE 21, 27: INPUT TIPOANALISE$
TIPOANALISE$ = UCASE$(TIPOANALISE$)
COLOR 7: LOCATE IL, 49: PRINT TIPOANALISE$
SELECT CASE TIPOANALISE$
    CASE "L": INCRMAX = 1
    CASE "N": INCRMAX = 4
    CASE ELSE
        COLOR 12: LOCATE 23, 50
        PRINT "Codigo Invalido"
        GOTO 2110
END SELECT
      
NPTMAX = 10
COLOR 3
IL = IL + 1
LOCATE IL, 10: PRINT "Numero de pontos (x,y) para analise = "
' Inclusao de um ponto (x,y) no centro de uma roda
LOCATE IL + 1, 10: PRINT "(Alem do ponto no centro de uma roda)"
COLOR 14: LOCATE IL, 48: PRINT NPONTOS
LOCATE 21, 5: PRINT "Entre com novo valor             "
COLOR 15: LOCATE 21, 27: PRINT "(max. de "; NPTMAX - 1; " pontos):"
COLOR 11: LOCATE 21, 49: INPUT NPONTOS
NPONTOS = NPONTOS + 1
COLOR 3
LOCATE IL, 10: PRINT "Numero total de pontos (x,y)       =  "
LOCATE IL + 1, 10: PRINT "                                       "
COLOR 7: LOCATE IL, 48: PRINT NPONTOS
      
IL = IL + 1
FOR I = 2 TO NPONTOS
    COLOR 3: LOCATE IL, 10: PRINT "Ponto No.:     "
    COLOR 15: LOCATE IL, 21: PRINT I
    COLOR 3: LOCATE IL + 1, 23: PRINT "x =         "
    LOCATE IL + 2, 23: PRINT "y =         "
    COLOR 14: LOCATE IL + 1, 27: PRINT X(I)
    LOCATE 21, 5: PRINT "Entre com novo valor:                               "
    COLOR 11: LOCATE 21, 27: INPUT X(I)
    COLOR 3: LOCATE IL + 1, 27: PRINT "               "
    COLOR 7: LOCATE IL + 1, 27: PRINT X(I)

    COLOR 14: LOCATE IL + 2, 27: PRINT Y(I)
    LOCATE 21, 5: PRINT "Entre com novo valor:            "
    COLOR 11: LOCATE 21, 27: INPUT Y(I)
    COLOR 3: LOCATE IL + 2, 27: PRINT "                  "
    COLOR 7: LOCATE IL + 2, 27: PRINT Y(I)
NEXT I
     
RETURN
     
     
2200 '***********************
'*  CARGAS DO TRAFEGO  *
'*     (Subrotina)     *
'***********************
     
CLS
COLOR 15
LOCATE 3, 26: PRINT "C A R G A S   D O   T R A F E G O"
COLOR 3
LOCATE 6, 10: PRINT "Carga por Roda (kgf)                = "
LOCATE 8, 10: PRINT "Caso (R)odoviario ou (A)eroportuario:"
COLOR 11: LOCATE 6, 50: INPUT CARGA
LOCATE 8, 50: INPUT CASO$
CASO$ = UCASE$(CASO$)
IF CASO$ = "R" THEN
    COLOR 3: LOCATE 10, 10
    PRINT "Pressao do pneu e' conhecida (S/N):"
    COLOR 11: LOCATE 10, 50: INPUT PRESC$
    PRESC$ = UCASE$(PRESC$)
ELSE
    PRESC$ = "S"
END IF
IF PRESC$ = "S" THEN
    COLOR 3: LOCATE 12, 10
    PRINT "Pressao de Inflacao do Pneu (kgf/cm2) = "
    COLOR 11: LOCATE 12, 50: INPUT PRESMAX
ELSE
    SELECT CASE TIPOEIXO$
        CASE "ESRS"
            QEIXO = CARGA * 2 / 1000
            Q1 = .5: Q2 = 7!
            SELECT CASE QEIXO
                CASE IS < Q1: Q = Q1
                CASE IS > Q2: Q = Q2
                CASE ELSE: Q = QEIXO
            END SELECT
            PRESMAX = 22.413 * EXP(.2317 * Q)
        CASE "ESRD"
            QEIXO = CARGA * 4 / 1000
            Q1 = 2.3: Q2 = 15!
            SELECT CASE QEIXO
                CASE IS < Q1: Q = Q1
                CASE IS > Q2: Q = Q2
                CASE ELSE: Q = QEIXO
            END SELECT
            PRESMAX = 10 + (50 / 6) * Q
        CASE "TDRD", "TDRS", "TDM"
            QEIXO = CARGA * 8 / 1000
            Q1 = 8.5: Q2 = 30.5
            SELECT CASE QEIXO
                CASE IS < Q1: Q = Q1
                CASE IS > Q2: Q = Q2
                CASE ELSE: Q = QEIXO
            END SELECT
            PRESMAX = (80 + 60 * Q) / 14
        CASE "TTRD", "TTRS"
            QEIXO = CARGA * 12 / 1000
            Q1 = 20: Q2 = 45
            SELECT CASE QEIXO
                CASE IS < Q1: Q = Q1
                CASE IS > Q2: Q = Q2
                CASE ELSE: Q = QEIXO
            END SELECT
            PRESMAX = (-100 + 20 * Q) / 6
        CASE ELSE
            PRINT "ERRO": STOP
    END SELECT
    PRESMAX = PRESMAX * .0703
END IF
AREA = CARGA / PRESMAX
LA = AREA ^ .5
BA = LA
SELECT CASE TIPOEIXO$
    CASE "ESRS"
        ' Coordenadas do ponto (x,y) no centro de uma roda
        X(1) = 0!
        Y(1) = 0!
        NA = 1
    CASE "ESRD"
        COLOR 3: LOCATE 18, 10
        PRINT "Distancia entre rodas (cm) = "
        COLOR 11: LOCATE 18, 40: PRINT DT
        LOCATE 21, 5: PRINT "Entre com novo valor:            "
        COLOR 11: LOCATE 21, 27: INPUT DT
        COLOR 11: LOCATE 18, 40: PRINT DT
        ' Coordenadas do ponto (x,y) no centro de uma roda
        X(1) = DT / 2
        Y(1) = 0!
        NA = 2
    CASE "TDM"
        COLOR 3: LOCATE 18, 10
        PRINT "Distancia entre rodas (cm) = "
        LOCATE 19, 10
        PRINT "Separacao entre eixos (cm) = "

        COLOR 14: LOCATE 18, 40: PRINT DT
        LOCATE 21, 5: PRINT "Entre com novo valor:            "
        COLOR 11: LOCATE 21, 27: INPUT DT
        COLOR 3: LOCATE 18, 40: PRINT DT

        COLOR 14: LOCATE 19, 40: PRINT DL
        LOCATE 21, 5: PRINT "Entre com novo valor:            "
        COLOR 11: LOCATE 21, 27: INPUT DL
        COLOR 3: LOCATE 19, 40: PRINT DL

        X(1) = DT / 2
        Y(1) = DL / 2
        NA = 3

    CASE "TDRD"
        COLOR 3: LOCATE 18, 10
        PRINT "Distancia entre rodas (cm) = "
        LOCATE 19, 10
        PRINT "Separacao entre eixos (cm) = "

        COLOR 14: LOCATE 18, 40: PRINT DT
        LOCATE 21, 5: PRINT "Entre com novo valor:            "
        COLOR 11: LOCATE 21, 27: INPUT DT
        COLOR 3: LOCATE 18, 40: PRINT DT

        COLOR 14: LOCATE 19, 40: PRINT DL
        LOCATE 21, 5: PRINT "Entre com novo valor:            "
        COLOR 11: LOCATE 21, 27: INPUT DL
        COLOR 3: LOCATE 19, 40: PRINT DL

        X(1) = DT / 2
        Y(1) = DL / 2
        NA = 4
    CASE "TTRD"
        COLOR 3: LOCATE 18, 10
        PRINT "Distancia entre rodas (cm) = "
        LOCATE 19, 10
        PRINT "Separacao entre eixos (cm) = "

        COLOR 14: LOCATE 18, 40: PRINT DT
        LOCATE 21, 5: PRINT "Entre com novo valor:            "
        COLOR 11: LOCATE 21, 27: INPUT DT
        COLOR 3: LOCATE 18, 40: PRINT DT

        COLOR 14: LOCATE 19, 40: PRINT DL
        LOCATE 21, 5: PRINT "Entre com novo valor:            "
        COLOR 11: LOCATE 21, 27: INPUT DL
        COLOR 3: LOCATE 19, 40: PRINT DL

        X(1) = DT / 2
        Y(1) = 0!
        NA = 6
    CASE "TDRS"
        COLOR 3: LOCATE 18, 10
        PRINT "Separacao entre eixos (cm) = "
        COLOR 14: LOCATE 18, 40: PRINT DL
        LOCATE 21, 5: PRINT "Entre com novo valor:            "
        COLOR 11: LOCATE 21, 27: INPUT DL
        COLOR 3: LOCATE 18, 40: PRINT DL
        X(1) = 0
        Y(1) = DL / 2
        NA = 2
    CASE "TTRS"
        COLOR 3: LOCATE 18, 10
        PRINT "Separacao entre eixos (cm) = "
        COLOR 14: LOCATE 18, 40: PRINT DL
        LOCATE 21, 5: PRINT "Entre com novo valor:            "
        COLOR 11: LOCATE 21, 27: INPUT DL
        COLOR 3: LOCATE 18, 40: PRINT DL
        X(1) = 0
        Y(1) = 0
        NA = 3
    CASE ELSE
        BEEP: COLOR 15: LOCATE 23, 10
        PRINT "ERRO: Numero de cargas inadmissivel."
        FOR IV = 1 TO 200000: NEXT IV
        LOCATE 23, 10
        PRINT "                                    "
END SELECT
RETURN
     
     
2300 '**************************
'*  CAMADAS DO PAVIMENTO  *
'*      (Subrotina)       *
'**************************
     
FOR ILAYER = 1 TO NC

    CLS
    COLOR 15
    LOCATE 3, 25: PRINT "C A M A D A S   D O   P A V I M E N T O"
    COLOR 3: LOCATE 5, 5: PRINT "Camada No.:      "
    COLOR 11: LOCATE 5, 17: PRINT ILAYER
    COLOR 3
    LOCATE 7, 10: PRINT "Espessura da Camada (cm)        =     "
    LOCATE 8, 10: PRINT "Tipo de Material:"
    COLOR 15
    LOCATE 9, 28: PRINT "(ASF)"
    LOCATE 10, 28: PRINT "(CIM)"
    LOCATE 11, 28: PRINT "(GRA)"
    LOCATE 12, 28: PRINT "(SOL)"
    COLOR 3
    LOCATE 9, 34: PRINT "Mistura asfaltica"
    LOCATE 10, 34: PRINT "Material cimentado"
    LOCATE 11, 34: PRINT "Materiais granulares"
    LOCATE 12, 34: PRINT "Solos"
    LOCATE 13, 10: PRINT "Coeficiente de Poisson          =     "
    COLOR 3
    LOCATE 14, 10: PRINT "Tipo de Comportamento Resiliente:     "
    COLOR 15
    LOCATE 15, 44: PRINT "(1)"
    LOCATE 16, 44: PRINT "(2)"
    LOCATE 17, 44: PRINT "(3)"
    LOCATE 18, 44: PRINT "(4)"
    LOCATE 19, 44: PRINT "(5)"
    LOCATE 20, 44: PRINT "(6)"
    COLOR 3
    LOCATE 15, 48: PRINT "Granular"
    LOCATE 16, 48: PRINT "Coesivo"
    LOCATE 17, 48: PRINT "Linear"
    LOCATE 18, 48: PRINT "Modelo E = f(Epsilon)"
    LOCATE 19, 48: PRINT "Modelo Geral"
    LOCATE 20, 48: PRINT "Modelo Combinado"

    COLOR 11: LOCATE 7, 44: PRINT H(ILAYER)
    COLOR 14: LOCATE 23, 5: PRINT "Entre com novo valor:            "
    COLOR 11: LOCATE 23, 27: INPUT H(ILAYER)
    LOCATE 7, 44: PRINT "                 "
    COLOR 11: LOCATE 7, 44: PRINT H(ILAYER)

    IF MAT$(ILAYER) = "" THEN MAT$(ILAYER) = "XXX"
    COLOR 11: LOCATE 8, 28: PRINT MAT$(ILAYER)
    LOCATE 23, 5: PRINT "Entre com novo valor:            "
    COLOR 11: LOCATE 23, 27: INPUT MAT$(ILAYER)
    MAT$(ILAYER) = UCASE$(MAT$(ILAYER))
    COLOR 11: LOCATE 8, 28: PRINT "              "
    LOCATE 8, 28: PRINT MAT$(ILAYER)

    IF MAT$(ILAYER) = "ASF" THEN
       POISS(ILAYER) = 0.33
    ELSE
       IF MAT$(ILAYER) = "GRA" THEN
          POISS(ILAYER) = 0.35
       ELSE
          IF MAT$(ILAYER) = "CIM" THEN
             POISS(ILAYER) = 0.20
          ELSE
             POISS(ILAYER) = 0.40
          END IF
       END IF
    END IF
    COLOR 11: LOCATE 13, 44: PRINT POISS(ILAYER)
    LOCATE 23, 5: PRINT "Entre com novo valor:            "
    COLOR 11: LOCATE 23, 27: INPUT POISS(ILAYER)
    LOCATE 8, 44: PRINT "                "
    COLOR 11: LOCATE 13, 44: PRINT POISS(ILAYER)

    IF TIPOANALISE$ = "L" THEN
        ICODE(ILAYER) = 3
        COLOR 11: LOCATE 14, 44
        PRINT ICODE(ILAYER)
    ELSE
        COLOR 14: LOCATE 14, 44
        PRINT ICODE(ILAYER)
        LOCATE 23, 5: PRINT "Entre com novo valor:            "
        COLOR 11: LOCATE 23, 27: INPUT ICODE(ILAYER)
        COLOR 11: LOCATE 14, 44: PRINT ICODE(ILAYER)
    END IF

    SELECT CASE ICODE(ILAYER)
        CASE 1
            COLOR 3: LOCATE 16, 5: PRINT "Parametro K1 = "
            LOCATE 18, 5: PRINT "Parametro K2 = "
            LOCATE 20, 5: PRINT "Tensao residual da compactacao = "

            COLOR 11: LOCATE 16, 20: PRINT K1(ILAYER)
            LOCATE 23, 5: PRINT "Entre com novo valor:            "
            COLOR 11: LOCATE 23, 27: INPUT K1(ILAYER)
            COLOR 11: LOCATE 16, 20: PRINT K1(ILAYER)

            COLOR 11: LOCATE 18, 20: PRINT K2(ILAYER)
            LOCATE 23, 5: PRINT "Entre com novo valor:            "
            COLOR 11: LOCATE 23, 27: INPUT K2(ILAYER)
            COLOR 11: LOCATE 18, 20: PRINT K2(ILAYER)

            COLOR 11: LOCATE 20, 38: PRINT SCOMPAC(ILAYER)
            LOCATE 23, 5: PRINT "Entre com novo valor:            "
            COLOR 11: LOCATE 23, 27: INPUT SCOMPAC(ILAYER)
            COLOR 11: LOCATE 20, 38: PRINT SCOMPAC(ILAYER)

            E(ILAYER) = K1(ILAYER)
        CASE 2, 6
            COLOR 3: LOCATE 16, 5
            PRINT "Parametro K =              "
            LOCATE 18, 5
            PRINT "Parametro n =              "
            LOCATE 20, 5
            PRINT "Tensao desvio minima =         "

            COLOR 11: LOCATE 16, 19: PRINT K1(ILAYER)
            LOCATE 23, 5: PRINT "Entre com novo valor:            "
            COLOR 11: LOCATE 23, 27: INPUT K1(ILAYER)
            COLOR 11: LOCATE 16, 19: PRINT K1(ILAYER)

            COLOR 11: LOCATE 18, 19: PRINT K2(ILAYER)
            LOCATE 23, 5: PRINT "Entre com novo valor:            "
            COLOR 11: LOCATE 23, 27: INPUT K2(ILAYER)
            COLOR 11: LOCATE 18, 19: PRINT K2(ILAYER)

            COLOR 11: LOCATE 20, 28: PRINT SDCRIT(ILAYER)
            LOCATE 23, 5: PRINT "Entre com novo valor:            "
            COLOR 11: LOCATE 23, 27: INPUT SDCRIT(ILAYER)
            COLOR 11: LOCATE 20, 28: PRINT SDCRIT(ILAYER)

            E(ILAYER) = K1(ILAYER)
        CASE 3
            COLOR 3: LOCATE 21, 10
            PRINT "Modulo de Resiliencia (kgf/cm2) = "
            COLOR 11: LOCATE 21, 44: PRINT E(ILAYER)
            LOCATE 23, 5: PRINT "Entre com novo valor:            "
            COLOR 11: LOCATE 23, 27: INPUT E(ILAYER)
            COLOR 11: LOCATE 21, 44: PRINT E(ILAYER)
        CASE 4
            COLOR 3: LOCATE 21, 10
            PRINT "Modulo Dinamico (kgf/cm2)       = "
            COLOR 11: LOCATE 21, 44: PRINT E(ILAYER)
            LOCATE 23, 5: PRINT "Entre com novo valor:            "
            COLOR 11: LOCATE 23, 27: INPUT E(ILAYER)
            COLOR 11: LOCATE 21, 44: PRINT E(ILAYER)
            E(ILAYER) = INT((E(ILAYER) / 1.3) * EXP(.0486))
        CASE 5
            COLOR 3
            LOCATE 16, 5: PRINT "Parametro K1 = "
            LOCATE 17, 5: PRINT "Parametro K2 = "
            LOCATE 18, 5: PRINT "Parametro K3 = "
            LOCATE 19, 5: PRINT "Tensao desvio minima = "
            LOCATE 20, 5: PRINT "Tensao residual da compactacao = "

            COLOR 11: LOCATE 16, 20: PRINT K1(ILAYER)
            LOCATE 23, 5: PRINT "Entre com novo valor:            "
            COLOR 11: LOCATE 23, 27: INPUT K1(ILAYER)
            COLOR 11: LOCATE 16, 20: PRINT K1(ILAYER)

            COLOR 11: LOCATE 17, 20: PRINT K2(ILAYER)
            LOCATE 23, 5: PRINT "Entre com novo valor:            "
            COLOR 11: LOCATE 23, 27: INPUT K2(ILAYER)
            COLOR 11: LOCATE 17, 20: PRINT K2(ILAYER)

            COLOR 11: LOCATE 18, 20: PRINT K3(ILAYER)
            LOCATE 23, 5: PRINT "Entre com novo valor:            "
            COLOR 11: LOCATE 23, 27: INPUT K3(ILAYER)
            COLOR 11: LOCATE 18, 20: PRINT K3(ILAYER)

            COLOR 11: LOCATE 19, 28: PRINT SDCRIT(ILAYER)
            LOCATE 23, 5: PRINT "Entre com novo valor:            "
            COLOR 11: LOCATE 23, 27: INPUT SDCRIT(ILAYER)
            COLOR 11: LOCATE 19, 28: PRINT SDCRIT(ILAYER)

            COLOR 11: LOCATE 20, 38: PRINT SCOMPAC(ILAYER)
            LOCATE 23, 5: PRINT "Entre com novo valor:            "
            COLOR 11: LOCATE 23, 27: INPUT SCOMPAC(ILAYER)
            COLOR 11: LOCATE 20, 38: PRINT SCOMPAC(ILAYER)

            E(ILAYER) = K1(ILAYER)
        CASE ELSE
            BEEP: COLOR 15: LOCATE 23, 10
            PRINT "ERRO: Codigo inadmissivel."
            FOR IV = 1 TO 200000: NEXT IV
            COLOR 3: LOCATE 23, 10
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
     
     
3000 '****************************
'* SALVAR OS DADOS EDITADOS *
'*       (Subrotina)        *
'****************************
     
CLS
      
COLOR 15: LOCATE 10, 15
PRINT "Codigo de Identificacao: "
COLOR 11: LOCATE 11, 15
PRINT "(max. de 8 caracteres)"
COLOR 11: LOCATE 10, 40
INPUT ARQSALVA$
      
OPEN "ARQDAT.DAT" FOR OUTPUT AS #2
WRITE #2, ARQSALVA$
WRITE #2, ARQSALVA$ + ".txt"
CLOSE #2

OPEN ARQSALVA$ FOR OUTPUT AS #2
COLOR 4: LOCATE 23, 10
PRINT "Escrevendo no arquivo ..."
      
WRITE #2, NC, NA, NPONTOS, INCRMAX
PRINT #2, USING "##########.####"; LA; BA; PRESMAX; DT; DL
FOR I = 1 TO NC
    PRINT #2, "'"; MAT$(I); "'"
    PRINT #2, ICODE(I), USING "#############.####"; E(I); POISS(I); H(I); K1(I); K2(I); K3(I); SDCRIT(I)
    PRINT #2, NDIVV(I), USING "#############.####"; COESAO(I); FI(I); GAMA(I); SCOMPAC(I)
NEXT I
FOR I = 1 TO NPONTOS
    PRINT #2, USING "########.####"; X(I); Y(I)
NEXT I
SELECT CASE TIPOEIXO$
    CASE "ESRS": Ieixo = 1
    CASE "ESRD": Ieixo = 2
    CASE "TDM": Ieixo = 3
    CASE "TDRS": Ieixo = 4
    CASE "TDRD": Ieixo = 5
    CASE "TTRS": Ieixo = 6
    CASE "TTRD": Ieixo = 7
    CASE ELSE
        PRINT "ERRO": STOP
END SELECT
WRITE #2, Ieixo
CLOSE #2
RETURN

