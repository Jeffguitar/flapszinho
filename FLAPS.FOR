C
C    *********************************
C    *                               *
C    *   METODO DAS CAMADAS FINITAS  *
C    *               -               *
C    * Elastico linear ou nao-linear *
C    *              ---              *
C    *         PROGRAMA FLAPS        *
C    *                               *
C    *********************************
C
C             
      PROGRAM FLAPS
      INTEGER NC,NA,NPONTOS,NDIVV(50),IDOF(50,4),L(102),ICODE(50),BD
      REAL*4 E(50),POISS(50),H(50),Z(41),T(16),AI(16),ALFA(15),BETA(15),
     &       AE(50),BE(50),CE(50),DE(50),FE(50),U(82),P(102),K(4,4),
     &       KG(102,103),UZ(50),URO(50),D1(51,60),D2(51,60),D3(51,60),
     &       ALFAI(16),BETAJ(16),TT(8),AII(8),X(10),Y(10),EX(50,60),
     &       EY(50,60),EZ(50,60),EXY(50,60),EZX(50,60),EZY(50,60),
     &       D(6,6),SDCRIT(15),COESAO(15),FI(15),GM,PI,RO,RRO,EPS(6),
     &       SIGMA(6),SX(50,60),SY(50,60),SZ(50,60),SXY(50,60),
     &       SZX(50,60),SZY(50,60),K1(15),K2(15),K3(15),TETA(50,60),
     &       K0,SIG13(50,60),ESUB(50),POISUB(50),TET(50),S13(50),LA,
     &       MULTX,MULTY,AALFA,BBETA,T1,T2,T3,TMAX,PRESSAO,PRESMAX,DT,
     &       DL,BA,PASSO,TJ,TI,EA,EC,ED,EF,DZ,FZ,G,SOMA,ZZ,AIJ,AIG,
     &       UY1,UY2,UX1,UX2,UXM,UYM,UZM,EXX,EYY,EZZ,GXY,GZX,GZY,CSA,
     &       CSB,SNA,SNB,GAMA(15),GEOSTAT(50),XX(60),YY(60),COES(50),
     &       ANG(50),UD(50),SCOMPAC(15),EAV(50)
      CHARACTER*12 ARQDADOS, ARQSAIDA, MAT(15) 
C
C
C
C *******************************************
C *                                         *
C *  DIMENSIONAMENTO DE VETORES E MATRIZES  *
C *                                         *
C *******************************************
C
C  Camadas de materiais diferentes = 15
C  Subcamadas                      = 50
C  Planos nodais                   = 51
C  Graus de liberdade              = 102
C  Pontos x,y                      = 10
C
C
C
      DO 6900 I = 1, 3
      WRITE(*,6000)
 6000 FORMAT(1X)
 6900 CONTINUE
      WRITE(*,6100)
 6100 FORMAT(10X,'* * * * * * * * * * * * * * * * * * * * * * * * * *')
      WRITE(*,6200)
 6200 FORMAT(10X,'*                    F L A P S                    *')
      WRITE(*,6300)
 6300 FORMAT(10X,'*  Finite Layer Analysis of Pavement Structures   *')
      WRITE(*,6400)
 6400 FORMAT(10X,'*                   Version 3.0                   *')
      WRITE(*,6500)
 6500 FORMAT(10X,'* * * * * * * * * * * * * * * * * * * * * * * * * *')
      WRITE(*,6000)
C
      OPEN(5,FILE='ARQDAT.DAT',STATUS='OLD')
      READ(5,*)ARQDADOS
      READ(5,*)ARQSAIDA
      ENDFILE 5
      CLOSE(5,STATUS='KEEP')
C
C
C
C **********************************************
C    LEITURA E IMPRESSAO DOS DADOS DE ENTRADA
C **********************************************
C
C  NC        = Numero de camadas elasticas (1 a 10)
C  NA        = Numero de areas carregadas retangulares
C  NPONTOS   = Numero de pontos (x,y) onde se deseja os resultados (1 a 6)
C  INCRMAX   = Numero de incrementos em que a carga sera aplicada (1 a 4)
C  LA,BA     = Comprimento e largura da area carregada
C  PRESMAX   = Pressao aplicada
C  DT        = Distancia de separacao entre as areas (centro a centro, eixo x)
C  DL        = Distancia de separacao entre as areas de carga (eixo y)
C  E(I)      = Modulo de elasticidade (inicial) da camada I
C  POISS(I)  = Coeficiente de Poisson da camada I
C  H(I)      = Espessura da camada I
C  ICODE(I)  = Codigo do material da camada I:
C                                              1 ==> Granular
C                                              2 ==> Coesivo
C                                              3 ==> Linear
C                                              4 ==> E = f(Epsilon)
C  K1,K2(I)  = Modelo Mr=f(Sd) para solos finos coesivos, ou
C            = Modelo Mr=f(teta) para materiais granulares
C  K1,K2,K3  = Modelo Geral Mr=f(teta,Sd) para quaisquer solos
C  X(I),Y(I) = Pontos onde se deseja os resultados
C
      OPEN(5,FILE=ARQDADOS,STATUS='OLD')
      OPEN(6,FILE=ARQSAIDA,STATUS='NEW')
C
      READ(5,*)NC,NA,NPONTOS,INCRMAX
C 1000 FORMAT(4I5)
      WRITE(6,2000)
 2000 FORMAT(1X,'      ARQUIVO DE DADOS  ',/)
      WRITE(6,2001)NC,NA,NPONTOS,INCRMAX
 2001 FORMAT(1X,4I5)
      READ(5,*)LA,BA,PRESMAX,DT,DL
C 1001 FORMAT(5F10.3)
      WRITE(6,2002)LA,BA,PRESMAX,DT,DL
 2002 FORMAT(1X,5F10.3)
C
      DO 5 I=1,NC
      READ(5,*)MAT(I)
      READ(5,*)ICODE(I),E(I),POISS(I),H(I),K1(I),K2(I),K3(I),SDCRIT(I)
C 1002 FORMAT(I5,7F10.3)
      READ(5,*)NDIVV(I),COESAO(I),FI(I),GAMA(I),SCOMPAC(I)
C 1004 FORMAT(I5,2F10.3,2F10.5)
      WRITE(6,*)MAT(I)
      WRITE(6,2003)ICODE(I),E(I),POISS(I),H(I),K1(I),K2(I),K3(I),
     &SDCRIT(I)
 2003 FORMAT(1X,I5,7F10.3)
      WRITE(6,2005)NDIVV(I),COESAO(I),FI(I),GAMA(I),SCOMPAC(I)
 2005 FORMAT(1X,I5,2F10.3,2F10.5)
    5 CONTINUE
      DO 7 I=1,NPONTOS
         READ(5,*)X(I),Y(I)
C 1003 FORMAT(2F10.3)
    7 CONTINUE
C
      DO 9 I=1,NPONTOS
         WRITE(6,2004)X(I),Y(I)
 2004    FORMAT(1X,2F10.3)
    9 CONTINUE
C
      READ(5,*)Ieixo
C
      NPTGLOBAL=NPONTOS
      NPONS=1
C - Caso de uma carga de roda simples
      IF (Ieixo.EQ.1)
     &THEN
           DO 4100 I=1,NPONTOS
              XX(I)=X(I)
              YY(I)=Y(I)
 4100      CONTINUE
           GOTO 1500
      ENDIF
C - Caso de Eixo Simples de Rodas Duplas
      IF (Ieixo.EQ.2)
     &THEN
               ITT=1
               DO 4000 I=1,NPONTOS
                  XX(ITT)=X(I)+(DT/2)
                  YY(ITT)=Y(I)
                  XX(ITT+1)=X(I)-(DT/2)
                  YY(ITT+1)=Y(I)
                  ITT=ITT+NA
 4000          CONTINUE
               NPONTOS=NA*NPONTOS
               GOTO 1500
      ENDIF     
C - Caso de Eixo Tandem Duplo de Rodas Simples
      IF (Ieixo.EQ.4)
     &THEN
              ITT=1
              DO 4001 I=1,NPONTOS
                 XX(ITT)=X(I)
                 YY(ITT)=Y(I)-(DL/2)
                 XX(ITT+1)=X(I)
                 YY(ITT+1)=Y(I)+(DL/2)
                 ITT=ITT+NA
 4001         CONTINUE
              NPONTOS=NA*NPONTOS
              GOTO 1500
      ENDIF
C - Caso de tandem-duplo misto
      IF (Ieixo.EQ.3)
     &THEN
                ITT=1
                DO 4201 I=1,NPONTOS
                   XX(ITT)=X(I)+(DT/2)
                   YY(ITT)=Y(I)+(DL/2)
                   XX(ITT+1)=X(I)+(DT/2)
                   YY(ITT+1)=Y(I)-(DL/2)
                   XX(ITT+2)=X(I)-(DT/2)
                   YY(ITT+2)=Y(I)-(DL/2)
                   ITT=ITT+NA
 4201           CONTINUE
                NPONTOS=NA*NPONTOS
                GOTO 1500
      ENDIF     
C - Caso de tandem-triplo de rodas simples
      IF (Ieixo.EQ.6)
     &THEN
                ITT=1
                DO 4301 I=1,NPONTOS
                   XX(ITT)=X(I)
                   YY(ITT)=Y(I)
                   XX(ITT+1)=X(I)
                   YY(ITT+1)=Y(I)+DL
                   XX(ITT+2)=X(I)
                   YY(ITT+2)=Y(I)-DL
                   ITT=ITT+NA
 4301           CONTINUE
                NPONTOS=NA*NPONTOS
                GOTO 1500
      ENDIF
C - Caso de tandem-duplo de rodas duplas
      IF (Ieixo.EQ.5)
     &THEN
           ITT=1
           DO 4200 I=1,NPONTOS
              XX(ITT)=X(I)+(DT/2)
              YY(ITT)=Y(I)+(DL/2)
              XX(ITT+1)=X(I)-(DT/2)
              YY(ITT+1)=Y(I)+(DL/2)
              XX(ITT+2)=X(I)+(DT/2)
              YY(ITT+2)=Y(I)-(DL/2)
              XX(ITT+3)=X(I)-(DT/2)
              YY(ITT+3)=Y(I)-(DL/2)
              ITT=ITT+NA
 4200      CONTINUE
           NPONTOS=NA*NPONTOS
           GOTO 1500
      ENDIF
C - Caso de tandem-triplo de rodas duplas
      IF (Ieixo.EQ.7)
     &THEN
           ITT=1
           DO 4300 I=1,NPONTOS
              XX(ITT)=X(I)+(DT/2)
              YY(ITT)=Y(I)+DL
              XX(ITT+1)=X(I)-(DT/2)
              YY(ITT+1)=Y(I)+DL
              XX(ITT+2)=X(I)+(DT/2)
              YY(ITT+2)=Y(I)
              XX(ITT+3)=X(I)-(DT/2)
              YY(ITT+3)=Y(I)
              XX(ITT+4)=X(I)+(DT/2)
              YY(ITT+4)=Y(I)-DL
              XX(ITT+5)=X(I)-(DT/2)
              YY(ITT+5)=Y(I)-DL
              ITT=ITT+NA
 4300      CONTINUE
           NPONTOS=NA*NPONTOS
           GOTO 1500
      ENDIF
C
      WRITE(6,4597)
 4597 FORMAT(1X,'ERRO: NUMERO DE CARGAS ADOTADO EH INADMISSIVEL')
      GOTO 7777
C
C
C
C ************************
C    PARAMETROS FIXADOS
C ************************
C
C  NDIVSL          = Numero de subdivisoes do subleito, em camadas com
C                    apenas uma subcamada;
C  H(NC) a H(NC+6) = Espessuras das camadas do subleito;
C  BD              = Base rigida e': (0) Lisa ou (1) Aspera
C
 1500 BD=0
      SUBGRADE=H(NC)
      IF (SUBGRADE.LE.50.0)
     &THEN
            NDIVSL=5
            PASSOSL=SUBGRADE/NDIVSL
            H(NC)=PASSOSL
            H(NC+1)=PASSOSL
            H(NC+2)=PASSOSL
            H(NC+3)=PASSOSL
            H(NC+4)=PASSOSL
      ENDIF
      IF ((SUBGRADE.GT.50.0).AND.(SUBGRADE.LE.100.0))
     &THEN
            NDIVSL=8
            PASSOSL=SUBGRADE/NDIVSL
            IF (PASSOSL.LT.8.0)
     &      THEN
                  H(NC)=PASSOSL
                  H(NC+1)=1.5*PASSOSL
            ELSE
                  H(NC)=8.0
                  H(NC+1)=12.0
            ENDIF
            RESTO=SUBGRADE-H(NC)-H(NC+1)
            NOVOPASSO=RESTO/(NDIVSL-2)
            H(NC+2)=NOVOPASSO
            H(NC+3)=NOVOPASSO
            H(NC+4)=NOVOPASSO
            H(NC+5)=NOVOPASSO
            H(NC+6)=NOVOPASSO
            H(NC+7)=NOVOPASSO
      ENDIF
      IF ((SUBGRADE.GT.100.0).AND.(SUBGRADE.LE.300.0))
     &THEN
            NDIVSL=8
            PASSOSL=SUBGRADE/NDIVSL
            H(NC)=8.0
            H(NC+1)=12.0
            H(NC+2)=20.0
            RESTO=SUBGRADE-H(NC)-H(NC+1)-H(NC+2)
            NOVOPASSO=RESTO/(NDIVSL-3)
            H(NC+3)=NOVOPASSO
            H(NC+4)=NOVOPASSO
            H(NC+5)=NOVOPASSO
            H(NC+6)=NOVOPASSO
            H(NC+7)=NOVOPASSO
      ENDIF
      IF ((SUBGRADE.GT.300.0).AND.(SUBGRADE.LE.700.0))
     &THEN
            NDIVSL=8
            PASSOSL=SUBGRADE/NDIVSL
            H(NC)=10.0
            H(NC+1)=20.0
            H(NC+2)=50.0
            RESTO=SUBGRADE-H(NC)-H(NC+1)-H(NC+2)
            NOVOPASSO=RESTO/(NDIVSL-3)
            H(NC+3)=NOVOPASSO
            H(NC+4)=NOVOPASSO
            H(NC+5)=NOVOPASSO
            H(NC+6)=NOVOPASSO
            H(NC+7)=NOVOPASSO
      ENDIF
      IF (SUBGRADE.GT.700.0)
     &THEN
            NDIVSL=8
            PASSOSL=SUBGRADE/NDIVSL
            H(NC)=10.0
            H(NC+1)=30.0
            H(NC+2)=100.0
            RESTO=SUBGRADE-H(NC)-H(NC+1)-H(NC+2)
            NOVOPASSO=RESTO/(NDIVSL-3)
            H(NC+3)=NOVOPASSO
            H(NC+4)=NOVOPASSO
            H(NC+5)=NOVOPASSO
            H(NC+6)=NOVOPASSO
            H(NC+7)=NOVOPASSO
      ENDIF
      NCC=NC+(NDIVSL-1)
      NC1=NC+1
C
C ************************************************
C   DEFINICAO DOS PLANOS NODAIS E DAS SUBCAMADAS
C ************************************************
      PASSO=0.0
      NDI=0
      NC0=NC-1
      DO 30 I=NC,NCC
         NDIVV(I)=1
         GAMA(I)=GAMA(NC)
   30 CONTINUE
      NSUBCAM=0
      IPRF=1
      Z(1)=0.0
      DO 40 I=1,NCC
         NSUBCAM=NSUBCAM+NDIVV(I)
         PASSO=H(I)/NDIVV(I)
         NDI=NDIVV(I)+1
         DO 50 J=1,NDI
            IF(IPRF.EQ.1)GOTO 45
            IF(IPRF.GT.(NSUBCAM+1))GOTO 50
            Z(IPRF)=Z(IPRF-1)+PASSO
   45       IPRF=IPRF+1
   50    CONTINUE
   40 CONTINUE
      NDF=2*(NSUBCAM+1)
      WRITE(6,2400)NSUBCAM,NDF
 2400 FORMAT(1X,'Numero de Subcamadas = ',I5,'        Numero de Graus de
     $ Liberdade = ',I5)
C
C **************************
C    TENSOES GEOSTATICAS
C  (para o caso nao-linear)
C **************************
C
      IF (INCRMAX.EQ.1) GOTO 3000
      SIGMAV=0.0
      ISUB=1
      DO 3100 I=1,NCC
         PASSO=H(I)/NDIVV(I)
         DO 3200 J=1,NDIVV(I)
            SIGMAV=SIGMAV+GAMA(I)*PASSO/2
            K0=POISS(I)/(1-POISS(I))
            GEOSTAT(ISUB)=(1+2*K0)*SIGMAV
            SIGMAV=SIGMAV+GAMA(I)*PASSO/2
            ISUB=ISUB+1
 3200    CONTINUE
 3100 CONTINUE
C
C
C
C ****************************************
C    PONTOS PARA A INTEGRACAO GAUSSIANA
C ****************************************
C
C - 16 PONTOS
 3000 NGAUSS = 16
      T(1) = 9.501250983763744E-02
      AI(1) = .1894506104550685
      T(3) = .2816035507792589
      AI(3) = .1826034150449236
      T(5) = .4580167776572274
      AI(5) = .1691565193950025
      T(7) = .6178762444026438
      AI(7) = .1495959888165767
      T(9) = .755404408355003
      AI(9) = .1246289712555339
      T(11) = .8656312023878318
      AI(11) = 9.515851168249279E-02
      T(13) = .9445750230732326
      AI(13) = 6.225352393864789E-02
      T(15) = .9894009349916499
      AI(15) = .0271524594117541
      DO 7773 I = 2,NGAUSS,2
         T(I) = -T(I - 1)
         AI(I) = AI(I - 1)
 7773 CONTINUE
C
C - 8 PONTOS
      TT(1)=.96028986
      TT(2)=-TT(1)
      TT(3)=.79666648
      TT(4)=-TT(3)
      TT(5)=.52553242
      TT(6)=-TT(5)
      TT(7)=.18343464
      TT(8)=-TT(7)
C
      AII(1)=.10122854
      AII(2)=AII(1)
      AII(3)=.22238104
      AII(4)=AII(3)
      AII(5)=.31370664
      AII(6)=AII(5)
      AII(7)=.36268378
      AII(8)=AII(7)
C
      PI=3.141592654
C
C - UMA AREA CARREGADA
C
      ITER=7
      KMAX=7
      ALFA(1)=0.0
      ALFA(2)=2*0.1*PI/LA
      ALFA(3)=2*1*PI/LA
      ALFA(4)=2*2*PI/LA
      ALFA(5)=2*4*PI/LA
      ALFA(6)=2*6*PI/LA
      ALFA(7)=2*8*PI/LA
      ALFA(8)=2*10*PI/LA
C
      KMAY=KMAX
      BETA(1)=0.0
      BETA(2)=2*0.1*PI/BA
      BETA(3)=2*1*PI/BA
      BETA(4)=2*2*PI/BA
      BETA(5)=2*4*PI/BA
      BETA(6)=2*6*PI/BA
      BETA(7)=2*8*PI/BA
      BETA(8)=2*10*PI/BA
      GOTO 100
C
  100 ICC=1
C
C *****************************************
C  NUMERACAO GLOBAL DOS GRAUS DE LIBERDADE
C *****************************************
C
      DO 110 ISUB=1,NSUBCAM
         IDOF(ISUB,1)=ICC
         IDOF(ISUB,2)=ICC+2
         IDOF(ISUB,3)=ICC+1
         IDOF(ISUB,4)=ICC+3
         ICC=ICC+2
  110 CONTINUE
C
C ******************************************
C  INICIALIZACAO DOS MODULOS DAS SUBCAMADAS
C ******************************************
C
      I=1
      DO 120 IL=1,NC
      IF (IL.LT.NC)
     $THEN
          NNDIV=NDIVV(IL)
      ELSE
          NNDIV=NDIVSL
      ENDIF
      DO 130 IJ=1,NNDIV
         ESUB(I)=E(IL)
         POISUB(I)=POISS(IL)
         COES(I)=COESAO(IL)
         ANG(I)=FI(IL)*PI/180
         I=I+1
  130 CONTINUE
  120 CONTINUE
C
C
C
C **************************
C     ITERACOES DE CARGA
C **************************
C
      DO 140 INCR=1,INCRMAX
         WRITE(*,7948)INCR
 7948    FORMAT(3X,'Iteracao No.:',I3)
C
C
C
C ************************************************
C     MATRIZ DE ELASTICIDADE DE CADA SUBCAMADA
C ************************************************
C
      I=1
      DO 150 IL=1,NC
      IF (IL.LT.NC)
     $THEN
          NNDIV=NDIVV(IL)
      ELSE
          NNDIV=NDIVSL
      ENDIF
      DO 160 IJ=1,NNDIV
         AE(I)=(ESUB(I)*(1-POISUB(I)))/((1+POISUB(I))*(1-2*POISUB(I)))
         BE(I)=AE(I)*(POISUB(I)/(1-POISUB(I)))
         CE(I)=AE(I)*(POISUB(I)/(1-POISUB(I)))
         DE(I)=AE(I)
         FE(I)=AE(I)*((1-2*POISUB(I))/(1-POISUB(I)))
         I=I+1
  160 CONTINUE
  150 CONTINUE
C
C ********************************************
C     INICIALIZACAO DE VETORES E MATRIZES
C ********************************************
C
      IZ=1
      DO 170 ISUB=1,NSUBCAM
         DO 180 IP=1,NPONTOS
            D1(IZ,IP)=0.
            D1(IZ+1,IP)=0.
            D2(IZ,IP)=0.
            D2(IZ+1,IP)=0.
            D3(IZ,IP)=0.
            D3(IZ+1,IP)=0.
            EX(ISUB,IP)=0.
            EY(ISUB,IP)=0.
            EZ(ISUB,IP)=0.
            EXY(ISUB,IP)=0.
            EZX(ISUB,IP)=0.
            EZY(ISUB,IP)=0.
  180    CONTINUE
         IZ=IZ+1
  170 CONTINUE
C
C *************************************************
C    COMBINACOES DE PONTOS AMOSTRAIS (ALFA,BETA)
C *************************************************
C
      DO 190 LL=1,KMAY
      IF (LL.LE.ITER)
     $THEN
          IY=16
      ELSE
          IY=8
      ENDIF
      DO 200 J=1,IY
       IF (LL.LE.ITER)
     $ THEN
            TJ=T(J)
       ELSE
            TJ=TT(J)
       ENDIF
       BETAJ(J)=.5*(BETA(LL+1)-BETA(LL))*TJ+.5*(BETA(LL)+BETA(LL+1))
       DO 210 KK=1,KMAX
        IF (KK.LE.ITER)
     $  THEN
             IX=16
        ELSE
             IX=8
        ENDIF
        DO 220 I=1,IX
         IF (KK.LE.ITER)
     $   THEN
              TI=T(I)
         ELSE
              TI=TT(I)
         ENDIF
         ALFAI(I)=.5*(ALFA(KK+1)-ALFA(KK))*TI+.5*(ALFA(KK)+ALFA(KK+1))
         AALFA=ALFAI(I)
         BBETA=BETAJ(J)
         RRO=(AALFA**2)+(BBETA**2)
         RO=SQRT(RRO)
C
C ----------------------------------------------------------------------
C  INICIALIZACAO DE VETORES E MATRIZES APOS A MUDANCA DE PONTO AMOSTRAL
C ----------------------------------------------------------------------
C
      NDF1=NDF+1
      DO 230 IKK=1,NDF
         U(IKK)=0.0
         P(IKK)=0.0
  230 CONTINUE
      DO 240 IKK=1,NDF
         DO 250 JKK=1,NDF1
            KG(IKK,JKK)=0.0
  250    CONTINUE
  240 CONTINUE
C
C
C
C ************************************
C       MATRIZ DE RIGIDEZ GLOBAL
C ************************************
C
      DO 300 ISUB=1,NSUBCAM
C
C - Matriz de Rigidez da Subcamada
      EA=AE(ISUB)
      EC=CE(ISUB)
      ED=DE(ISUB)
      EF=FE(ISUB)
      DZ=Z(ISUB+1)-Z(ISUB)
      K(1,1)=(EA*RRO*DZ/4)+(EF/(2*DZ))
      K(2,1)=(EA*RRO*DZ/4)-(EF/(2*DZ))
      K(3,1)=(-RO*EC/2)+(RO*EF/4)
      K(4,1)=(RO*EC/2)+(RO*EF/4)
      K(1,2)=K(2,1)
      K(2,2)=(EA*RRO*DZ/4)+(EF/(2*DZ))
      K(3,2)=(-RO*EC/2)-(RO*EF/4)
      K(4,2)=(RO*EC/2)-(RO*EF/4)
      K(1,3)=K(3,1)
      K(2,3)=K(3,2)
      K(3,3)=(EF*RRO*DZ/8)+(ED/DZ)
      K(4,3)=(EF*RRO*DZ/8)-(ED/DZ)
      K(1,4)=K(4,1)
      K(2,4)=K(4,2)
      K(3,4)=K(4,3)
      K(4,4)=(EF*RRO*DZ/8)+(ED/DZ)
      DO 310 II=1,4
      IG=IDOF(ISUB,II)
      DO 320 JJ=II,4
      IV=IDOF(ISUB,JJ)
      KG(IG,IV)=KG(IG,IV)+K(II,JJ)
      KG(IV,IG)=KG(IG,IV)
  320 CONTINUE
  310 CONTINUE
  300 CONTINUE
C
C
C
C ********************************************************
C     TRANSFORMADA DE FOURIER DO CARREGAMENTO APLICADO
C ********************************************************
C
C     PRESSAO=PRESMAX*SIN((INCR*PI)/(2*INCRMAX))
      PRESSAO=PRESMAX
      FZ=(PRESSAO/(PI*PI*AALFA*BBETA))*SIN(AALFA*LA/2)*
     &SIN(BBETA*BA/2)
C
C
C
C *********************************************
C    CONDICOES DE FRONTEIRA E VETOR DE CARGA
C *********************************************
C
      GM=1.E+30
      KG(NDF,NDF)=KG(NDF,NDF)+GM
      IF (BD.EQ.1) KG(NDF-1,NDF-1)=KG(NDF-1,NDF-1)+GM
      P(2)=FZ
C
C
C
C ***************************************************
C      RESOLUCAO DO SISTEMA DE EQUACOES LINEARES
C             METODO DA REDUCAO DE CROUT
C ***************************************************
C
      N=NDF
      NB=N+1
      DO 400 II=1,N
         KG(II,NB)=P(II)
  400 CONTINUE
      DO 410 JJ=1,NB
         II=1
  430    G=KG(II,JJ)
         IF(G.EQ.0.)GOTO 420
         L(JJ)=II
         GOTO 410
  420    II=II+1
         GOTO 430
  410 CONTINUE
C -  Reducao da Matriz de Coeficientes e do Vetor de Carga
      DO 440 JJ=2,NB
         L1=L(JJ)+1
         IF(L1.GT.JJ)GOTO 440
         IF(JJ.EQ.NB)JM=N
         IF(JJ.NE.NB)JM=JJ
         DO 450 II=L1,JM
            IF(L(JJ).GE.L(II))L2=L(JJ)
            IF(L(JJ).LT.L(II))L2=L(II)
            I2=II-1
            IF(L2.GT.I2)GOTO 450
            DO 460 MM=L2,I2
               IF(KG(MM,MM).EQ.0.)WRITE(6,2100)MM
 2100          FORMAT(1X,'RIGIDEZ NULA NO GRAU DE LIBERDADE ',I5)
               KG(II,JJ)=KG(II,JJ)-KG(MM,II)*(KG(MM,JJ)/KG(MM,MM))
  460       CONTINUE
  450    CONTINUE
  440 CONTINUE
C - Retro-substituicao
      KG(N,NB)=KG(N,NB)/KG(N,N)
      II=N-1
  520 SOMA=0.
      I11=II+1
      DO 500 JJ=I11,N
         SOMA=SOMA+KG(II,JJ)*KG(JJ,NB)
  500 CONTINUE
      KG(II,NB)=(KG(II,NB)-SOMA)/KG(II,II)
      II=II-1
      IF(II.EQ.0)GOTO 510
      IF(II.GT.0)GOTO 520
  510 DO 530 II=1,NDF
         U(II)=KG(II,NB)
  530 CONTINUE
C
C
C
C ***************************************************
C    DESLOCAMENTOS TRANSFORMADOS DOS PLANOS NODAIS
C ***************************************************
C
      NSUM1=NSUBCAM+1
      DO 550 IZ=1,NSUM1
         URO(IZ)=U(2*IZ-1)
         UZ(IZ)=U(2*IZ)
  550 CONTINUE
C
C
C
C ******************************************************************
C  CALCULO DOS DESLOCAMENTOS NOS PLANOS NODAIS PARA OS PONTOS (X,Y)
C   SELECIONADOS, POR INVERSAO NUMERICA DA TRANSFORMADA DE FOURIER
C ******************************************************************
C
      IF (LL.LE.ITER)
     $THEN
          AIJ=AI(J)
      ELSE
          AIJ=AII(J)
      ENDIF
      IF (KK.LE.ITER)
     $THEN
          AIG=AI(I)
      ELSE
          AIG=AII(I)
      ENDIF
      MULTX=(ALFA(KK+1)-ALFA(KK))*AIG
      MULTY=(BETA(LL+1)-BETA(LL))*AIJ
      DO 570 IZ=1,NSUM1
         DO 580 IP=1,NPONTOS
            D3(IZ,IP)=D3(IZ,IP)+(MULTX*MULTY*COS(BBETA*YY(IP))*UZ(IZ)*
     $      COS(AALFA*XX(IP)))
            D2(IZ,IP)=D2(IZ,IP)+(MULTX*MULTY*URO(IZ)*COS(AALFA*XX(IP))*
     $      SIN(BBETA*YY(IP))/RO)
            D1(IZ,IP)=D1(IZ,IP)+(MULTX*MULTY*URO(IZ)*SIN(AALFA*XX(IP))*
     $      COS(BBETA*YY(IP))/RO)
  580    CONTINUE
  570 CONTINUE
C
C ****************************
C  DEFORMACOES NAS SUBCAMADAS
C ****************************
C
      IZ=1
      DO 600 ISUB=1,NSUBCAM
         UY1=(BBETA/RO)*URO(IZ)
         UY2=(BBETA/RO)*URO(IZ+1)
         UX1=(AALFA/RO)*URO(IZ)
         UX2=(AALFA/RO)*URO(IZ+1)
         UXM=(UX1+UX2)/2
         UYM=(UY1+UY2)/2
         UZM=(UZ(IZ)+UZ(IZ+1))/2
         EXX=-AALFA*UXM
         EYY=-BBETA*UYM
         DZ=Z(IZ+1)-Z(IZ)
         EZZ=-(UZ(IZ+1)-UZ(IZ))/DZ
         GXY=-BBETA*UXM-AALFA*UYM
         GZX=-((UX2-UX1)/DZ)+AALFA*UZM
         GZY=-((UY2-UY1)/DZ)+BBETA*UZM
         IZ=IZ+1
         DO 610 IP=1,NPONTOS
            CSA=COS(AALFA*XX(IP))
            CSB=COS(BBETA*YY(IP))
            SNA=SIN(AALFA*XX(IP))
            SNB=SIN(BBETA*YY(IP))
            EX(ISUB,IP)=EX(ISUB,IP)+(MULTX*MULTY*CSB*CSA*EXX)
            EY(ISUB,IP)=EY(ISUB,IP)+(MULTX*MULTY*CSA*CSB*EYY)
            EZ(ISUB,IP)=EZ(ISUB,IP)+(MULTX*MULTY*CSA*CSB*EZZ)
            EXY(ISUB,IP)=EXY(ISUB,IP)+(-MULTX*MULTY*SNA*SNB*GXY)
            EZX(ISUB,IP)=EZX(ISUB,IP)+(MULTX*MULTY*SNA*CSB*GZX)
            EZY(ISUB,IP)=EZY(ISUB,IP)+(MULTX*MULTY*CSA*SNB*GZY)
  610    CONTINUE
  600 CONTINUE
C
C *******************************
C    MUDANCA DE PONTO AMOSTRAL
C *******************************
C
  220 CONTINUE
  210 CONTINUE
  200 CONTINUE
  190 CONTINUE
C
C
C
C ****************************************
C    CALCULO DAS TENSOES NAS SUBCAMADAS
C ****************************************
C
      DO 650 ISUB=1,NSUBCAM
C -      Matriz de Elasticidade da Subcamada
         D(1,1)=AE(ISUB)
         D(1,2)=BE(ISUB)
         D(1,3)=CE(ISUB)
         D(1,4)=0.
         D(1,5)=0.
         D(1,6)=0.
         D(2,2)=AE(ISUB)
         D(2,3)=CE(ISUB)
         D(2,4)=0.
         D(2,5)=0.
         D(2,6)=0.
         D(3,3)=DE(ISUB)
         D(3,4)=0.
         D(3,5)=0.
         D(3,6)=0.
         D(4,4)=(AE(ISUB)-BE(ISUB))/2
         D(4,5)=0.
         D(4,6)=0.
         D(5,5)=FE(ISUB)/2
         D(5,6)=0.
         D(6,6)=FE(ISUB)/2
         DO 660 I=1,6
            DO 670 J=I,6
               D(J,I)=D(I,J)
  670       CONTINUE
  660    CONTINUE
C - Vetor de Deformacoes Especificas
      DO 680 IP=1,NPONTOS
         EPS(1)=EX(ISUB,IP)
         EPS(2)=EY(ISUB,IP)
         EPS(3)=EZ(ISUB,IP)
         EPS(4)=EXY(ISUB,IP)
         EPS(5)=EZX(ISUB,IP)
         EPS(6)=EZY(ISUB,IP)
C - Produto SIGMA = D * EPS
      DO 690 I=1,6
         SOMA=0.
         DO 700 J=1,6
            SOMA=SOMA+D(I,J)*EPS(J)
  700    CONTINUE
         SIGMA(I)=SOMA
  690 CONTINUE
      SX(ISUB,IP)=SIGMA(1)
      SY(ISUB,IP)=SIGMA(2)
      SZ(ISUB,IP)=SIGMA(3)
      SXY(ISUB,IP)=SIGMA(4)
      SZX(ISUB,IP)=SIGMA(5)
      SZY(ISUB,IP)=SIGMA(6)
  680 CONTINUE
  650 CONTINUE
C
C
C
C ****************************
C   SUPERPOSICAO DOS EFEITOS
C ****************************
C
      IZ=1
      IL=1
      ICONT=1
      DO 9000 ISUB=1,NSUBCAM
         TET(ISUB)=0.0
         S13(ISUB)=0.0
         EAV(ISUB)=0.0
         IP1=1
         IF ((ICONT.GT.NDIVV(IL)).AND.(IL.LT.NC))
     &   THEN
               IL=IL+1
               ICONT=1
         END IF
         DO 9100 IPP=1,NPTGLOBAL
            IPW=IP1
            EXSOMA=0.0
            EYSOMA=0.0
            EZSOMA=0.0
            EXYSOMA=0.0
            EZXSOMA=0.0
            EZYSOMA=0.0
            SXSOMA=0.0
            SYSOMA=0.0
            SZSOMA=0.0
            SXYSOMA=0.0
            SZXSOMA=0.0
            SZYSOMA=0.0
            D3SOMA=0.0
            D2SOMA=0.0
            D1SOMA=0.0
            DO 9110 INA=1,NA
               EXSOMA=EXSOMA+EX(ISUB,IPW)
               EYSOMA=EYSOMA+EY(ISUB,IPW)
               EZSOMA=EZSOMA+EZ(ISUB,IPW)
               SXSOMA=SXSOMA+SX(ISUB,IPW)
               SYSOMA=SYSOMA+SY(ISUB,IPW)
               SZSOMA=SZSOMA+SZ(ISUB,IPW)
               SXYSOMA=SXYSOMA+SXY(ISUB,IPW)
               SZXSOMA=SZXSOMA+SZX(ISUB,IPW)
               SZYSOMA=SZYSOMA+SZY(ISUB,IPW)
               EXYSOMA=EXYSOMA+EXY(ISUB,IPW)
               EZXSOMA=EZXSOMA+EZX(ISUB,IPW)
               EZYSOMA=EZYSOMA+EZY(ISUB,IPW)
               D3SOMA=D3SOMA+D3(IZ,IPW)
               D2SOMA=D2SOMA+D2(IZ,IPW)
               D1SOMA=D1SOMA+D1(IZ,IPW)
               IPW=IPW+1
 9110       CONTINUE
            EX(ISUB,IPP)=EXSOMA
            EY(ISUB,IPP)=EYSOMA
            EZ(ISUB,IPP)=EZSOMA
            SX(ISUB,IPP)=SXSOMA
            SY(ISUB,IPP)=SYSOMA
            SZ(ISUB,IPP)=SZSOMA
            SXY(ISUB,IPP)=SXYSOMA
            SZX(ISUB,IPP)=SZXSOMA
            SZY(ISUB,IPP)=SZYSOMA
            EXY(ISUB,IPP)=EXYSOMA
            EZX(ISUB,IPP)=EZXSOMA
            EZY(ISUB,IPP)=EZYSOMA
            D3(IZ,IPP)=D3SOMA
            D2(IZ,IPP)=D2SOMA
            D1(IZ,IPP)=D1SOMA
C
      IF (MAT(IL).EQ.'GRA')
     &THEN
C          Tensoes Horizontais Residuais da Compactacao,
C          em Camadas Granulares
           SXCOMPAC=SCOMPAC(IL)
           SYCOMPAC=SXCOMPAC
           SXTOTAL=SXCOMPAC+SX(ISUB,IPP)
           SYTOTAL=SYCOMPAC+SY(ISUB,IPP)
           TETA(ISUB,IPP)=SXTOTAL+SYTOTAL+SZ(ISUB,IPP)
           SIG13(ISUB,IPP)=SZ(ISUB,IPP)-SXTOTAL
           IF (SIG13(ISUB,IPP).LT.0.0) SIG13(ISUB,IPP)=0.0
      ELSE
           TETA(ISUB,IPP)=SX(ISUB,IPP)+SY(ISUB,IPP)+SZ(ISUB,IPP)
           T1=(SXY(ISUB,IPP)**2)+(((SX(ISUB,IPP)-SY(ISUB,IPP))/2)**2)
           T1=SQRT(T1)
           T2=(SZX(ISUB,IPP)**2)+(((SZ(ISUB,IPP)-SX(ISUB,IPP))/2)**2)
           T2=SQRT(T2)
           T3=(SZY(ISUB,IPP)**2)+(((SZ(ISUB,IPP)-SY(ISUB,IPP))/2)**2)
           T3=SQRT(T3)
           TMAX=T1
           IF (T2.GT.TMAX)
     &     THEN
                TMAX=T2
           ENDIF
           IF (T3.GT.TMAX)
     &     THEN
                TMAX=T3
           ENDIF
           SIG13(ISUB,IPP)=2*TMAX
      END IF
      IP1=IP1+NA
 9100 CONTINUE
C - Consideracao para a tensao media na subcamada
      TET(ISUB)=TETA(ISUB,1)
      S13(ISUB)=SIG13(ISUB,1)
      EAV(ISUB)=EZ(ISUB,1)
C
      IZ=IZ+1
      ICONT=ICONT+1
 9000 CONTINUE
C
C
C
      IF (INCRMAX.EQ.1) GOTO 140
C
C
C
C ***************************************
C    ATUALIZACAO DOS MODULOS ELASTICOS
C           (Caso Nao-linear)
C ***************************************
C
C - Calculo das Tensoes Medias nas Subcamadas
C
C      IF (NPONS.EQ.1)
C     $THEN
C           DO 777 ISUB=1,NSUBCAM
C     Verificacao de ruptura
C           T2=(SZX(ISUB,NPONTOS)**2)+(((SZ(ISUB,NPONTOS)-
C     $        SX(ISUB,NPONTOS))/2)**2)
C           T2=SQRT(T2)
C           T3=(SZY(ISUB,NPONTOS)**2)+(((SZ(ISUB,NPONTOS)-
C     $        SY(ISUB,NPONTOS))/2)**2)
C           T3=SQRT(T3)
C           IF (T2.GT.T3)
C     &     THEN
C                TMAX=T2
C                SS1=SZ(ISUB,NPONTOS)
C                SS3=SX(ISUB,NPONTOS)
C           ELSE
C                TMAX=T3
C                SS1=SZ(ISUB,NPONTOS)
C                SS3=SY(ISUB,NPONTOS)
C           ENDIF
C           SMED=(SS1+SS3)/2
C           TF=COES(ISUB)+SMED*TAN(ANG(ISUB))
C          Em caso de ruptura, ajuste das tensoes
C           IF (TMAX.GE.TF)
C     &     THEN
C                SS3=-2*COES(ISUB)*COS(ANG(ISUB))
C                SS3=SS3+SS1*(1-SIN(ANG(ISUB)))
C                SS3=SS3/(1+SIN(ANG(ISUB)))
C                TETA(ISUB,NPONTOS)=SS1+2*SS3
C           ENDIF
C           TET(ISUB)=TETA(ISUB,NPONTOS)+GEOSTAT(ISUB)
C           S13(ISUB)=SIG13(ISUB,NPONTOS)
C  777      CONTINUE
C           GO TO 7766
C      ENDIF
C      DO 705 ISUB=1,NSUBCAM
C      SOMA1=0.0
C      SOMA2=0.0
C      NP1=NPONS-1
C      DO 710 IP=1,NP1
C      MED=(TETA(ISUB,IP)+TETA(ISUB,(IP+1)))/2
C      DIST=X(IP+1)-X(IP)
C      SOMA1=SOMA1+(DIST*MED)
C
C      MED=(SIG13(ISUB,IP)+SIG13(ISUB,(IP+1)))/2
C      SOMA2=SOMA2+(DIST*MED)
C  710 CONTINUE
C      TET(ISUB)=(SOMA1/(X(NPONS)-X(1)))+GEOSTAT(ISUB)
C      S13(ISUB)=SOMA2/(X(NPONS)-X(1))
C  705 CONTINUE
C
C - Substituicao nos Modelos de Nao-linearidade
 7766 ISUB=1
      DO 720 IL=1,NC
      IF (IL.LT.NC)
     $THEN
           NNDIV=NDIVV(IL)
      ELSE
           NNDIV=NDIVSL
      ENDIF
      DO 730 IJ=1,NNDIV
         IF(ICODE(IL).EQ.6)GOTO 790
         IF(ICODE(IL).EQ.5)GOTO 780
         IF(ICODE(IL).EQ.4)GOTO 760
         IF(ICODE(IL).EQ.3)GOTO 740
         IF(ICODE(IL).EQ.2)GOTO 750
         IF(ICODE(IL).EQ.1)GOTO 770
         WRITE(*,5685)
 5685 FORMAT(1X,'ERRO: CODIGO DE MATERIAL NAO IDENTIFICADO')
      GOTO 7777
C - Codigo 6
  790 IF (TET(ISUB).LT.0.1) TET(ISUB)=0.1
      IF (S13(ISUB).LT.SDCRIT(IL)) S13(ISUB)=SDCRIT(IL)
      ESUB(ISUB)=K1(IL)*((TET(ISUB)/S13(ISUB))**K2(IL))
      IF (ESUB(ISUB).LT.50.0) ESUB(ISUB)=50.0
      GOTO 740
C
C - Codigo 5
  780 IF (TET(ISUB).LT.0.1) TET(ISUB)=0.1
      IF (EAV(ISUB).LT.0.0)
     &THEN
          ESUB(ISUB)=50.0
      ELSE
          IF (EAV(ISUB).LT.1.0E-6) EAV(ISUB)=1.0E-6
          ESUB(ISUB)=K1(IL)*(TET(ISUB)**K2(IL))*(EAV(ISUB)**K3(IL))
          IF (ESUB(ISUB).LT.50.0) ESUB(ISUB)=50.0
          IF (ESUB(ISUB).GT.7000.0) ESUB(ISUB)=7000.0
      ENDIF
      GOTO 740
C
C - Codigo 1
  770 IF (TET(ISUB).LT.0.1) TET(ISUB)=0.1
      ESUB(ISUB)=K1(IL)*(TET(ISUB)**K2(IL))
      IF (EAV(ISUB).GT.1.0E-4) 
     &THEN 
           ESUB(ISUB)=ESUB(ISUB)*1.3*EXP(-0.0486*EAV(ISUB)/1.0E-4)
      ELSE
           ESUB(ISUB)=ESUB(ISUB)*1.238
      ENDIF
      IF (ESUB(ISUB).LT.50.0) ESUB(ISUB)=50.0
      GOTO 740
C
C - Codigo 4
  760 IF (EAV(ISUB).GT.1.0E-4) 
     &THEN 
           ESUB(ISUB)=E(IL)*1.3*EXP(-0.0486*EAV(ISUB)/1.0E-4)
      ELSE
           ESUB(ISUB)=E(IL)*1.238
      ENDIF
      IF (ESUB(ISUB).LT.50.0) ESUB(ISUB)=50.0
      GOTO 740
C
C - Codigo 2
  750 IF (S13(ISUB).LT.SDCRIT(IL)) S13(ISUB)=SDCRIT(IL)
      ESUB(ISUB)=K1(IL)*(S13(ISUB)**K2(IL))
      IF (ESUB(ISUB).LT.50.0) ESUB(ISUB)=50.0
      GOTO 740
C
C - Modelo de Fossberg (desativado)
C     IF (S13(ISUB).LT.SDCRIT(IL)) S13(ISUB)=SDCRIT(IL)
C     UMIDADE=UMID1+UMID2*PROFUNDIDADE
C     ESUB(ISUB)=(SL1+SL2*UMIDADE)*(1-(S13(ISUB)/(SL3+SL4*UMIDADE)))
C     GOTO 740
C
  740 ISUB=ISUB+1
  730 CONTINUE
  720 CONTINUE
C
C
C
C - Mudanca de Incremento de Carga
  140 CONTINUE
C
C
C
C ******************************
C    IMPRESSAO DOS RESULTADOS
C ******************************
C
      NPONTOS=NPONTOS/NA
      WRITE(6,2105)
 2105 FORMAT(//////)
      WRITE(6,2110)
 2110 FORMAT(1X,'  P R O G R A M A   F L A P S  ',////)
      WRITE(6,2120)
 2120 FORMAT(1X,' * * * *  D E S L O C A M E N T O S   * * * * ',///)
      WRITE(6,2130)
 2130 FORMAT(1X,'      X         Y         Z                Ux',
     $'        Uy        Uz ',//)
      DO 800 IP=1,NPONTOS
      NSUB1=NSUBCAM+1
      DO 810 IZ=1,NSUB1
      WRITE(6,2140)X(IP),Y(IP),Z(IZ),D1(IZ,IP),D2(IZ,IP),D3(IZ,IP)
 2140 FORMAT(1X,3F10.3,9X,3E10.4)
  810 CONTINUE
  800 CONTINUE
C
C
C
      WRITE(6,6145)
 6145 FORMAT(1X,////)
      WRITE(6,6150)
 6150 FORMAT(1X,'D E N S I D A D E   D E   E N E R G I A')
      WRITE(6,6151)
 6151 FORMAT(1X,'        D E   D I S T O R C A O        ',///)
      WRITE(6,6160)
 6160 FORMAT(1X,'      X         Y         Z                Ud',//)
C
      DO 900 IP=1,NPONTOS
C
      DO 910 ISUB=1,NSUBCAM
      DED=((SX(ISUB,IP)-SY(ISUB,IP))**2)+((SY(ISUB,IP)-SZ(ISUB,IP))**2)
      DED=DED+((SZ(ISUB,IP)-SX(ISUB,IP))**2)
      DED=((1+POISUB(ISUB))/(6*ESUB(ISUB)))*DED
      G=ESUB(ISUB)/(2*(1+POISUB(ISUB)))
      DED=DED+((SXY(ISUB,IP)**2)+(SZX(ISUB,IP)**2)+(SZY(ISUB,IP)**2))
     &/(2*G)
      UD(ISUB)=DED
  910 CONTINUE
C
      NCC=NC+(NDIVSL-1)
      ISUB=1
      IZ=1
      DO 925 I=1,NCC
      DO 930 IJJ=1,NDIVV(I)
      IF ((IJJ.EQ.1).AND.(I.LE.NC))
     &THEN
             WRITE(6,2169)I
 2169        FORMAT(1X,'Camada No.: ',I3)
             ZZ1=(Z(IZ)+Z(IZ+1))/2
             ZZ2=(Z(IZ+1)+Z(IZ+2))/2
             UD1=(ZZ2*UD(ISUB)-ZZ1*UD(ISUB+1))/(ZZ2-ZZ1)
             UD1=UD1+((UD(ISUB+1)-UD(ISUB))*Z(IZ))/(ZZ2-ZZ1)
             IF (UD1.LT.0.0) UD1=0.0
             WRITE(6,6170)X(IP),Y(IP),Z(IZ),UD1
      ENDIF
      ZZ=(Z(IZ)+Z(IZ+1))/2
      WRITE(6,6170)X(IP),Y(IP),ZZ,UD(ISUB)
 6170 FORMAT(1X,3F10.3,9X,E12.6)
      IF ((IJJ.EQ.NDIVV(I)).AND.(I.LT.NC))
     &THEN
             ZZ1=(Z(IZ)+Z(IZ+1))/2
             ZZ2=(Z(IZ-1)+Z(IZ))/2
             UD1=(ZZ2*UD(ISUB)-ZZ1*UD(ISUB-1))/(ZZ2-ZZ1)
             UD1=UD1+((UD(ISUB-1)-UD(ISUB))*Z(IZ+1))/(ZZ2-ZZ1)
             IF (UD1.LT.0.0) UD1=0.0
             WRITE(6,6170)X(IP),Y(IP),Z(IZ+1),UD1
      ENDIF
      IZ=IZ+1
      ISUB=ISUB+1
  930 CONTINUE
  925 CONTINUE
  900 CONTINUE
C
C
C
      WRITE(6,2145)
 2145 FORMAT(1X,////)
      WRITE(6,2150)
 2150 FORMAT(1X,' * *  T E N S O E S   N O R M A I S  * * ',///)
      WRITE(6,2160)
 2160 FORMAT(1X,'      X         Y         Z                Sx',
     $'        Sy        Sz',//)
      DO 820 IP=1,NPONTOS
      NCC=NC+(NDIVSL-1)
      ISUB=1
      IZ=1
      DO 825 I=1,NCC
      DO 830 IJJ=1,NDIVV(I)
      IF ((IJJ.EQ.1).AND.(I.LE.NC))
     &THEN
             WRITE(6,2169)I
             ZZ1=(Z(IZ)+Z(IZ+1))/2
             ZZ2=(Z(IZ+1)+Z(IZ+2))/2
             SZ1=(ZZ2*SZ(ISUB,IP)-ZZ1*SZ(ISUB+1,IP))/(ZZ2-ZZ1)
             SZ1=SZ1+((SZ(ISUB+1,IP)-SZ(ISUB,IP))*Z(IZ))/(ZZ2-ZZ1)
             SY1=(ZZ2*SY(ISUB,IP)-ZZ1*SY(ISUB+1,IP))/(ZZ2-ZZ1)
             SY1=SY1+((SY(ISUB+1,IP)-SY(ISUB,IP))*Z(IZ))/(ZZ2-ZZ1)
             SX1=(ZZ2*SX(ISUB,IP)-ZZ1*SX(ISUB+1,IP))/(ZZ2-ZZ1)
             SX1=SX1+((SX(ISUB+1,IP)-SX(ISUB,IP))*Z(IZ))/(ZZ2-ZZ1)
             WRITE(6,2170)X(IP),Y(IP),Z(IZ),SX1,SY1,SZ1
      ENDIF
      ZZ=(Z(IZ)+Z(IZ+1))/2
      WRITE(6,2170)X(IP),Y(IP),ZZ,SX(ISUB,IP),SY(ISUB,IP),SZ(ISUB,IP)
 2170 FORMAT(1X,3F10.3,9X,3E10.4)
      IF ((IJJ.EQ.NDIVV(I)).AND.(I.LT.NC))
     &THEN
             ZZ1=(Z(IZ)+Z(IZ+1))/2
             ZZ2=(Z(IZ-1)+Z(IZ))/2
             SZ1=(ZZ2*SZ(ISUB,IP)-ZZ1*SZ(ISUB-1,IP))/(ZZ2-ZZ1)
             SZ1=SZ1+((SZ(ISUB-1,IP)-SZ(ISUB,IP))*Z(IZ+1))/(ZZ2-ZZ1)
             SY1=(ZZ2*SY(ISUB,IP)-ZZ1*SY(ISUB-1,IP))/(ZZ2-ZZ1)
             SY1=SY1+((SY(ISUB-1,IP)-SY(ISUB,IP))*Z(IZ+1))/(ZZ2-ZZ1)
             SX1=(ZZ2*SX(ISUB,IP)-ZZ1*SX(ISUB-1,IP))/(ZZ2-ZZ1)
             SX1=SX1+((SX(ISUB-1,IP)-SX(ISUB,IP))*Z(IZ+1))/(ZZ2-ZZ1)
             WRITE(6,2170)X(IP),Y(IP),Z(IZ+1),SX1,SY1,SZ1
      ENDIF
      IZ=IZ+1
      ISUB=ISUB+1
  830 CONTINUE
  825 CONTINUE
  820 CONTINUE
C
C
C
      WRITE(6,2180)
 2180 FORMAT(1X,////)
      WRITE(6,2190)
 2190 FORMAT(1X,' * *  T E N S O E S  C I S A L H A N T E S  * * ',///)
      WRITE(6,2200)
 2200 FORMAT(1X,'      X         Y         Z               Txy',
     $'        Tzx       Tzy',//)
      DO 840 IP=1,NPONTOS
      NCC=NC+(NDIVSL-1)
      ISUB=1
      IZ=1
      DO 845 I=1,NCC
      DO 850 IJJ=1,NDIVV(I)
      IF ((IJJ.EQ.1).AND.(I.LE.NC))
     &THEN
             WRITE(6,2169)I
             ZZ1=(Z(IZ)+Z(IZ+1))/2
             ZZ2=(Z(IZ+1)+Z(IZ+2))/2
             SXY1=(ZZ2*SXY(ISUB,IP)-ZZ1*SXY(ISUB+1,IP))/(ZZ2-ZZ1)
             SXY1=SXY1+((SXY(ISUB+1,IP)-SXY(ISUB,IP))*Z(IZ))/(ZZ2-ZZ1)
             SZX1=(ZZ2*SZX(ISUB,IP)-ZZ1*SZX(ISUB+1,IP))/(ZZ2-ZZ1)
             SZX1=SZX1+((SZX(ISUB+1,IP)-SZX(ISUB,IP))*Z(IZ))/(ZZ2-ZZ1)
             SZY1=(ZZ2*SZY(ISUB,IP)-ZZ1*SZY(ISUB+1,IP))/(ZZ2-ZZ1)
             SZY1=SZY1+((SZY(ISUB+1,IP)-SZY(ISUB,IP))*Z(IZ))/(ZZ2-ZZ1)
             WRITE(6,2210)X(IP),Y(IP),Z(IZ),SXY1,SZX1,SZY1
      ENDIF
      ZZ=(Z(IZ)+Z(IZ+1))/2
      WRITE(6,2210)X(IP),Y(IP),ZZ,SXY(ISUB,IP),SZX(ISUB,IP)
     $,SZY(ISUB,IP)
 2210 FORMAT(1X,3F10.3,9X,3E10.4)
      IF ((IJJ.EQ.NDIVV(I)).AND.(I.LT.NC))
     &THEN
             ZZ1=(Z(IZ)+Z(IZ+1))/2
             ZZ2=(Z(IZ-1)+Z(IZ))/2
             SXY1=(ZZ2*SXY(ISUB,IP)-ZZ1*SXY(ISUB-1,IP))/(ZZ2-ZZ1)
             SXY1=SXY1+((SXY(ISUB-1,IP)-SXY(ISUB,IP))*Z(IZ+1))
     &/(ZZ2-ZZ1)
             SZX1=(ZZ2*SZX(ISUB,IP)-ZZ1*SZX(ISUB-1,IP))/(ZZ2-ZZ1)
             SZX1=SZX1+((SZX(ISUB-1,IP)-SZX(ISUB,IP))*Z(IZ+1))
     &/(ZZ2-ZZ1)
             SZY1=(ZZ2*SZY(ISUB,IP)-ZZ1*SZY(ISUB-1,IP))/(ZZ2-ZZ1)
             SZY1=SZY1+((SZY(ISUB-1,IP)-SZY(ISUB,IP))*Z(IZ+1))
     &/(ZZ2-ZZ1)
             WRITE(6,2210)X(IP),Y(IP),Z(IZ+1),SXY1,SZX1,SZY1
      ENDIF
      IZ=IZ+1
      ISUB=ISUB+1
  850 CONTINUE
  845 CONTINUE
  840 CONTINUE
C
C
C
      WRITE(6,2220)
 2220 FORMAT(1X,////)
      WRITE(6,2230)
 2230 FORMAT(1X,' * *  D E F O R M A C O E S   N O R M A I S  * * '
     $,///)
      WRITE(6,2240)
 2240 FORMAT(1X,'      X         Y         Z               Ex',
     $'         Ey        Ez',//)
      DO 860 IP=1,NPONTOS
      NCC=NC+(NDIVSL-1)
      ISUB=1
      IZ=1
      DO 865 I=1,NCC
      DO 870 IJJ=1,NDIVV(I)
      IF ((IJJ.EQ.1).AND.(I.LE.NC))
     &THEN
             WRITE(6,2169)I
             ZZ1=(Z(IZ)+Z(IZ+1))/2
             ZZ2=(Z(IZ+1)+Z(IZ+2))/2
             EZ1=(ZZ2*EZ(ISUB,IP)-ZZ1*EZ(ISUB+1,IP))/(ZZ2-ZZ1)
             EZ1=EZ1+((EZ(ISUB+1,IP)-EZ(ISUB,IP))*Z(IZ))/(ZZ2-ZZ1)
             EY1=(ZZ2*EY(ISUB,IP)-ZZ1*EY(ISUB+1,IP))/(ZZ2-ZZ1)
             EY1=EY1+((EY(ISUB+1,IP)-EY(ISUB,IP))*Z(IZ))/(ZZ2-ZZ1)
             EX1=(ZZ2*EX(ISUB,IP)-ZZ1*EX(ISUB+1,IP))/(ZZ2-ZZ1)
             EX1=EX1+((EX(ISUB+1,IP)-EX(ISUB,IP))*Z(IZ))/(ZZ2-ZZ1)
             WRITE(6,2250)X(IP),Y(IP),Z(IZ),EX1,EY1,EZ1
      ENDIF
      ZZ=(Z(IZ)+Z(IZ+1))/2
      WRITE(6,2250)X(IP),Y(IP),ZZ,EX(ISUB,IP),EY(ISUB,IP),EZ(ISUB,IP)
 2250 FORMAT(1X,3F10.3,9X,3E10.4)
      IF ((IJJ.EQ.NDIVV(I)).AND.(I.LT.NC))
     &THEN
             ZZ1=(Z(IZ)+Z(IZ+1))/2
             ZZ2=(Z(IZ-1)+Z(IZ))/2
             EZ1=(ZZ2*EZ(ISUB,IP)-ZZ1*EZ(ISUB-1,IP))/(ZZ2-ZZ1)
             EZ1=EZ1+((EZ(ISUB-1,IP)-EZ(ISUB,IP))*Z(IZ+1))/(ZZ2-ZZ1)
             EY1=(ZZ2*EY(ISUB,IP)-ZZ1*EY(ISUB-1,IP))/(ZZ2-ZZ1)
             EY1=EY1+((EY(ISUB-1,IP)-EY(ISUB,IP))*Z(IZ+1))/(ZZ2-ZZ1)
             EX1=(ZZ2*EX(ISUB,IP)-ZZ1*EX(ISUB-1,IP))/(ZZ2-ZZ1)
             EX1=EX1+((EX(ISUB-1,IP)-EX(ISUB,IP))*Z(IZ+1))/(ZZ2-ZZ1)
             WRITE(6,2250)X(IP),Y(IP),Z(IZ+1),EX1,EY1,EZ1
      ENDIF
      IZ=IZ+1
      ISUB=ISUB+1
  870 CONTINUE
  865 CONTINUE
  860 CONTINUE
C
C
C
      WRITE(6,2260)
 2260 FORMAT(1X,////)
      WRITE(6,2270)
 2270 FORMAT(1X,' * *  D E F O R M A C O E S   C I S A L H A N T E S
     $ * * ',///)
      WRITE(6,2280)
 2280 FORMAT(1X,'      X         Y         Z               Exy',
     $'        Ezx       Ezy',//)
      DO 880 IP=1,NPONTOS
      NCC=NC+(NDIVSL-1)
      ISUB=1
      IZ=1
      DO 885 I=1,NCC
      DO 890 IJJ=1,NDIVV(I)
      IF ((IJJ.EQ.1).AND.(I.LE.NC))
     &THEN
             WRITE(6,2169)I
             ZZ1=(Z(IZ)+Z(IZ+1))/2
             ZZ2=(Z(IZ+1)+Z(IZ+2))/2
             EXY1=(ZZ2*EXY(ISUB,IP)-ZZ1*EXY(ISUB+1,IP))/(ZZ2-ZZ1)
             EXY1=EXY1+((EXY(ISUB+1,IP)-EXY(ISUB,IP))*Z(IZ))/(ZZ2-ZZ1)
             EZX1=(ZZ2*EZX(ISUB,IP)-ZZ1*EZX(ISUB+1,IP))/(ZZ2-ZZ1)
             EZX1=EZX1+((EZX(ISUB+1,IP)-EZX(ISUB,IP))*Z(IZ))/(ZZ2-ZZ1)
             EZY1=(ZZ2*EZY(ISUB,IP)-ZZ1*EZY(ISUB+1,IP))/(ZZ2-ZZ1)
             EZY1=EZY1+((EZY(ISUB+1,IP)-EZY(ISUB,IP))*Z(IZ))/(ZZ2-ZZ1)
             WRITE(6,2290)X(IP),Y(IP),Z(IZ),EXY1,EZX1,EZY1
      ENDIF
      ZZ=(Z(IZ)+Z(IZ+1))/2
      WRITE(6,2290)X(IP),Y(IP),ZZ,EXY(ISUB,IP),EZX(ISUB,IP),
     &EZY(ISUB,IP)
 2290 FORMAT(1X,3F10.3,9X,3E10.4)
      IF ((IJJ.EQ.NDIVV(I)).AND.(I.LT.NC))
     &THEN
             ZZ1=(Z(IZ)+Z(IZ+1))/2
             ZZ2=(Z(IZ-1)+Z(IZ))/2
             EXY1=(ZZ2*EXY(ISUB,IP)-ZZ1*EXY(ISUB-1,IP))/(ZZ2-ZZ1)
             EXY1=EXY1+((EXY(ISUB-1,IP)-EXY(ISUB,IP))*Z(IZ+1))
     &/(ZZ2-ZZ1)
             EZX1=(ZZ2*EZX(ISUB,IP)-ZZ1*EZX(ISUB-1,IP))/(ZZ2-ZZ1)
             EZX1=EZX1+((EZX(ISUB-1,IP)-EZX(ISUB,IP))*Z(IZ+1))
     &/(ZZ2-ZZ1)
             EZY1=(ZZ2*EZY(ISUB,IP)-ZZ1*EZY(ISUB-1,IP))/(ZZ2-ZZ1)
             EZY1=EZY1+((EZY(ISUB-1,IP)-EZY(ISUB,IP))*Z(IZ+1))
     &/(ZZ2-ZZ1)
             WRITE(6,2290)X(IP),Y(IP),Z(IZ+1),EXY1,EZX1,EZY1
      ENDIF
      IZ=IZ+1
      ISUB=ISUB+1
  890 CONTINUE
  885 CONTINUE
  880 CONTINUE
C
C
C
      WRITE(6,2300)
 2300 FORMAT(1X,////)
      WRITE(6,2310)
 2310 FORMAT(1X,' * *  M O D U L O S   E L A S T I C O S  * * ',///)
      WRITE(6,2320)
 2320 FORMAT(1X,'      Z              E',//)
      IZ=1
      DO 999 ISUB=1,NSUBCAM
      ZZ=(Z(IZ)+Z(IZ+1))/2
      WRITE(6,2330)ZZ,ESUB(ISUB)
 2330 FORMAT(1X,F10.3,5X,F10.0)
      IZ=IZ+1
  999 CONTINUE
      ENDFILE 6
      CLOSE(6,STATUS='KEEP')
C
C
C
C *****************
C    FINALIZACAO
C *****************
 7777 STOP
      END
