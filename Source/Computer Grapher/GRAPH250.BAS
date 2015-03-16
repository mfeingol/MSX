10 ' ллллллллллллллллллллллллллллллллллллллллл
20 ' лл     COMPUTER GRAPHER  VER. 2.50     лл
30 ' лл    (c) ILLUMINATUS SOFTWARE 1993    лл
40 ' лл PROGRAMACION: ALTAIR (MAX FEINGOLD) лл
50 ' ллллллллллллллллллллллллллллллллллллллллл
60 '    La funciЂn est  en la lЁnea 1110
65 '    Este programa esta hecho para ejecutarse bajo el KUN-Basic (call run)
70 ' ллллллллллллллллллллллллллл
80 ' лл CONFIGURACION INICIAL лл
90 ' ллллллллллллллллллллллллллл
100 DIM A (511) ' RX
110 SC=7:RX=512:RY=212:CF=15:CE=4
120 PI=3.1415926#
130 E =2.7182818#
140 SCREEN 0:KEY OFF
150 INPUT "ЈC lculo autom tico de los valores extremos de la Y (S/N)";A$
160 IF A$="S" OR A$="s" THEN A=1:B=2:GOTO 260
170 IF A$="N" OR A$="n" THEN A=2:GOTO 190
180 LOCATE 0,0:CLS:GOTO 150
190 PRINT:INPUT "ЈRepresentaciЂn simulada en 3 dimensiones (S/N)";B$
200 IF B$="S" OR B$="s" THEN B=1:GOTO 260
210 IF B$="N" OR B$="n" THEN B=2:GOTO 260
220 LOCATE 0,2:FOR I=1 TO 80:PRINT" ";:NEXT I:LOCATE 0,1:GOTO 190
230 ' ллллллллллллллллллллллллллллллллллллллллллллл
240 ' лл INTRODUCIR VALORES EXTREMOS DE X Y DE Y лл
250 ' ллллллллллллллллллллллллллллллллллллллллллллл
260 IF I=0 THEN PRINT
270 INPUT "Valor mЁnimo de la X:";X1
280 INPUT "Valor m ximo de la X:";X2
290 IF (X1 => X2) THEN PRINT:PRINT "VALORES NO VALIDOS":FOR I=1 TO 3000:NEXT I:CLS:GOTO 260
300 IF A=1 THEN 370
310 INPUT "Valor mЁnimo de la Y:";MI
320 INPUT "Valor m ximo de la Y:";MA
330 IF (MI => MA) THEN PRINT:PRINT "VALORES NO VALIDOS":FOR I=1 TO 3000:NEXT I:CLS:GOTO 260 ELSE SWAP MA,MI:MA=-MA:MI=-MI:GOTO 540
340 ' лллллллллллллллллллллллллл
350 ' лл CONFIGURAR VARIABLES лл
360 ' лллллллллллллллллллллллллл
370 I=0:MA=-65535!:MI=65535!:ST=(X2-X1)/RX
380 LOCATE 0,5:PRINT"Porcentaje:"
390 ' ллллллллллллллллллллллллллллллллллллллл
400 ' лл CALCULAR VALORES EXTREMOS DE LA Y лл
410 ' ллллллллллллллллллллллллллллллллллллллл
420 FOR X=X1 TO X2 STEP ST
430   GOSUB 1110 ' Leer datos de la funciЂn
440   IF Y=1.701E+38 OR Y=-1.701E+38 THEN 470
450   IF Y<MI THEN MI=Y ' Nuevo mЁnimo
460   IF Y>MA THEN MA=Y ' Nuevo m ximo
470   A(I)=Y ' Hacer tabla de consulta con valores de Y
480   I=I+1 ' Contador
490   LOCATE 12,5:PRINT INT(I*100/RX);"%" ' Porcentaje calculado
500 NEXT X
510 ' лллллллллллллллллллллллллллллллллллллл
520 ' лл CALCULAR ESCALA Y DESPLAZAMIENTO лл
530 ' лллллллллллллллллллллллллллллллллллллл
540 XS=RX/(X2-X1):YS=RY/(MA-MI) ' Escala de X y de Y
550 XO=-X1*XS:YO=-MI*YS ' Desplazamiento del origen de coordenadas
560 ' ллллллллллллллллллллллллллллллллл
570 ' лл CONFIGURAR PANTALLA GRAFICA лл
580 ' ллллллллллллллллллллллллллллллллл
590 SCREEN SC:COLOR CF,0,0:CLS
600 COLOR=(CF,7,7,7):COLOR=(CE,0,0,0)
610 ' лллллллллллллллллллллллллллллллллллллллллллллллллллллл
620 ' лл DIBUJAR EJES SEGUN LA ESCALA Y EL DESPLAZAMIENTO лл
630 ' лллллллллллллллллллллллллллллллллллллллллллллллллллллл
640 IF XO<=RX-1 AND XO=>0 THEN LINE(XO,0)-(XO,RY-1),CE ELSE 680 ' Eje de la Y
650 IF MA-MI<RY-1 THEN FOR Y=YO TO 0 STEP -YS:LINE(XO-2,Y)-(XO+2,Y),CE:NEXT Y:FOR Y=YO TO RY-1 STEP YS:LINE(XO+2,Y)-(XO-2,Y),CE:NEXT Y ELSE 680 ' Marcas
660 IF MI-INT(MI)=0 THEN LINE (XO-2,0)-(XO+2,0),CE
670 IF MA-INT(MA)=0 THEN LINE (XO-2,RY-1)-(XO+2,RY-1),CE
680 IF YO=>0 AND YO<=RY+1 THEN LINE(0,YO)-(RX-1,YO),CE ELSE 750  ' Eje de la X
690 IF X2-X1<RX-1 THEN FOR X=XO TO 0 STEP -XS:LINE (X,YO-2)-(X,YO+2),CE:NEXT X:FOR X=XO TO RX-1 STEP XS:LINE (X,YO-2)-(X,YO+2),CE:NEXT X ELSE 750 ' Marcas
700 IF X1-INT(X1)=0 THEN LINE (0,YO-2)-(0,YO+2),CE
710 IF X2-INT(X2)=0 THEN LINE (RX-1,YO-2)-(RX-1,YO+2),CE
720 ' ллллллллллллллллллллллллл
730 ' лл HACER APARECER EJES лл
740 ' ллллллллллллллллллллллллл
750 FOR I=1 TO 7:COLOR=(CE,0,0,I):FOR N=1 TO 160:NEXT N,I
760 ' ллллллллллллллллллллллллллллллллллллллллллллллл
770 ' лл COLOCAR CURSOR GRAFICO EN EL PRIMER PUNTO лл
780 ' ллллллллллллллллллллллллллллллллллллллллллллллл
790 Z=1:IF A=2 THEN GOTO 1170 ' Extremos de Y introducidos anteriormente
800 I=0:ST=(X2-X1)/RX
810 YY=A(I)*YS+YO:PSET(0,YY),CF ' Calcular primer punto de funciЂn
820 ' лллллллллллллллллллллллллллл
830 ' лл REPRESENTAR LA FUNCION лл
840 ' лллллллллллллллллллллллллллл
850 FOR X=X1 TO X2 STEP ST
860   XX=I:YY=A(I)*YS+YO ' Calcular coordenadas en pantalla gr fica
870   IF A(I)=1.701E+38 THEN LINE-(XX,211),CF:Z=0:I=I+1:GOTO 920
880   IF A(I)=-1.701E+38 THEN LINE-(XX,0),CF:Z=0:I=I+1:GOTO 920
890   I=I+1 ' Contador
900   IF Z=0 THEN PSET(XX,YY),CF:Z=1:GOTO 920
910   LINE-(XX,YY),CF ' Dibujar funciЂn
920 NEXT X
930 ' ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
940 ' лл ESPERAR A QUE SE PULSE UNA TECLA, PONER EXTREMOS Y ACABAR лл
950 ' ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
960 IF INKEY$="" THEN 960
970 FOR I=6 TO 0 STEP -1
980   IF B=2 THEN COLOR=(CE,0,0,I)
990   COLOR=(CF,I,I,I)
1000   FOR N=1 TO 160:NEXT N
1010 NEXT I
1020 SCREEN 0:COLOR CF,0,0:COLOR=(CF,7,7,7)
1030 PRINT"X mЁnimo: ";X1
1040 PRINT"X m ximo: ";X2
1050 PRINT"Y mЁnimo: ";-MA;:IF B=1 THEN PRINT "(aprox.)" ELSE PRINT
1060 PRINT"Y m ximo: ";-MI;:IF B=1 THEN PRINT "(aprox.)"
1070 END
1080 ' лллллллллллллллллллллллллл
1090 ' лл AQUI ESTA LA FUNCION лл
1100 ' лллллллллллллллллллллллллл
1110 'Y=ABS(X)*INT(X)
1111 Y=SIN(X) * COS(X)
1120 Y=-Y ' Invertir sistema de coordenadas
1130 RETURN
1140 ' лллллллллллллллллллллллллллллллллллллллллллллллллллллллл
1150 ' лл REPRESENTAR FUNCION CON EXTREMOS DE Y INTRODUCIDOS лл
1160 ' лллллллллллллллллллллллллллллллллллллллллллллллллллллллл
1170 I=0:MA=-65535!:MI=65535!:ST=(X2-X1)/RX:Z=0
1171 'Computar punto anterior a la pantalla
1172 X=X1-1:GOSUB 1110:YY=Y*YS+YO
1173 IF YY<0 THEN LAST=1:YY=0 ELSE IF YY>RY-1 THEN LAST=2:YY=RY-1 ELSE LAST=3
1174 PSET(0,YY),POINT(0,YY)
1179 FOR X = X1 TO X2 STEP ST
1180   IF I>RX-1 THEN GOTO 1280 'Se acabo la computacion
1190   GOSUB 1110 ' Leer datos de la funciЂn
1200   XX=I:YY=Y*YS+YO ' Calcular coordenadas en pantalla gr fica
1206   IF YY<0 THEN CURRENT=1 ELSE IF YY>RY-1 THEN CURRENT=2 ELSE CURRENT=0
1250   IF Y<MI THEN MI=Y ' Valor mЁnimo de la Y
1260   IF Y>MA THEN MA=Y ' Valor maximo de la Y
1264   'Dibujar la funcion
1265   IF CURRENT=0 THEN IF I<>0 THEN LINE-(XX,YY),CF ELSE PSET(XX,YY),CF
1266   IF CURRENT=1 THEN IF LAST=0 THEN LINE-(XX,0),CF ELSE                            PSET(XX,0),POINT(XX,0)
1267   IF CURRENT=2 THEN IF LAST=0 THEN LINE-(XX,RY-1),CF ELSE                         PSET(XX,RY-1),POINT(XX,RY-1)
1268   A(I)=YY ' Hacer tabla de consulta con estas coordenadas
1269   I=I+1 ' Contador
1270   LAST=CURRENT
1280 NEXT X
1290 IF B=1 THEN 1330 ELSE 960 ' Hacer 3D simulada o finalizar
1300 ' лллллллллллллллллллллллллллллллллллллллллллл
1310 ' лл REPRESENTAR EN 3 DIMENSIONES SIMULADAS лл
1320 ' лллллллллллллллллллллллллллллллллллллллллллл
1330 IF INKEY$="" THEN 1330
1340 PSET(0,A(0)),POINT(0,A(0))
1350 G=INT(RY/9)+1:Q=INT(RX/54)+1:I=0:Z=0:ZZ=0:R=0
1360 FOR X=0 TO RX-1
1370   Y=A(X) ' Usar tabla de consulta
1380   IF Y>RY OR Y<0 THEN IF Z=1 THEN LINE-(X,Y),CF:Z=0:GOTO 1420 ELSE                PSET(X,Y),POINT(X,Y):GOTO 1420 ELSE Z=1 ' Arregla discontinuidades
1390   LINE-(X,Y-G),CF ' Dibujar funciЂn
1400   IF ZZ=0 THEN LINE(X,Y-G)-(X,Y+G-1),CF:LINE(X,Y+G-1)-(X,Y-G),CF
1410   R=R+1:IF R MOD Q=0 THEN ZZ=0 ELSE ZZ=1 ' Dibujar rayas verticales
1420 NEXT X
1430 FOR I=-G+G/4 TO G STEP G/4
1440   Z=0
1450   IF I=0 THEN 1520
1460   PSET(0,A(0)),POINT(0,A(0))
1470   FOR X=0 TO RX-1
1480     Y=A(X)
1490     IF Y>RY OR Y<0 THEN R=0:IF Z=1 THEN LINE-(X,Y):Z=0:GOTO 1510 ELSE               PSET(X,Y),POINT(X,Y):GOTO 1510 ELSE Z=1
1500     LINE-(X,Y+I),CF
1510   NEXT X
1520 NEXT I
1530 ' лллллллллллллллллллллллллллллллллллллллл
1540 ' лл HACER DESAPARECER EJES Y FINALIZAR лл
1550 ' лллллллллллллллллллллллллллллллллллллллл
1560 IF INKEY$="" THEN 1560
1570 FOR I=6 TO 0 STEP -1
1580   COLOR=(CE,0,0,I)
1590   FOR N=1 TO 160:NEXT N
1600 NEXT I
1610 GOTO 960
