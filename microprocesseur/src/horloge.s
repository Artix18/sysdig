#MACRO
RS(n, X) {
  RSE X;
  SUBi 1, n;
  MTZi -2;
}

MUL201(X) {
  MOVE X, R20;
  LSE R20;
  LSE R20;
  LSE R20;
  ADD R20, X;
  LSE R20;
  LSE R20;
  LSE R20;
  ADD R20, X;
  LSE R20;
  ADD R20, X;
}

MUL(X1, X2) {
  MOVEi 0, R1;
  MOVEi 0, R2;
  MOVEi 1, R29;
  MOVEi 32, R23;
  MOVE X2, R26; // Partie Basse
  MOVEi 0, R27; 
  MOVE X1, R22;
  AND R29, R22; // Recuperation du premier bit
  MOVEi 0, R28;
  SUB R22, R28; // MASQUE
  MOVE R26, R25;
  AND R28, R25;
  ADD R25, R1;
  ADD R30, R2;
  MOVE R27, R25;
  AND R28, R25;
  ADD R25, R2; // ADDITION SI NECESSAIRE
  MOVE R26, R25;
  MOVEi 31, R24;
  RS R24, R25;
  LSE R26;
  LSE R27;
  ADD R25, R27;
  RSE X1;
  SUBi 1, R23;
  MTZi -21;
}

MOD24(X) {
    // On effectue le calcul trois et non deux fois car 2^16 mod 24 = 16,
    // on a besoin de plus d'étapes (preuve par le calcul avec les valeurs maximales
    // possible d'un nombre 64 bits
  MOVE X, R10;
  MOVEi 10, R13;
  MOVE *R13, R13;
  AND R13, R10;
  MOVEi 16, R11;
  RS R11, X;
  // 2^16 mod 24
  LSE X;
  LSE X;
  LSE X;
  LSE X;
  ADD R10, X;
  MOVE X, R10;
  AND R13, R10;
  MOVEi 16, R11;
  RS R11, X;
  // 2^16 mod 24
  LSE X;
  LSE X;
  LSE X;
  LSE X;
  ADD R10, X;
  MOVE X, R10;
  AND R13, R10;
  MOVEi 16, R11;
  RS R11, X;
  // 2^16 mod 24
  LSE X;
  LSE X;
  LSE X;
  LSE X;
  ADD R10, X;
  
  MOVE X, R10;
  MOVEi 255, R13;
  AND R13, R10;
  MOVEi 8, R11;
  RS R11, X;
  // 2^8 mod 24
  LSE X;
  LSE X;
  LSE X;
  LSE X;
  ADD R10, X;
  MOVE X, R10;
  AND R13, R10;
  MOVEi 8, R11;
  RS R11, X;
  // 2^8 mod 24
  LSE X;
  LSE X;
  LSE X;
  LSE X;
  ADD R10, X;
  MOVE X, R10;
  AND R13, R10;
  MOVEi 8, R11;
  RS R11, X;
  // 2^8 mod 24
  LSE X;
  LSE X;
  LSE X;
  LSE X;
  ADD R10, X;
  
  ADDi 11, X; // Calcul de l'adresse dans la lookup table du modulo 60 (sur 1 octet et non pas sur 4 bits)
  MOVE *X, X;
}

MOD60(X) {
    // On effectue le calcul trois et non deux fois car 2^16 mod 60 = 16,
    // on a besoin de plus d'étapes (preuve par le calcul avec les valeurs maximales
    // possible d'un nombre 64 bits
  MOVE X, R10;
  MOVEi 10, R13;
  MOVE *R13, R13;
  AND R13, R10;
  MOVEi 16, R11;
  RS R11, X;
  // 2^16 mod 24
  LSE X;
  LSE X;
  LSE X;
  LSE X;
  ADD R10, X;
  MOVE X, R10;
  AND R13, R10;
  MOVEi 16, R11;
  RS R11, X;
  // 2^16 mod 24
  LSE X;
  LSE X;
  LSE X;
  LSE X;
  ADD R10, X;
  MOVE X, R10;
  AND R13, R10;
  MOVEi 16, R11;
  RS R11, X;
  // 2^16 mod 24
  LSE X;
  LSE X;
  LSE X;
  LSE X;
  ADD R10, X;
  
    // Par le calcul quatre étapes
  MOVE X, R10;
  MOVEi 255, R13;
  AND R13, R10;
  MOVEi 8, R11;
  RS R11, X;
  // 2^8 mod 24
  LSE X;
  LSE X;
  LSE X;
  LSE X;
  ADD R10, X;
  MOVE X, R10;
  AND R13, R10;
  MOVEi 8, R11;
  RS R11, X;
  // 2^8 mod 24
  LSE X;
  LSE X;
  LSE X;
  LSE X;
  ADD R10, X;
  MOVE X, R10;
  AND R13, R10;
  MOVEi 8, R11;
  RS R11, X;
  // 2^8 mod 24
  LSE X;
  LSE X;
  LSE X;
  LSE X;
  ADD R10, X;
  
  ADDi 267, X; // Calcul de l'adresse dans la lookup table du modulo 60 (sur 1 octet et non pas sur 4 bits)
  MOVE *X, X;
}

MOD365(X) {
    // On effectue le calcul trois et non deux fois car 2^16 mod 365 = 201,
    // on a besoin de plus d'étapes (preuve par le calcul avec les valeurs maximales
    // possible d'un nombre 64 bits
  MOVE X, R10;
  MOVEi 10, R13;
  MOVE *R13, R13;
  AND R13, R10;
  MOVEi 16, R11;
  RS R11, X;
  MUL201 X; // 2^16 mod 365
  ADD R10, X;
  MOVE X, R10;
  AND R13, R10;
  MOVEi 16, R11;
  RS R11, X;
  MUL201 X; // 2^16 mod 365
  ADD R10, X;
  MOVE X, R10;
  AND R13, R10;
  MOVEi 16, R11;
  RS R11, X;
  MUL201 X; // 2^16 mod 365
  ADD R10, X;
  
  ADDi 523, X; // Calcul de l'adresse dans la lookup table du modulo 60 (sur 1 octet et non pas sur 4 bits)
  MOVE *X, X;
}

DIV24(X) {
  MOVEi 6, R10;
  MOVE *R10, R10;
  MUL R10, X;
  MOVE R2, X;
}

DIV60(X) {
  MOVEi 7, R10;
  MOVE *R10, R10;
  MUL R10, X;
  MOVE R2, X;
}

DIV365(X) {
  MOVEi 8, R10;
  MOVE *R10, R10;
  MUL R10, X;
  MOVE R2, X;
}




#ENDMACRO

MOVEi 0, R4; // Recuperation du nombre de seconde de l'horloge
MOVE *R4, R5;

MOVE R5, R6; // Calcul des secondes
MOD60 R6;
MOVEi 1, R9; // Case mémoire de sortie seconde
MOVE R6, *R9; // Sauvegarde du nombre de secondes

SUB R6, R5;
DIV60 R5;

MOVE R5, R6; // Calcul des minutes
MOD60 R6;
MOVEi 2, R9; // Case mémoire de sortie minute
MOVE R6, *R9; // Sauvegarde du nombre de minutes

SUB R6, R5;
DIV60 R5;

MOVE R5, R6; // Calcul des heures
MOD24 R6;
MOVEi 3, R9; // Case mémoire de sortie heure
MOVE R6, *R9; // Sauvegarde du nombre de heures

SUB R6, R5;
DIV24 R5;

MOVE R5, R6; // Calcul des jours
MOD365 R6;
MOVEi 4, R9; // Case mémoire de sortie jour
MOVE R6, *R9; // Sauvegarde du nombre de jours

SUB R6, R5;
DIV365 R5;

MOVEi 5, R9; // Case mémoire de sortie annee
MOVE R5, *R9; // Sauvegarde du nombre de annee

MOVEi 0, R32; // Boucle avec le pointeur d'instruction




























