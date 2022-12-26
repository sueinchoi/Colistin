$PROB metabolite model (2 comp CMS + 2comp ColistinA + 1comp ColistinB)
$INPUT ID TIME MDV AMT II ADDL DUR RATE DV CMT TAD SSS=DROP NTAD Sex Age HT WT SOFA APACHE CKD Charson BSA Hct Cr Albumin CRP Volume_cum
$DATA  C:/Users/admin/Documents/GitHub/Colistin/Data_tidy/nonmem_covariates.csv IGNORE=@

$SUBROUTINE ADVAN13 TOL=4


$MODEL COMP=(CENT, DEFDOSE) COMP=(PERI) COMP=(MCENTA) COMP=(MPERIA) COMP=(MCENTB)

$PK
    ;---- FIXED EFFECT DEFINITION ---- 
        
    CLCR = (140-AGE)/Cr*WT/7.2*(0.85 + 0.15*SEX)
    
    TVCL = THETA(7)*CLCR**THETA(16)*Albumin**THETA(19)
    TVV1 = THETA(8)*(AGE/68)**THETA(17)*(WT/60)**THETA(18)
    TVV2 = THETA(9)
    TVQC = THETA(10)


    TVK13  = THETA(11)
    TVCLMA  = THETA(12)
    TVQM   = THETA(13)


    TVK15  = THETA(14)
    TVCLMB  = THETA(15)
  
    ;---- RANDOM EFFECT DEFINITION ----

    CL    = TVCL  * EXP(ETA(1))
    V1    = TVV1  * EXP(ETA(4))
    V2    = TVV2  * EXP(ETA(3))
    QC    = TVQC  * EXP(ETA(2))

    K13   = TVK13 * EXP(ETA(5))
    CLMA  = TVCLMA* EXP(ETA(8))
    QM    = TVQM  * EXP(ETA(7))

    K15   = TVK15 * EXP(ETA(6))
    CLMB  = TVCLMB* EXP(ETA(9))

    V3     = V1
    V4     = V2
    V5     = V1

    S1 = V1/1000
    S3 = V3/1000
    S5 = V5/1000

    K10 = CL/V1-K13-K15
    K12 = QC/V1
    K21 = QC/V2 
    K30 = CLMA/V3
    K34 = QM/V3
    K43 = QM/V4
    K50 = CLMB/V5

$DES 
    DADT(1) = - K12*A(1) - K10*A(1) + K21*A(2) - K13*A(1) - K15 * A(1)
    DADT(2) = K12*A(1) - K21*A(2)
    DADT(3) = K13*A(1) - K30*A(3) - K34*A(3) + K43*A(4)
    DADT(4) = K34*A(3) - K43*A(5)
    DADT(5) = K15*A(1) - K50*A(5)

$ERROR
    IF (CMT.EQ.1) THEN
    IPRED  = A(1)/S1
    W = SQRT(THETA(1)**2 + THETA(2)**2*IPRED**2)
    IRES  = DV - IPRED
    IWRES = IRES / W
    ENDIF

    IF (CMT.EQ.3) THEN
    IPRED  = A(3)/S3
    W = SQRT(THETA(3)**2 + THETA(4)**2*IPRED**2)  
    IRES  = DV - IPRED
    IWRES = IRES / W
    ENDIF

    IF (CMT.EQ.5) THEN
    IPRED  = A(5)/S5
    W = SQRT(THETA(5)**2 + THETA(6)**2*IPRED**2)  
    IRES  = DV - IPRED
    IWRES = IRES / W
    ENDIF

    Y     = IPRED + W * EPS(1)

$THETA
    0.0001 FIX    ; prop 
    0.5           ; add
    0.0001 FIX    ; prop
    0.5           ; add
    0.0001 FIX    ; prop 
    0.5           ; add
    
    (0, 1)        ; CL (7)
    (0, 0.1)        ; V1 (8)
    (0, 10)       ; V2 (9)
    (0, 1)        ; QC (10)
    (0, 1)        ; K13 (11)
    (0, 3)        ; CLMA (12)
    (0, 1)       ; QM (13)
    (0, 0.01)        ; K15 (14)
    (0, 0.1)        ; CLMB (15)

    0.1         ; Age on CL
    0.1        ; Age on V1
    0 FIX        ; WT on V1
    -0.1       ; Albumin on CL
    
$OMEGA BLOCK(1)
    0.1       ; CL (1)
$OMEGA BLOCK(1)
    0 FIX       ; V2 (2)
$OMEGA BLOCK(1)
    0 FIX       ; QC (3)
$OMEGA BLOCK(3)
    0.1    ; V1(4)
    -0.01 0.1       ; K13 (5)
    0.01 0.01  0.1   ; K15 (6)
$OMEGA BLOCK(1)
    0 FIX       ; QM (7)
$OMEGA BLOCK(1)
    0 FIX       ; CLMA (8)
$OMEGA BLOCK(1)
    0 FIX       ; CLMB (9) X

$SIGMA
    1 FIX


$ESTIMATION NOABORT MAXEVAL=9999 METHOD=1 INTER PRINT=10 SIGDIGITS=3  
;$COV
$TABLE ID TIME DV MDV CMT TAD IPRED IWRES IRES CWRES    ONEHEADER NOPRINT FILE = sdtab010_c1
$TABLE ID TIME CL V1 V2 QC K13 CLMA QM K15 CLMB ETA(1) ETA(2) ETA(3) ETA(4) ETA(5) ETA(6) ETA(7) ETA(8)
ETA(9)                     ONEHEADER NOPRINT NOAPPEND FILE = patab010_c1
$TABLE ID Sex SOFA APACHE CKD Charson ONEHEADER NOPRINT FILE = catab010_c1
$TABLE ID Age HT WT BSA Cr Albumin CRP Volume_cum ONEHEADER NOPRINT FILE = cotab010_c1