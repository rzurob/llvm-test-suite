!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgMaxloc
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 26, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics 
!*
!*  SECONDARY FUNCTIONS TESTED : MAXLOC 
!*
!*  REFERENCE                  : Feature Number 289083 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*   
!*   
!*  Characteristics :: non initexp/non scalar/non int 
!*
!*  (322694) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgMaxloc


  INTEGER               :: I(2) = (/0,1/) 
  INTEGER,    PARAMETER :: II(1)= (/1/) 
  REAL,       PARAMETER :: R(2) = (/1.,1./) 
  CHARACTER,  PARAMETER :: CC(2)= (/ACHAR(1), ACHAR(1)/) 
  LOGICAL(8), PARAMETER :: L(2) = (/.TRUE., .FALSE./) 
  COMPLEX(8), PARAMETER :: Z(2) = (/(1.,1.), (1.,1.)/) 
  
  INTEGER               :: IC(1)

  IC  = MAXLOC((/" "," "/), .TRUE., I(1))  !Shall not complain mask
  IC1 = MAXLOC((/character(2)::"  "/),DIM=1, KIND=I) 

  IC = MAXLOC((/character(2)::" ", " "/), KIND=II)

  IC1= MAXLOC(ARRAY=R,  KIND=R(1))
  IC1= MAXLOC(ARRAY=CC, KIND=CC(2))
  IC1= MAXLOC(ARRAY=I,  KIND=L(1))
  IC1= MAXLOC(ARRAY=Z,  KIND=I(2))

  IC = MAXLOC(ARRAY=R, KIND=Z%KIND) !ok

  END

