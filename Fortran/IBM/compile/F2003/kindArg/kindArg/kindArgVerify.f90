!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgVerify
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jul. 06, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics 
!*
!*  SECONDARY FUNCTIONS TESTED : VERIFY 
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
!*  characteristics :: non initexp/non scalar/non int 
!*
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgVerify


  INTEGER               :: I(2) = (/0,1/) 
  INTEGER,    PARAMETER :: II(1)= (/1/) 
  REAL,       PARAMETER :: R(2) = (/1.,1./) 
  CHARACTER,  PARAMETER :: CC(2)= (/ACHAR(1), ACHAR(1)/) 
  LOGICAL(8), PARAMETER :: L(2) = (/.TRUE., .FALSE./) 
  COMPLEX(8), PARAMETER :: Z(2) = (/(1.,1.), (1.,1.)/) 
  
  INTEGER               :: IC(1)

  IC  = VERIFY("  "," ", .TRUE., I(1))
  IC1 = VERIFY((/character(2)::"  "/),(/character::" "/), KIND=I) 

  IC = VERIFY((/character(2)::"  "/),(/character::" "/), KIND=II)

  IC1= VERIFY(STRING="  ",SET=" ", KIND=R(1))
  IC1= VERIFY(STRING="  ",SET=" ", KIND=CC(2))
  IC1= VERIFY(STRING="  ",SET=" ", KIND=L(1))
  IC1= VERIFY(STRING="  ",SET=" ", KIND=Z(2))

  IC = VERIFY((/character(2)::"  "/),(/character::" "/), KIND=KIND((/character(2)::"  "/))) !ok

  END

