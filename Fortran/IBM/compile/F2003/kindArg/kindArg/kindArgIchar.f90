!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgIchar
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 12, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics 
!*
!*  SECONDARY FUNCTIONS TESTED : ICHAR 
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


  PROGRAM kindArgIchar


  INTEGER               :: I(2) = (/0,1/) 
  INTEGER,    PARAMETER :: II(2)= (/1,1/) 
  REAL,       PARAMETER :: R(2) = (/1.,1./) 
  CHARACTER,  PARAMETER :: CC(2)= (/ACHAR(1), ACHAR(1)/) 
  LOGICAL(8), PARAMETER :: L(2) = (/.TRUE., .FALSE./) 
  COMPLEX(8), PARAMETER :: Z(2) = (/(1.,1.), (1.,1.)/) 
  
  INTEGER(II(1)) :: ONE

  IC  = ICHAR(" ", I(1))
  IC1 = ICHAR(" ", I) 

  IC = ICHAR((/" ", "A"/), II)

  IC1= ICHAR(C=" ", KIND=R(1))
  IC1= ICHAR(C=" ", KIND=CC(2))
  IC1= ICHAR(C=" ", KIND=L(1))
  IC1= ICHAR(C=" ", KIND=Z(2))

  IC = ICHAR(C=" ", KIND=ONE%KIND) !ok

  END

