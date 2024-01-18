!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgLbound
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 21, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics 
!*
!*  SECONDARY FUNCTIONS TESTED : LBOUND 
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


  PROGRAM kindArgLbound
  IMPLICIT NONE

  INTEGER               :: I(2) = (/0,1/) 
  INTEGER,    PARAMETER :: II(2)= (/1,1/) 
  REAL,       PARAMETER :: R(2) = (/1.,1./) 
  CHARACTER,  PARAMETER :: CC(2)= (/ACHAR(1), ACHAR(1)/) 
  LOGICAL(8), PARAMETER :: L(2) = (/.TRUE., .FALSE./) 
  COMPLEX(8), PARAMETER :: Z(2) = (/(1.,1.), (1.,1.)/) 
  
  ENUM, BIND(C)
    ENUMERATOR :: ONE=1
  END ENUM

  INTEGER  :: IC(1)

  IC    = LBOUND(II, I(1), I(1))
  IC(1) = LBOUND(II, DIM=I(1), KIND=II) 

  IC = LBOUND(ARRAY=II, KIND=II)

  IC(1) = LBOUND(ARRAY=II, KIND=R(1))
  IC(1) = LBOUND(ARRAY=II, KIND=CC(2))
  IC(1) = LBOUND(ARRAY=II, KIND=L(1))
  IC(1) = LBOUND(ARRAY=II, KIND=Z(2))

  IC    = LBOUND(ARRAY=II, KIND=ONE%KIND) !ok

  END

