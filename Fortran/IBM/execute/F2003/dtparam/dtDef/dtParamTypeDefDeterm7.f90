!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypeDefDeterm7   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Dec. 15, 2005
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Determination of Types 
!*
!*  REFERENCE                  : Feature Number 289057
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
!*  Determination of derived types - sequence types 
!*
!*  (Syntax err&ice/340231)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT(K, L)
    INTEGER, KIND :: K
    INTEGER, LEN  :: L
    SEQUENCE      
    INTEGER(K)  :: I
    REAL(K)     :: R
    COMPLEX(K)  :: Cplx
    LOGICAL(K)  :: LL
    CHARACTER(L):: C
  END TYPE

  INTERFACE 
    TYPE(DT(8, 4)) FUNCTION Fun(Arg)
      IMPORT DT
      TYPE(DT(8, *)) :: Arg
    END FUNCTION 
  END INTERFACE

 
  END MODULE

  PROGRAM dtParamTypeDefDeterm7 
  USE M, ONLY : Fun 

  TYPE :: DT(K, L)
    INTEGER, KIND :: K
    INTEGER, LEN  :: L
    SEQUENCE
    INTEGER(K)  :: I
    REAL(K)     :: R
    COMPLEX(K)  :: Cplx
    LOGICAL(K)  :: LL
    CHARACTER(L):: C
  END TYPE

  TYPE(DT(8, 4)) :: T
  
  T = Fun(DT(8,4)(-1_8, -1.0_8, (1._8, -1._8), .FALSE._8, "B"))

  IF ( T%I      .NE.   -1_8 )           STOP 23
  IF ( T%R      .NE.   -1.0_8 )         STOP 24
  IF ( T%Cplx   .NE.   (1._8, -1._8) )  STOP 25
  IF ( T%LL     .NEQV. .FALSE._8 )      STOP 26
  IF ( TRIM(T%C).NE.   TRIM("B"))       STOP 27

  END

  TYPE(DT(8, 4)) FUNCTION Fun(Arg) 
  TYPE :: DT(K, L)
    INTEGER, KIND :: K
    INTEGER, LEN  :: L
    SEQUENCE
    INTEGER(K)  :: I
    REAL(K)     :: R
    COMPLEX(K)  :: Cplx
    LOGICAL(K)  :: LL
    CHARACTER(L):: C
  END TYPE
  TYPE(DT(8, *)) Arg

    Fun =Arg 

  END FUNCTION 


