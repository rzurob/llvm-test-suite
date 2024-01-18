!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpEquivC576 
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jul. 06, 2007
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Data Object Declaration and specification
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
!* 
!*  -- The equivalence statement
!* 
!*  C576 (R556) An equivalence-object shall not be a designator with a base object that is a dummy
!*  argument, a pointer, an allocatable variable, a derived-type object that has an allocatable ulti-
!*  mate component, an object of a nonsequence derived type, an object of a derived type that has
!*  a pointer at any level of component selection, an automatic object, a function name, an entry
!*  name, a result name, a variable with the BIND attribute, a variable in a common block that
!*  has the BIND attribute, or a named constant.
!* 
!*  Ensure there will be no ice 
!*  ()
!*   
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
  USE ISO_C_BINDING
  TYPE :: DT_R(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    REAL(K)       :: R(L)=K
  END TYPE
 
  END MODULE

  MODULE M1
  USE M

  TYPE(DT_R), SAVE :: T

  TYPE, BIND(C) :: DT_B
    INTEGER(C_INT) :: I
  END TYPE
  TYPE(DT_B), BIND(C) :: TB
  EQUIVALENCE(TB, T)

  TYPE(DT_B) :: TC
  BIND(C) /C/
  COMMON /C/ TC 

  EQUIVALENCE(TC, T)
  
  END MODULE


  PROGRAM dtpEquivC576 
  USE M

  TYPE(DT_R), SAVE :: T

  TYPE(DT_R), POINTER :: P 
  EQUIVALENCE(P, T)

  TYPE(DT_R), ALLOCATABLE :: A 
  EQUIVALENCE(A, T)

  TYPE :: DT_A(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    REAL(K), ALLOCATABLE  :: R
  END TYPE
  TYPE(DT_A) :: TA
  EQUIVALENCE(TA, T)
 
  TYPE :: DT_NS(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    REAL(K)  :: R
  END TYPE
  TYPE(DT_NS) ::NS 
  EQUIVALENCE(NS, T)

  TYPE :: DT_P(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    REAL(K), POINTER  :: R
  END TYPE
  TYPE(DT_P) :: TP
  EQUIVALENCE(TP, T)
 
  TYPE(DT_R), PARAMETER :: TConst=DT_R()
  EQUIVALENCE(TConst, T)

  CONTAINS

  FUNCTION F(Arg, I)

  TYPE(DT_R) :: Arg, T
  EQUIVALENCE(Arg, T)

  TYPE(DT_R) :: Auto(I)
  EQUIVALENCE(Auto, T)

  TYPE(DT_R) :: F 
  EQUIVALENCE(F, T)

  END FUNCTION 

  FUNCTION F1() RESULT( Res )

  TYPE(DT_R) :: Res 
  EQUIVALENCE(Res, T)

  END FUNCTION 

  END

  FUNCTION F()
  USE M
  ENTRY EntF

  TYPE(DT_R) :: T
  TYPE(DT_R) :: F, EntF
  EQUIVALENCE(EntF, T)

  END FUNCTION 

