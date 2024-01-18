! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=self /tstdev/F2003/initExp/Misc/InitExpAssgn4.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=none

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TESTOP CASE NAME             : InitExpAssgn4.f  
!*  TESTOP CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Sept. 11 2006
!*  ORIGIN                     : Compiler Development IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Charber 289074 
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
!*  Derived typw intrinsic assignment 
!*  -- on component level
!* 
!* () -- impossible for defined assignment from being involved
!* 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT0(K1)    ! (4)
    INTEGER, KIND :: K1
    INTEGER(K1)   :: ID
  CONTAINS
  PROCEDURE :: MyAssgn
  GENERIC :: ASSIGNMENT(=) => MyAssgn
  END TYPE

  CONTAINS

  ELEMENTAL SUBROUTINE MyAssgn(Arg1, Arg2)
  CLASS(DT0(4)), INTENT(INOUT) :: Arg1
  TYPE(DT0(4)), INTENT(IN)    :: Arg2
    Arg1%ID = -Arg2%ID
  END SUBROUTINE

  END MODULE

  PROGRAM InitExpAssgn4 
  USE M
  IMPLICIT NONE

  INTEGER     :: I, J, K

  TYPE :: DT1(K2)    ! (4)
    INTEGER, KIND :: K2
    TYPE(DT0(K2)) :: T
  END TYPE

  TYPE :: DT(K3)    ! (4) 
    INTEGER, KIND :: K3
    TYPE(DT1(K3)) :: T(128)=[(DT1(K3)(T=DT0(K3)(I)), I=1,128)]
  END TYPE

  TYPE(DT(4)) :: T

  !IF ( ANY(T%T%T%ID .NE. [(-I, I=1,128)] )) STOP 11
  IF ( ANY(T%T%T%ID .NE. [(I, I=1,128)] )) STOP 11  

  END 


 
