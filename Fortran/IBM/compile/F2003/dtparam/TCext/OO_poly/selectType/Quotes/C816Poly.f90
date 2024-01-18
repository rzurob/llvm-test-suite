! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_poly/selectType/Quotes/C816Poly.f
! opt variations: -qnok -ql -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  redherring.f  
! %VERIFY:  
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: tcomp C816Poly.f 
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : C816Poly
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Dec. 3, 2004
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type 
!*
!*  SECONDARY FUNCTIONS TESTED : Constraint C816 
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*    The selector is a non unlimited poly entity 
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE, ABSTRACT :: Level0(K1)    ! (4) 
        INTEGER, KIND :: K1
    END TYPE

    TYPE, EXTENDS(Level0) :: Level1    ! (4) 
      INTEGER(K1) :: Level1Id = 1
    END TYPE

    TYPE, EXTENDS(Level1) :: Level2    ! (4) 
      INTEGER(K1) :: Level2Id = 2
    END TYPE

    TYPE, EXTENDS(Level2) :: Level3    ! (4) 
      INTEGER(K1) :: Level3Id = 3
    END TYPE

    TYPE, EXTENDS(Level3) :: Level4    ! (4) 
      INTEGER(K1) :: Level4Id = 4
    END TYPE

  END MODULE

  PROGRAM C816Poly
  USE M
  IMPLICIT NONE
 
  CLASS(Level4(4)), ALLOCATABLE :: Var
 
  ALLOCATE(Var )

  SELECT TYPE ( Var) 
    TYPE IS (Level4(4))
      STOP 50
    TYPE IS (Level3(4))
      STOP 51
    TYPE IS (Level2(4))
      STOP 52
    TYPE IS (Level1(4))
      STOP 53
    TYPE IS (Level0(4))
      STOP 54
    CLASS DEFAULT
      STOP 30
  END SELECT 


  END

