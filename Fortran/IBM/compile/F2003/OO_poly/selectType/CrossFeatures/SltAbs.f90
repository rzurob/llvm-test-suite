! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: redherring.f 
! %VERIFY:  
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: tcomp SltAbs.f
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : SltAbs 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jan. 28, 2005
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type 
!*
!*  SECONDARY FUNCTIONS TESTED : Selector 
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
!*     
!*  Diagnostic : The selector is a parent component of abstract type 
!* 
!*  (ICE) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
    TYPE  :: DT0
      SEQUENCE
      INTEGER(4)      :: IArr(2)=-1 
      CHARACTER(1025) :: CArr(2)="!" 
    END TYPE

    TYPE, ABSTRACT :: DT1
      TYPE(DT0) :: Seq
    END TYPE
  
    TYPE, EXTENDS(DT1) :: DT
    END TYPE
  
  END MODULE

  PROGRAM SltAbs
  USE M
  IMPLICIT NONE
  TYPE (DT)  :: U(2,2,2)

  CALL Sub(U)

  CONTAINS
 
  SUBROUTINE Sub(U)
  CLASS(DT1) :: U(:,:,:)

  SELECT TYPE(U)
  CLASS IS (DT)
    SELECT TYPE (U%DT1)
    CLASS IS (DT1)
    END SELECT 
  END SELECT 

  END SUBROUTINE

  END



