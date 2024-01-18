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
! %POSTCMD: tcomp C816NonType.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : C816NonType
!*
!*  DATE                       : Dec. 3, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Constraint C816
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*    The non type entity is specified for TYPE SPEC
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM C816NonType
  IMPLICIT NONE

  INTEGER :: arr(100)
  COMMON /CN/ arr

  INTERFACE IFace
    SUBROUTINE Sub()
    END SUBROUTINE
  END INTERFACE


  CLASS(*), POINTER :: Ptr
  INTEGER, TARGET :: Tar

  Ptr => Tar
  SELECT TYPE ( Ptr )
    TYPE IS (INTEGER)
      STOP 50
    TYPE IS (REAL(9))
      STOP 50
    TYPE IS (CN)
      STOP 50
    TYPE IS (Any)
      STOP 50
    TYPE IS (Sub)
      STOP 50
    TYPE IS (IFACE)
      STOP 50

  END SELECT

  END

