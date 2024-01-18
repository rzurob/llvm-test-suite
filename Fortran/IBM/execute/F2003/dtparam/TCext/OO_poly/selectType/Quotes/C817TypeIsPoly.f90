! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/selectType/Quotes/C817TypeIsPoly.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: C817TypeIsPoly.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : C817TypeIsPoly
!*
!*  DATE                       : Dec. 3, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Constraint C817
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
!*    The type guard TYPE IS is specified with the same type twice
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE, ABSTRACT :: Level0(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
    END TYPE

    TYPE, EXTENDS(Level0) :: Level1    ! (4,20)
      INTEGER(K1) :: Level1Id = 1
    END TYPE

    TYPE, EXTENDS(Level1) :: Level2    ! (4,20)
      INTEGER(K1) :: Level2Id = 2
    END TYPE

    TYPE, EXTENDS(Level2) :: Level3    ! (4,20)
      INTEGER(K1) :: Level3Id = 3
    END TYPE

    TYPE, EXTENDS(Level3) :: Level4    ! (4,20)
      INTEGER(K1) :: Level4Id = 4
    END TYPE

  END MODULE

  PROGRAM C817TypeIsPoly
  USE M
  IMPLICIT NONE

  CLASS(Level1(4,:)), POINTER :: Var
  TYPE(Level1(4,20)), TARGET  :: Tar

  Var => Tar

  ASSOCIATE ( As => Var )
  SELECT TYPE ( As )

    TYPE IS (Level4(4,*))
      STOP 50
    TYPE IS (Level3(4,*))
      STOP 51
    TYPE IS (Level1(4,*))
      !STOP 52
      PRINT*, "E-level, still run here"
    TYPE IS (Level3(4,*))
      STOP 53
    TYPE IS (Level1(4,*))
      STOP 54

    CLASS DEFAULT
      STOP 30
  END SELECT

  END ASSOCIATE

  END

