! GB DTP extension using:
! ftcx_dtp -qck /tstdev/OO_poly/selectType/CrossFeatures/Do1.f
! opt variations: -qnock

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: Do1.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Do1
!*
!*  DATE                       : Feb. 02, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Selector
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
!*
!* Do
!* ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM Do1
  IMPLICIT CLASS(DT(4,1,3))(U)
  IMPLICIT CLASS(*)(V)
  TYPE :: DT(K1,K2,N1)    ! (4,1,3)
    INTEGER, KIND             :: K1,K2
    INTEGER, LEN              :: N1
    INTEGER(K1)               :: Int
    CHARACTER(kind=K2,len=N1) :: C
  END TYPE
  INTEGER :: i

  CALL Sub((/(DT(4,1,3)(Int=6, C="123"), i=1,16)/), 6)

  CONTAINS

  SUBROUTINE Sub(U, V)
  DIMENSION :: U(:)

  SELECT TYPE (U)
  CLASS IS (DT(4,1,*))

    IF (ANY(U%Int   .NE. 6))      STOP 20
    IF (ANY(U%C     .NE. "123"))   STOP 21
    IF (ANY(SHAPE(U).NE. (/16/)))  STOP 22

    ASSOCIATE (N => U(U(1)%Int)%Int)
      DO i=1, N
        CONTINUE
      END DO
    END ASSOCIATE

  CLASS DEFAULT
    STOP 40
  END SELECT

  SELECT TYPE (V)
  CLASS DEFAULT
    STOP 40
  TYPE IS (INTEGER)
      DO i=1, V
        CONTINUE
      END DO
  END SELECT

  END SUBROUTINE

  END



