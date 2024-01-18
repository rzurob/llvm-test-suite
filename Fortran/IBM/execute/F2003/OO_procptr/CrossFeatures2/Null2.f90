! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: Null7.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Null7.f
!*
!*  DATE                       : May. 11, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED : Pointer assignment
!*
!*  REFERENCE                  : Feature 289058
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*   null()
!*   characteristics of the result are determined by the entity with which
!*   the reference is associated.
!*  (ice-struct constr)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT
      INTEGER :: Id
      PROCEDURE(Fun), POINTER :: ProcPtr=>NULL()
    END TYPE

    CONTAINS

    FUNCTION Fun(Arg)
    TYPE(DT)  :: Fun
    CLASS(DT) :: Arg
      Fun = Arg
    END FUNCTION

  END MODULE

  PROGRAM Null2
  USE M
  IMPLICIT NONE
  TYPE(DT), TARGET  :: V,W(3)
  TYPE(DT), POINTER :: Ptr(:)

  INTERFACE ExtSub
    SUBROUTINE ExtSub1(V1, V2, V3, V4)
    IMPORT  DT
      TYPE (DT), POINTER :: V1(:)
      TYPE (DT) :: V2(:)
      TYPE (DT) :: V3(:)
      TYPE (DT) :: V4(:)
    END SUBROUTINE

    SUBROUTINE ExtSub2(V1, V2)
    IMPORT  DT
      TYPE (DT), POINTER :: V1(:)
      TYPE (DT) :: V2(:)
    END SUBROUTINE

  END INTERFACE

  Ptr => W(1:0)
  CALL ExtSub( NULL(Ptr)   ,                                            &
             & (/DT(-1, NULL(V%ProcPtr)) /),                      &
             & (/DT(-1, NULL()),DT(-1, NULL()),DT(-1, NULL()) /), &
             & (/DT(-1, NULL(W(1)%ProcPtr)) /)  )

  CALL ExtSub( Ptr,  &
             & (/DT(-1, NULL(V%ProcPtr)) /) )

  END

  SUBROUTINE ExtSub2(V1, V2)
  USE M
  TYPE (DT), POINTER :: V1(:)
  TYPE (DT) :: V2(:)

  IF (.NOT. ASSOCIATED(V1))  STOP 21
  IF (SIZE(V1) .NE. 0) STOP 22
  IF (SIZE(V2) .NE. 1) STOP 24

  END SUBROUTINE



  SUBROUTINE ExtSub1(V1, V2, V3, V4)
  USE M
  TYPE (DT), POINTER :: V1(:)
  TYPE (DT) :: V2(:)
  TYPE (DT) :: V3(:)
  TYPE (DT) :: V4(:)


  IF (ASSOCIATED(V1))  STOP 11

  IF (SIZE(V2) .NE. 1) STOP 14

  IF (SIZE(V3) .NE. 3) STOP 16

  IF (SIZE(V4) .NE. 1) STOP 18

  END SUBROUTINE



