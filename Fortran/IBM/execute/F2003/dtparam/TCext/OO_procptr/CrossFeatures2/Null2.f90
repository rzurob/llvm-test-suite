! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/OO_procptr/CrossFeatures2/Null2.f
! opt variations: -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
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
    TYPE :: DT(N1,K1)    ! (20,4)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      INTEGER(K1)   :: Id
      PROCEDURE(Fun), POINTER :: ProcPtr=>NULL()
    END TYPE

    CONTAINS

    FUNCTION Fun(Arg)
    TYPE(DT(20,4))  :: Fun
    CLASS(DT(*,4)) :: Arg
      Fun = Arg
    END FUNCTION

  END MODULE

  PROGRAM Null2
  USE M
  IMPLICIT NONE
  TYPE(DT(20,4)), TARGET  :: V,W(3)
  TYPE(DT(:,4)), POINTER :: Ptr(:)

  INTERFACE ExtSub
    SUBROUTINE ExtSub1(V1, V2, V3, V4)
    IMPORT  DT
      TYPE (DT(:,4)), POINTER :: V1(:)
      TYPE (DT(*,4)) :: V2(:)
      TYPE (DT(*,4)) :: V3(:)
      TYPE (DT(*,4)) :: V4(:)
    END SUBROUTINE

    SUBROUTINE ExtSub2(V1, V2)
    IMPORT  DT
      TYPE (DT(:,4)), POINTER :: V1(:)
      TYPE (DT(*,4)) :: V2(:)
    END SUBROUTINE

  END INTERFACE

  Ptr => W(1:0)
  CALL ExtSub( NULL(Ptr)   ,                                            &
             & (/DT(20,4)(-1, NULL(V%ProcPtr)) /),                      &
             & (/DT(20,4)(-1, NULL()),DT(20,4)(-1, NULL()),DT(20,4)(-1, NULL()) /), &
             & (/DT(20,4)(-1, NULL(W(1)%ProcPtr)) /)  )

  CALL ExtSub( Ptr,  &
             & (/DT(20,4)(-1, NULL(V%ProcPtr)) /) )

  END

  SUBROUTINE ExtSub2(V1, V2)
  USE M
  TYPE (DT(:,4)), POINTER :: V1(:)
  TYPE (DT(*,4)) :: V2(:)

  IF (.NOT. ASSOCIATED(V1))  ERROR STOP 21
  IF (SIZE(V1) .NE. 0) ERROR STOP 22
  IF (SIZE(V2) .NE. 1) ERROR STOP 24

  END SUBROUTINE



  SUBROUTINE ExtSub1(V1, V2, V3, V4)
  USE M
  TYPE (DT(:,4)), POINTER :: V1(:)
  TYPE (DT(*,4)) :: V2(:)
  TYPE (DT(*,4)) :: V3(:)
  TYPE (DT(*,4)) :: V4(:)


  IF (ASSOCIATED(V1))  ERROR STOP 11

  IF (SIZE(V2) .NE. 1) ERROR STOP 14

  IF (SIZE(V3) .NE. 3) ERROR STOP 16

  IF (SIZE(V4) .NE. 1) ERROR STOP 18

  END SUBROUTINE



