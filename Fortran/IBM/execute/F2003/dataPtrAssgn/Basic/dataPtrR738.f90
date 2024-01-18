!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 02, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 289075
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  R738 bounds-remapping is lower-bound-expr : upper-bound-expr
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dataPtrR738
  IMPLICIT NONE

  TYPE :: DT
    CLASS(DT), POINTER :: Ptr(:)
  END TYPE

  TYPE(DT), TARGET  :: Arr(8)
  TYPE(DT), POINTER :: Ptr(:)
  INTEGER(1)        :: I1
  INTEGER(8)        :: I8
  INTEGER           :: I

  I1=0; I8=7
  DO  I=1, 8
    Arr(I)%Ptr(I1:I8) => Arr
    IF (ANY(LBOUND(Arr(I)%Ptr) .NE.   (/I1/))) ERROR STOP 11
    IF (ANY(UBOUND(Arr(I)%Ptr) .NE.   (/I8/))) ERROR STOP 12
    IF ( .NOT. ASSOCIATED(Arr(I)%Ptr, Arr))    ERROR STOP 13
  END DO

  I1=1; I8=8
  Ptr(I1:I8) => Arr
  DO  I=1, 8
    Ptr(I)%Ptr(I1:I8) => Ptr
    IF (ANY(LBOUND(Ptr(I)%Ptr) .NE.   (/I1/))) ERROR STOP 21
    IF (ANY(UBOUND(Ptr(I)%Ptr) .NE.   (/I8/))) ERROR STOP 22
    IF ( .NOT. ASSOCIATED(Ptr(I)%Ptr, Ptr))    ERROR STOP 23
  END DO



  END


