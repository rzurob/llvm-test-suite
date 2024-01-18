!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 2, 2006
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
!*  C717 (R735) If data-target is unlimited polymorphic, data-pointer-object shall be
!*  unlimited polymorphic,  of a sequence derived type, or of a type with
!*  the BIND attribute.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dataPtrC717_1
  IMPLICIT NONE

  INTEGER, TARGET  :: IArr(3)=(/1,2,3/)
  INTEGER :: I, J

  TYPE :: DT
    CHARACTER(3) :: C="123"
  END TYPE

  CLASS(*), POINTER :: T(:)
  CLASS(*), POINTER :: Ptr(:)

  T => IArr
  Ptr(SIZE(T):) => T(:)
  IF (ANY(LBOUND(Ptr) .NE. (/SIZE(T)/)))       STOP 11
  IF (ANY(UBOUND(Ptr) .NE. (/2*SIZE(T)-1/)))   STOP 12

  SELECT TYPE (Ptr)
  TYPE IS (INTEGER)
    IF (ANY(Ptr         .NE. (/1,2,3/) ))      STOP 13
  CLASS DEFAULT
    STOP 14
  END SELECT

  ALLOCATE(Ptr(0:2), SOURCE=(/DT("123"),DT("213"),DT("312")/))
  I=LBOUND(Ptr,1); J=UBOUND(Ptr,1)
  Ptr(LBOUND(Ptr,1):UBOUND(Ptr,1)) => Ptr(I:J)
  IF (ANY(LBOUND(Ptr) .NE. (/LBOUND(Ptr,1)/)))   STOP 21
  IF (ANY(UBOUND(Ptr) .NE. (/UBOUND(Ptr,1)/)))   STOP 22

  SELECT TYPE (Ptr)
  TYPE IS (DT)
    IF (ANY(Ptr%C .NE. (/"123","213","312"/) ))  STOP 23
  CLASS DEFAULT
    STOP 24
  END SELECT

  END


