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
!*  -- sequence
!*
!*  (322954)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dataPtrC717_2
  IMPLICIT NONE

  INTEGER :: I, J

  TYPE :: DT0
    SEQUENCE
    CHARACTER(3) :: C="123"
  END TYPE

  TYPE :: DT
    CLASS(*), POINTER :: Ptr(:)
  END TYPE

  TYPE(DT) :: T
  TYPE(DT0), TARGET :: Arr(3)=(/DT0("123"), DT0("213"), DT0("312")/)
  TYPE(DT0), POINTER :: SPtr(:)


  T%Ptr(1:) => Arr(1:)
  IF (ANY(LBOUND(T%Ptr) .NE. (/1/)))   ERROR STOP 11
  IF (ANY(UBOUND(T%Ptr) .NE. (/3/)))   ERROR STOP 12
  SPtr(1:) => T%Ptr
  IF (ANY(SPtr%C .NE. (/"123","213","312"/) ))   ERROR STOP 13

  ALLOCATE(T%Ptr(0:2), SOURCE=(/DT0("123"),DT0("213"),DT0("312")/))
  I=LBOUND(T%Ptr,1); J=UBOUND(T%Ptr,1)-1
  T%Ptr(I:J) => T%Ptr(I:J+1)
  IF (ANY(LBOUND(T%Ptr) .NE. (/I/)))   ERROR STOP 21
  IF (ANY(UBOUND(T%Ptr) .NE. (/J/)))   ERROR STOP 22

  SPtr(1:) => T%Ptr
  IF (ANY(SPtr%C .NE. (/"123","213"/) ))  ERROR STOP 23

  T%Ptr(1:-1) => T%Ptr
  IF (ANY(LBOUND(T%Ptr) .NE. (/1/)))   ERROR STOP 31
  IF (ANY(UBOUND(T%Ptr) .NE. (/0/)))   ERROR STOP 32

  END


