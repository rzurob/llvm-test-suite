!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 08, 2006
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
!*  Save
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dataPtrSave
  IMPLICIT NONE

  TYPE :: DT
    SEQUENCE
    INTEGER      :: I=0
  END TYPE

  INTEGER        :: I, J

  DO I=1, 100
    CALL Sub()
  END DO

  CONTAINS

  SUBROUTINE Sub()
  TYPE(DT),   TARGET,  SAVE :: Arr(100)
  TYPE(DT),   POINTER, SAVE :: Ptr(:)
  INTEGER                   :: I=0

  IF ( I .EQ. 0 ) THEN

    Ptr(I:) => Arr
    IF (.NOT. ASSOCIATED(Ptr, Arr))            ERROR STOP 11
    IF (ANY( LBOUND(Ptr) .NE. (/I /)))         ERROR STOP 12
    IF (ANY( UBOUND(Ptr) .NE. (/I+99/)))       ERROR STOP 13
    IF (ANY( Ptr%I       .NE. 0 ))             ERROR STOP 14

    Ptr(I:99) => Arr
    IF (.NOT. ASSOCIATED(Ptr, Arr))            ERROR STOP 21
    IF (ANY( LBOUND(Ptr) .NE. (/I /)))         ERROR STOP 22
    IF (ANY( UBOUND(Ptr) .NE. (/ 99/)))        ERROR STOP 23
    IF (ANY( Ptr%I       .NE. 0 ))             ERROR STOP 24

    Arr(:)%I = 1
    I = 1

  ELSE

    Ptr(I:) => Arr
    IF (.NOT. ASSOCIATED(Ptr, Arr))            ERROR STOP 31
    IF (ANY( LBOUND(Ptr) .NE. (/I /)))         ERROR STOP 32
    IF (ANY( UBOUND(Ptr) .NE. (/I+99/)))       ERROR STOP 33
    IF (ANY( Ptr%I       .NE. I ))             ERROR STOP 34

    Ptr(I:100) => Arr
    IF (.NOT. ASSOCIATED(Ptr))                 ERROR STOP 41
    IF (ANY( LBOUND(Ptr) .NE. (/I /)))         ERROR STOP 42
    IF (ANY( UBOUND(Ptr) .NE. (/100/)))        ERROR STOP 43
    IF (ANY( Ptr%I       .NE. I ))             ERROR STOP 44

    I = I + 1
    Arr(:)%I = I

  END IF


  END SUBROUTINE

  END


