! GB DTP extension using:
! ftcx_dtp -qck -qdeferredlp /tstdev/F2003/dataPtrAssgn/Basic/dataPtrC716_1.f
! opt variations: -qnock -qnodeferredlp

!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             :  dataPtrC716_1.f
!*
!*  DATE                       : Jan. 31, 2006
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
!*  C716 (R735) If data-target is not unlimited polymorphic, data-pointer-object shall
!*  be type compatible (5.1.1.2) with it, and the corresponding kind type
!*  parameters shall be equal.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dataPtrC716_1
  IMPLICIT NONE

  CHARACTER(3), TARGET  :: Arr(3)="123"
  CHARACTER(:), POINTER :: ptr(:)
  INTEGER               :: I, J

  TYPE :: DT0(K1,N1)    ! (1,1)
    INTEGER, KIND             :: K1
    INTEGER, LEN              :: N1
    CHARACTER(kind=K1,len=N1) :: C="!"
  END TYPE

  TYPE, EXTENDS(DT0) :: DT    ! (1,1)
  END TYPE

  TYPE(DT(1,1)), TARGET  :: Arr1(3)=(/DT(1,1)("1"), DT(1,1)("2"), DT(1,1)("3")/)
  TYPE(DT(1,:) ), POINTER :: Ptr1(:)

  CLASS(DT0(1,:)), POINTER  :: Ptr2(:)

  Ptr(4:) => Arr
  IF (ANY(LBOUND(Ptr) .NE. (/4/))) STOP 11
  IF (ANY(UBOUND(Ptr) .NE. (/6/))) STOP 12
  IF (ANY(Ptr         .NE. "123")) STOP 13

  Ptr(1:3) => Ptr(:)
  IF (ANY(LBOUND(Ptr) .NE. (/1/))) STOP 21
  IF (ANY(UBOUND(Ptr) .NE. (/3/))) STOP 22
  IF (ANY(Ptr         .NE. "123")) STOP 23

  Ptr1(1:) => Arr1(3:1:-1)
  IF (ANY(LBOUND(Ptr1) .NE. (/1/))) STOP 31
  IF (ANY(UBOUND(Ptr1) .NE. (/3/))) STOP 32
  IF (ANY(Ptr1%C       .NE. (/"3","2","1"/))) STOP 33

  I=2; J=2
  Ptr1(I:J) => Arr1
  IF (ANY(LBOUND(Ptr1) .NE. (/I/)))   STOP 41
  IF (ANY(UBOUND(Ptr1) .NE. (/J/)))   STOP 42
  IF (ANY(Ptr1%C       .NE. (/"1"/))) STOP 43

  I = -1
  Ptr2(I:) => Arr1(3:1:-1)
  IF (ANY(LBOUND(Ptr2) .NE. (/I/)))   STOP 51
  IF (ANY(UBOUND(Ptr2) .NE. (/I+2/))) STOP 52
  IF (ANY(Ptr2%C       .NE. (/"3","2","1"/))) STOP 53

  I=0; J=0
  Ptr1(I:J) => Ptr1
  IF (ANY(LBOUND(Ptr1) .NE. (/I/)))   STOP 61
  IF (ANY(UBOUND(Ptr1) .NE. (/J/)))   STOP 62
  IF (ANY(Ptr1%C       .NE. (/"1"/))) STOP 63


  END


