!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 03, 2006
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
!*  R739 data-target is variable or expr
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dataPtrR739
  IMPLICIT NONE

  TYPE :: DT
    INTEGER            ::   ID
    CLASS(DT), POINTER :: Ptr1(:)
    CLASS(DT), POINTER :: Ptr2(:)
  END TYPE

  TYPE(DT), TARGET  :: Arr(0:7)
  TYPE(DT), POINTER :: Ptr(:)
  INTEGER(1)        :: I1
  INTEGER(8)        :: I8
  INTEGER           :: I, J

  I1=0; I8=7

  Arr(:)%ID = (/0,1,2,3,4,5,6,7/)
  DO  I=I1, I8
    Arr(I)%Ptr1(I:I+I8) => Arr
    Arr(I)%Ptr2(I-I8:I) => Arr(I)%Ptr1
  END DO

  DO  I=I1, I8
    IF (ANY( Arr(I)%Ptr1%ID     .NE.   (/(J,J=I1, I8)/))) STOP 10
    IF (ANY(LBOUND(Arr(I)%Ptr1) .NE.   (/I/)))            STOP 11
    IF (ANY(UBOUND(Arr(I)%Ptr1) .NE.   (/I+I8/)))         STOP 12
    IF ( .NOT. ASSOCIATED(Arr(I)%Ptr1, Arr))              STOP 13
    IF (ANY( Arr(I)%Ptr2%ID     .NE.   (/(J,J=I1, I8)/))) STOP 14
    IF (ANY(LBOUND(Arr(I)%Ptr2) .NE.   (/I-I8/)))         STOP 15
    IF (ANY(UBOUND(Arr(I)%Ptr2) .NE.   (/I/)))            STOP 16
    IF ( .NOT. ASSOCIATED(Arr(I)%Ptr2, Arr))              STOP 17
  END DO


  Ptr(I1:I8) => Arr
  Arr(:)%ID = (/-0,-1,-2,-3,-4,-5,-6,-7/)

  DO  I=I1, I8
    Arr(I)%Ptr1(I:I+I8) => Ptr
    Arr(I)%Ptr2(I-I8:I) => Ptr(I)%Ptr1
  END DO

  DO  I=I1, I8
    IF (ANY( Arr(I)%Ptr1%ID     .NE.   (/(-J,J=I1, I8)/))) STOP 20
    IF (ANY(LBOUND(Arr(I)%Ptr1) .NE.   (/I/)))             STOP 21
    IF (ANY(UBOUND(Arr(I)%Ptr1) .NE.   (/I+I8/)))          STOP 22
    IF ( .NOT. ASSOCIATED(Arr(I)%Ptr1, Arr))               STOP 23
    IF (ANY( Arr(I)%Ptr2%ID     .NE.   (/(-J,J=I1, I8)/))) STOP 24
    IF (ANY(LBOUND(Arr(I)%Ptr2) .NE.   (/I-I8/)))          STOP 25
    IF (ANY(UBOUND(Arr(I)%Ptr2) .NE.   (/I/)))             STOP 26
    IF ( .NOT. ASSOCIATED(Arr(I)%Ptr2, Arr))               STOP 27
  END DO


  END


