! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv -qdeferredlp -qreuse=self /tstdev/F2003/dataPtrAssgn/Basic/dataPtrR739.f
! opt variations: -qnol -qdefaultpv -qnodeferredlp -qreuse=none

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

  TYPE :: DT(N1,K1)    ! (20,4)
    INTEGER, KIND            :: K1
    INTEGER, LEN             :: N1
    INTEGER(K1)              :: ID
    CLASS(DT(:,K1)), POINTER :: Ptr1(:)
    CLASS(DT(:,K1)), POINTER :: Ptr2(:)
  END TYPE

  TYPE(DT(20,4)), TARGET  :: Arr(0:7)
  TYPE(DT(:,4)), POINTER :: Ptr(:)
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
    IF (ANY( Arr(I)%Ptr1%ID     .NE.   (/(J,J=I1, I8)/))) ERROR STOP 10
    IF (ANY(LBOUND(Arr(I)%Ptr1) .NE.   (/I/)))            ERROR STOP 11
    IF (ANY(UBOUND(Arr(I)%Ptr1) .NE.   (/I+I8/)))         ERROR STOP 12
    IF ( .NOT. ASSOCIATED(Arr(I)%Ptr1, Arr))              ERROR STOP 13
    IF (ANY( Arr(I)%Ptr2%ID     .NE.   (/(J,J=I1, I8)/))) ERROR STOP 14
    IF (ANY(LBOUND(Arr(I)%Ptr2) .NE.   (/I-I8/)))         ERROR STOP 15
    IF (ANY(UBOUND(Arr(I)%Ptr2) .NE.   (/I/)))            ERROR STOP 16
    IF ( .NOT. ASSOCIATED(Arr(I)%Ptr2, Arr))              ERROR STOP 17
  END DO


  Ptr(I1:I8) => Arr
  Arr(:)%ID = (/-0,-1,-2,-3,-4,-5,-6,-7/)

  DO  I=I1, I8
    Arr(I)%Ptr1(I:I+I8) => Ptr
    Arr(I)%Ptr2(I-I8:I) => Ptr(I)%Ptr1
  END DO

  DO  I=I1, I8
    IF (ANY( Arr(I)%Ptr1%ID     .NE.   (/(-J,J=I1, I8)/))) ERROR STOP 20
    IF (ANY(LBOUND(Arr(I)%Ptr1) .NE.   (/I/)))             ERROR STOP 21
    IF (ANY(UBOUND(Arr(I)%Ptr1) .NE.   (/I+I8/)))          ERROR STOP 22
    IF ( .NOT. ASSOCIATED(Arr(I)%Ptr1, Arr))               ERROR STOP 23
    IF (ANY( Arr(I)%Ptr2%ID     .NE.   (/(-J,J=I1, I8)/))) ERROR STOP 24
    IF (ANY(LBOUND(Arr(I)%Ptr2) .NE.   (/I-I8/)))          ERROR STOP 25
    IF (ANY(UBOUND(Arr(I)%Ptr2) .NE.   (/I/)))             ERROR STOP 26
    IF ( .NOT. ASSOCIATED(Arr(I)%Ptr2, Arr))               ERROR STOP 27
  END DO


  END

