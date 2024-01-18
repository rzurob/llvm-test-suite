! GB DTP extension using:
! ftcx_dtp -ql -qnodeferredlp -qreuse=base /tstdev/F2003/dataPtrAssgn/Basic/dataPtrPolyTar.f
! opt variations: -qnol -qdeferredlp -qreuse=none

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             :  dataPtrPolyTar.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Feb. 03, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 289075 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  
!*  If data-pointer-object is not polymorphic and data-target is polymorphic with 
!*  dynamic type that differs from its declared type, the assignment target is the ancestor
!*  component of data-target that has the type of data-pointer-object. Otherwise, the 
!*  assignment  target is data-target.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  TYPE :: DT0(N1,K1)    ! (20,4)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    INTEGER(K1)   :: ID0
  END TYPE

  TYPE, EXTENDS(DT0) :: DT1    ! (20,4)
    INTEGER(K1) :: ID1
  END TYPE

  END MODULE


  PROGRAM dataPtrPolyTar 
  USE M
  IMPLICIT NONE
  
  INTEGER               :: I, J
  TYPE(DT1(20,4)),  TARGET    :: Tar1(-10:10, -10:10)=DT1(20,4)(ID1=1, ID0=-1)

  TYPE(DT0(20,4)),  POINTER   :: Ptr0(:,:)
  CLASS(DT0(20,4)), POINTER   :: Ptr1(:,:)
  CLASS(DT1(20,4)), POINTER   :: Ptr2(:,:)

  Ptr1(LBOUND(Tar1,1):, LBOUND(Tar1,1):) => Tar1
  I=1
  Ptr0(I:, I:) => Ptr1
 
  IF (ANY(LBOUND(Ptr0) .NE. (/I,    I    /) )) STOP 11
  IF (ANY(UBOUND(Ptr0) .NE. (/I+20, I+20 /) )) STOP 12
  IF (ANY(Ptr0%ID0     .NE. -1 ))              STOP 13
  SELECT TYPE (Ptr1)
  TYPE IS (DT1(*,4)) 
    IF ( .NOT. ASSOCIATED(Ptr0, Ptr1%DT0))     STOP 14
  CLASS DEFAULT
    STOP 15
  END SELECT

  Tar1=DT1(20,4)(ID1=-1, ID0=1) 
  Ptr1(LBOUND(Tar1, 1):, LBOUND(Tar1, 1):) => Tar1
  I=-1
  Ptr0(I:I+1, I:I+2) => Ptr1(1,:)
 
  IF (ANY(LBOUND(Ptr0) .NE. (/I,    I    /) )) STOP 21
  IF (ANY(UBOUND(Ptr0) .NE. (/I+1,  I+2  /) )) STOP 22
  IF (ANY(Ptr0%ID0     .NE.   1 ))             STOP 23
  SELECT TYPE (Ptr1)
  TYPE IS (DT1(*,4)) 
    IF ( ASSOCIATED(Ptr0, Ptr1%DT0))           STOP 24
  CLASS DEFAULT
    STOP 25
  END SELECT
 
  Tar1=DT1(20,4)(ID1=-1, ID0=1) 
  I=1
  Ptr1(I:, I:) => Tar1 

  SELECT TYPE (Ptr1)
  TYPE IS (DT1(*,4)) 
    IF (ANY(LBOUND(Ptr1) .NE. (/I,    I    /) )) STOP 31
    IF (ANY(UBOUND(Ptr1) .NE. (/I+20, I+20 /) )) STOP 32
    IF (ANY(Ptr1%ID0         .NE.  1 ))          STOP 33
    IF (ANY(Ptr1%ID1         .NE. -1 ))          STOP 34
  CLASS DEFAULT 
    STOP 36
  END SELECT
  IF ( .NOT. ASSOCIATED(Ptr1, Tar1))             STOP 35

  Tar1=DT1(20,4)(ID1=-1, ID0=1) 
  I=16
  Ptr2(I:20, I:20) => Tar1(:,0)

  IF (ANY(LBOUND(Ptr2) .NE. (/I,  I    /) ))   STOP 41
  IF (ANY(UBOUND(Ptr2) .NE. (/20,   20 /) ))   STOP 42
  IF (ANY(Ptr2%ID0         .NE.  1 ))          STOP 43
  IF (ANY(Ptr2%ID1         .NE. -1 ))          STOP 44
  IF ( ASSOCIATED(Ptr2, Tar1))                 STOP 45
 
 
  END


