! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=none /tstdev/F2003/dataPtrAssgn/Basic/dataPtrR738.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=self

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             :  dataPtrR738.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Feb. 02, 2006
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
!*  R738 bounds-remapping is lower-bound-expr : upper-bound-expr 
!*   
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dataPtrR738 
  IMPLICIT NONE

  TYPE :: DT(K1,N1)    ! (4,20)
    INTEGER, KIND            :: K1
    INTEGER, LEN             :: N1
    CLASS(DT(K1,:)), POINTER :: Ptr(:)
  END TYPE

  TYPE(DT(4,20)), TARGET  :: Arr(8)
  TYPE(DT(4,:)), POINTER :: Ptr(:)
  INTEGER(1)        :: I1
  INTEGER(8)        :: I8
  INTEGER           :: I

  I1=0; I8=7 
  DO  I=1, 8
    Arr(I)%Ptr(I1:I8) => Arr
    IF (ANY(LBOUND(Arr(I)%Ptr) .NE.   (/I1/))) STOP 11
    IF (ANY(UBOUND(Arr(I)%Ptr) .NE.   (/I8/))) STOP 12
    IF ( .NOT. ASSOCIATED(Arr(I)%Ptr, Arr))    STOP 13
  END DO

  I1=1; I8=8 
  Ptr(I1:I8) => Arr 
  DO  I=1, 8
    Ptr(I)%Ptr(I1:I8) => Ptr 
    IF (ANY(LBOUND(Ptr(I)%Ptr) .NE.   (/I1/))) STOP 21
    IF (ANY(UBOUND(Ptr(I)%Ptr) .NE.   (/I8/))) STOP 22
    IF ( .NOT. ASSOCIATED(Ptr(I)%Ptr, Ptr))    STOP 23
  END DO



  END


