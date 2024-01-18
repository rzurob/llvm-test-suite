! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/Basic/dataPtrC717_2.f
! opt variations: -qck -qnok -qnol -qnodeferredlp

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             :  dataPtrC717_2.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Feb. 2, 2006
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

  TYPE :: DT0(K1,N1)    ! (4,3)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    SEQUENCE
    CHARACTER(N1) :: C="123" 
  END TYPE

  TYPE :: DT(K2,N2)    ! (4,20)
      INTEGER, KIND :: K2
      INTEGER, LEN  :: N2
    CLASS(*), POINTER :: Ptr(:)
  END TYPE

  TYPE(DT(4,20)) :: T 
  TYPE(DT0(4,3)), TARGET :: Arr(3)=(/DT0(4,3)("123"), DT0(4,3)("213"), DT0(4,3)("312")/)
  TYPE(DT0(4,:)), POINTER :: SPtr(:)


  T%Ptr(1:) => Arr(1:)
  IF (ANY(LBOUND(T%Ptr) .NE. (/1/)))   STOP 11
  IF (ANY(UBOUND(T%Ptr) .NE. (/3/)))   STOP 12
  SPtr(1:) => T%Ptr
  IF (ANY(SPtr%C .NE. (/"123","213","312"/) ))   STOP 13

  ALLOCATE(T%Ptr(0:2), SOURCE=(/DT0(4,3)("123"),DT0(4,3)("213"),DT0(4,3)("312")/)) 
  I=LBOUND(T%Ptr,1); J=UBOUND(T%Ptr,1)-1
  T%Ptr(I:J) => T%Ptr(I:J+1)
  IF (ANY(LBOUND(T%Ptr) .NE. (/I/)))   STOP 21
  IF (ANY(UBOUND(T%Ptr) .NE. (/J/)))   STOP 22

  SPtr(1:) => T%Ptr
  IF (ANY(SPtr%C .NE. (/"123","213"/) ))  STOP 23

  T%Ptr(1:-1) => T%Ptr
  IF (ANY(LBOUND(T%Ptr) .NE. (/1/)))   STOP 31
  IF (ANY(UBOUND(T%Ptr) .NE. (/0/)))   STOP 32

  END


