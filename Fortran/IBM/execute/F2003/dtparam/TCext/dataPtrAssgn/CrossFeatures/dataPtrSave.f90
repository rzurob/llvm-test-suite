! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/CrossFeatures/dataPtrSave.f
! opt variations: -qnol -qnodeferredlp

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrSave..f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Feb. 08, 2006
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
!*  Save 
!*
!*  
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dataPtrSave 
  IMPLICIT NONE

  TYPE :: DT(N1,K1)    ! (20,4)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    SEQUENCE
    INTEGER(K1)   :: I=0
  END TYPE

  INTEGER        :: I, J

  DO I=1, 100
    CALL Sub()
  END DO

  CONTAINS

  SUBROUTINE Sub()
  TYPE(DT(20,4)),   TARGET,  SAVE :: Arr(100)
  TYPE(DT(:,4)),   POINTER, SAVE :: Ptr(:)
  INTEGER                   :: I=0

  IF ( I .EQ. 0 ) THEN 

    Ptr(I:) => Arr 
    IF (.NOT. ASSOCIATED(Ptr, Arr))            STOP 11
    IF (ANY( LBOUND(Ptr) .NE. (/I /)))         STOP 12
    IF (ANY( UBOUND(Ptr) .NE. (/I+99/)))       STOP 13
    IF (ANY( Ptr%I       .NE. 0 ))             STOP 14

    Ptr(I:99) => Arr 
    IF (.NOT. ASSOCIATED(Ptr, Arr))            STOP 21
    IF (ANY( LBOUND(Ptr) .NE. (/I /)))         STOP 22
    IF (ANY( UBOUND(Ptr) .NE. (/ 99/)))        STOP 23
    IF (ANY( Ptr%I       .NE. 0 ))             STOP 24

    Arr(:)%I = 1
    I = 1 

  ELSE

    Ptr(I:) => Arr 
    IF (.NOT. ASSOCIATED(Ptr, Arr))            STOP 31
    IF (ANY( LBOUND(Ptr) .NE. (/I /)))         STOP 32
    IF (ANY( UBOUND(Ptr) .NE. (/I+99/)))       STOP 33
    IF (ANY( Ptr%I       .NE. I ))             STOP 34

    Ptr(I:100) => Arr 
    IF (.NOT. ASSOCIATED(Ptr))                 STOP 41
    IF (ANY( LBOUND(Ptr) .NE. (/I /)))         STOP 42
    IF (ANY( UBOUND(Ptr) .NE. (/100/)))        STOP 43
    IF (ANY( Ptr%I       .NE. I ))             STOP 44

    I = I + 1
    Arr(:)%I = I

  END IF


  END SUBROUTINE

  END


