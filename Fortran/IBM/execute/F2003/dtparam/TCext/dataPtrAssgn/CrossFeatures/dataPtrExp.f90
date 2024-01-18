! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/F2003/dataPtrAssgn/CrossFeatures/dataPtrExp.f
! opt variations: -ql -qreuse=none

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrExp.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Feb. 16, 2006
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
!*  Expression - type 
!*
!*  
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT(K1)    ! (4)
    INTEGER, KIND :: K1
    INTEGER(K1)   :: ID=0 
    CLASS(*), POINTER :: Ptr(:, :) 
  END TYPE

  TYPE, EXTENDS(DT) :: DT1    ! (4)
    INTEGER(K1), PRIVATE :: ID1=1 
  END TYPE

  CONTAINS

  fUNCTION F1(Arg)
  CLASS(*), TARGET  :: Arg(:, :)
  CLASS(*), POINTER :: F1(:, :)
    F1(SIZE(Arg, 1):, SIZE(Arg, 2):) => Arg
  END FUNCTION

  FUNCTION F2(Arg)
  CLASS(*), TARGET  :: Arg(:)
  CLASS(*), POINTER :: F2(:)
    F2(SIZE(Arg, 1):) => Arg
  END FUNCTION

  END MODULE


  PROGRAM dataPtrExp 
  USE M
  IMPLICIT NONE

  TYPE(DT(4)), TARGET   :: T(100, 100)  = DT(4)(ID=-1, Ptr=NULL())
  TYPE(DT1(4)), TARGET  :: T1(10000)    = DT1(4)(ID=-2, Ptr=NULL()) 
  INTEGER    :: I, J, K, N
 
  N = 100; K = 0

  DO I =1, N 
  DO J =I, N 
    T(I, J)%Ptr(I:, J:) => F1(T) 
    IF (.NOT. ASSOCIATED(T(I, J)%Ptr))                   STOP 11
    IF (ANY( LBOUND(T(I, J)%Ptr) .NE. (/I, J /)))        STOP 12
    IF (ANY( UBOUND(T(I, J)%Ptr) .NE. (/I+N-1, J+N-1/))) STOP 13
    SELECT TYPE( As => T(I, J)%Ptr)
    TYPE IS (DT(4))
      IF (ANY( As%ID      .NE.  -1 ))                    STOP 14
    CLASS DEFAULT
      STOP 15
    END SELECT
 
    T1(I)%Ptr(I:J, I:J) => F2(T1) 
    IF (.NOT. ASSOCIATED(T1(I)%Ptr))                 STOP 21
    IF (ANY( LBOUND(T1(I)%Ptr) .NE. (/I,  I/)))      STOP 22
    IF (ANY( UBOUND(T1(I)%Ptr) .NE. (/J,  J/)))      STOP 23
    SELECT TYPE( As => T1(I)%Ptr)
    TYPE IS (DT1(4))
      IF (ANY( As%ID      .NE.  -2 ))                STOP 24
    CLASS DEFAULT
      STOP 25
    END SELECT
 
  END DO
  END DO

  END


