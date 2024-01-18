! GB DTP extension using:
! ftcx_dtp -qck -qnodefaultpv -qdeferredlp -qreuse=self -qreuse=base /tstdev/F2003/dataPtrAssgn/CrossFeatures/dataPtrUse.f
! opt variations: -qnock -qdefaultpv -qnodeferredlp -qreuse=none

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrUse.f  
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
!*  Use 
!*
!*  
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: MT(K1,N1,K2)    ! (1,3,4)
    INTEGER, KIND                      :: K1,K2
    INTEGER, LEN                       :: N1
    CHARACTER(kind=K1,len=N1), PRIVATE :: C="???"
    INTEGER(K2)                        :: ID
    CLASS(MT(K1,:,K2)), POINTER        :: Ptr(:, :)
    CONTAINS
    PROCEDURE, NOPASS :: ModFun1
    PROCEDURE, NOPASS :: ModFun2
  END TYPE

  TYPE, EXTENDS(MT) :: MT1    ! (1,3,4)
    CHARACTER(kind=K1,len=N1), PRIVATE :: CC="???"
  END TYPE

  CLASS(MT(1,:,4)), POINTER :: MPtr(:, :)

  CONTAINS

  FUNCTION ModFun1(Arg)
  CLASS(MT(1,*,4)), TARGET, INTENT(IN) :: Arg(:)
  CLASS(MT(1,:,4)),  POINTER    :: ModFun1(:)
    ModFun1 => Arg
  END FUNCTION

  FUNCTION ModFun2(Arg)
  CLASS(MT(1,*,4)), TARGET, INTENT(IN) :: Arg(:, :)
  CLASS(MT(1,:,4)),  POINTER    :: ModFun2(:, :)
    ModFun2 => Arg
  END FUNCTION

  END MODULE

  PROGRAM dataPtrUse 
  USE M, DT=>MT, DT1=>MT1, Ptr=>MPtr
  IMPLICIT NONE

  TYPE(DT(1,3,4)), TARGET  ::  Tar2(100, 100)
  TYPE(DT1(1,3,4)), TARGET  :: Tar1(10000)

 
  INTEGER    :: I, J, K, N
 
  N = 100; K = 0

  DO I =1, N 
  DO J =I, N

    Ptr(I:, J:) => Ptr%ModFun2(Tar2)
    SELECT TYPE(Ptr)
    TYPE IS (DT(1,*,4))
      Ptr = DT(1,3,4)(ID=I*J, Ptr=Tar2)
    END SELECT
 
    IF (.NOT. ASSOCIATED(Ptr, Tar2))             STOP 11
    IF (ANY( LBOUND(Ptr) .NE. (/I, J /)))        STOP 12
    IF (ANY( UBOUND(Ptr) .NE. (/I+N-1, J+N-1/))) STOP 13
    IF (ANY( Tar2%ID     .NE.  I*J ))            STOP 14
 
    Ptr(I:J, I:J) => Ptr%ModFun1(Tar1) 
    SELECT TYPE (Ptr)
    TYPE IS (DT1(1,*,4))
      Ptr = DT1(1,3,4)(ID=-I*J, Ptr=Tar2) 
    END SELECT

    IF (.NOT. ASSOCIATED(Ptr))                 STOP 21
    IF (ANY( LBOUND(Ptr) .NE. (/I,  I/)))      STOP 22
    IF (ANY( UBOUND(Ptr) .NE. (/J,  J/)))      STOP 23
    IF (ANY( Tar1(1:(J-I+1)*(J-I+1))%ID .NE.  -I*J ))  STOP 24
 
  END DO
  END DO


  END


