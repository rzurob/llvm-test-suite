! GB DTP extension using:
! ftcx_dtp -qck -qnodefaultpv -qdeferredlp -qreuse=base /tstdev/F2003/dataPtrAssgn/CrossFeatures/dataPtrAssign2.f
! opt variations: -qnock -qdefaultpv -qnodeferredlp -qreuse=self -qreuse=none

!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 16, 2006
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
!*  Assignment
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT(K1,N1,K2)    ! (1,3,4)
    INTEGER, KIND                      :: K1,K2
    INTEGER, LEN                       :: N1
    CHARACTER(kind=K1,len=N1), PRIVATE :: C="???"
    INTEGER(K2)                        :: ID
    CLASS(DT(K1,:,K2)), POINTER        :: Ptr(:, :)
  END TYPE

  TYPE, EXTENDS(DT) :: DT1    ! (1,3,4)
    CHARACTER(kind=K1,len=N1), PRIVATE :: CC="???"
  END TYPE

  END MODULE

  PROGRAM dataPtrAssign2
  USE M
  IMPLICIT NONE

  TYPE(DT(1,3,4)), TARGET  ::  Tar2(100, 100)
  TYPE(DT1(1,3,4)), TARGET  :: Tar1(10000)
  CLASS(DT(1,:,4)), POINTER :: Ptr(:, :)

  INTEGER    :: I, J, K, N

  N = 100; K = 0

  DO I =1, N
  DO J =I, N

    Ptr(I:, J:) => Tar2
    SELECT TYPE(Ptr)
    TYPE IS (DT(1,*,4))
      Ptr = DT(1,3,4)(ID=I*J, Ptr=Tar2)
    END SELECT

    IF (.NOT. ASSOCIATED(Ptr, Tar2))             ERROR STOP 11
    IF (ANY( LBOUND(Ptr) .NE. (/I, J /)))        ERROR STOP 12
    IF (ANY( UBOUND(Ptr) .NE. (/I+N-1, J+N-1/))) ERROR STOP 13
    IF (ANY( Tar2%ID     .NE.  I*J ))            ERROR STOP 14

    Ptr(I:J, I:J) => Tar1
    SELECT TYPE (Ptr)
    TYPE IS (DT1(1,*,4))
      Ptr = DT1(1,3,4)(ID=-I*J, Ptr=Tar2)
    END SELECT

    IF (.NOT. ASSOCIATED(Ptr))                 ERROR STOP 21
    IF (ANY( LBOUND(Ptr) .NE. (/I,  I/)))      ERROR STOP 22
    IF (ANY( UBOUND(Ptr) .NE. (/J,  J/)))      ERROR STOP 23
    IF (ANY( Tar1(1:(J-I+1)*(J-I+1))%ID .NE.  -I*J ))  ERROR STOP 24

  END DO
  END DO


  END


