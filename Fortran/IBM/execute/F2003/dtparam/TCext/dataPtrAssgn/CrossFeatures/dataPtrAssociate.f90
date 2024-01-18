! GB DTP extension using:
! ftcx_dtp -qk -qnodefaultpv -qdeferredlp -qreuse=none /tstdev/F2003/dataPtrAssgn/CrossFeatures/dataPtrAssociate.f
! opt variations: -qck -qnok -qdefaultpv -qnodeferredlp -qreuse=self -qreuse=base

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
!*  Associate
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT(N1,K1)    ! (3,4)
    INTEGER, KIND               :: K1
    INTEGER, LEN                :: N1
    CHARACTER(N1), PRIVATE      :: C="???"
    INTEGER(K1)                 :: ID
    CLASS(DT(:,K1)), POINTER    :: Ptr(:, :)
    CONTAINS
    PROCEDURE :: ModFun
  END TYPE

  TYPE, EXTENDS(DT) :: DT1(K2,N2)    ! (3,4,4,3)
    INTEGER, KIND          :: K2
    INTEGER, LEN           :: N2
    CHARACTER(N2), PRIVATE :: CC="???"
  END TYPE

  CONTAINS

  ELEMENTAL FUNCTION ModFun(Arg)
  CLASS(DT(*,4)), INTENT(IN) :: Arg
  INTEGER               :: ModFun
    ModFun = Arg%ID
  END FUNCTION

  END MODULE

  PROGRAM dataPtrAssociate
  USE M
  IMPLICIT NONE

  TYPE(DT(3,4)), TARGET  ::  Tar2(100, 100)
  TYPE(DT1(3,4,4,3)), TARGET  :: Tar1(10000)
  CLASS(DT(:,4)), POINTER :: Ptr(:, :)

  INTEGER    :: I, J, K, N

  N = 100; K = 0

  DO I =1, N
  DO J =I, N

    Ptr(I:, J:) => Tar2
    ASSOCIATE (Ptr => Ptr)
      SELECT TYPE(Ptr)
      TYPE IS (DT(*,4))
        Ptr = DT(3,4)(ID=I*J, Ptr=Tar2)
      END SELECT

      IF (ANY( LBOUND(Ptr) .NE. (/I, J /)))        STOP 12
      IF (ANY( UBOUND(Ptr) .NE. (/I+N-1, J+N-1/))) STOP 13
      IF (ANY( Tar2%ID     .NE.  I*J ))            STOP 14
    END ASSOCIATE
    IF (.NOT. ASSOCIATED(Ptr, Tar2))               STOP 11
    IF (ANY( Ptr%ModFun() .NE.  I*J ))             STOP 15

    Ptr(I:J, I:J) => Tar1
    ASSOCIATE( Ptr => Ptr)
      SELECT TYPE (Ptr)
      TYPE IS (DT1(*,4,4,*))
        Ptr = DT1(3,4,4,3)(ID=-I*J, Ptr=Tar2)
      END SELECT

      IF (ANY( LBOUND(Ptr) .NE. (/I,  I/)))      STOP 22
      IF (ANY( UBOUND(Ptr) .NE. (/J,  J/)))      STOP 23
      IF (ANY( Tar1(1:(J-I+1)*(J-I+1))%ID .NE.  -I*J ))  STOP 24
    END ASSOCIATE
    IF (.NOT. ASSOCIATED(Ptr))                   STOP 21
    IF (ANY( Ptr%ModFun() .NE.  -I*J ))          STOP 15

  END DO
  END DO


  END


