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

  TYPE :: DT
    CHARACTER(3), PRIVATE :: C="???"
    INTEGER               :: ID
    CLASS(DT), POINTER    :: Ptr(:, :)
    CONTAINS
    PROCEDURE :: ModFun
  END TYPE

  TYPE, EXTENDS(DT) :: DT1
    CHARACTER(3), PRIVATE :: CC="???"
  END TYPE

  CONTAINS

  ELEMENTAL FUNCTION ModFun(Arg)
  CLASS(DT), INTENT(IN) :: Arg
  INTEGER               :: ModFun
    ModFun = Arg%ID
  END FUNCTION

  END MODULE

  PROGRAM dataPtrAssociate
  USE M
  IMPLICIT NONE

  TYPE(DT), TARGET  ::  Tar2(100, 100)
  TYPE(DT1), TARGET  :: Tar1(10000)
  CLASS(DT), POINTER :: Ptr(:, :)

  INTEGER    :: I, J, K, N

  N = 100; K = 0

  DO I =1, N
  DO J =I, N

    Ptr(I:, J:) => Tar2
    ASSOCIATE (Ptr => Ptr)
      SELECT TYPE(Ptr)
      TYPE IS (DT)
        Ptr = DT(ID=I*J, Ptr=Tar2)
      END SELECT

      IF (ANY( LBOUND(Ptr) .NE. (/I, J /)))        ERROR STOP 12
      IF (ANY( UBOUND(Ptr) .NE. (/I+N-1, J+N-1/))) ERROR STOP 13
      IF (ANY( Tar2%ID     .NE.  I*J ))            ERROR STOP 14
    END ASSOCIATE
    IF (.NOT. ASSOCIATED(Ptr, Tar2))               ERROR STOP 11
    IF (ANY( Ptr%ModFun() .NE.  I*J ))             ERROR STOP 15

    Ptr(I:J, I:J) => Tar1
    ASSOCIATE( Ptr => Ptr)
      SELECT TYPE (Ptr)
      TYPE IS (DT1)
        Ptr = DT1(ID=-I*J, Ptr=Tar2)
      END SELECT

      IF (ANY( LBOUND(Ptr) .NE. (/I,  I/)))      ERROR STOP 22
      IF (ANY( UBOUND(Ptr) .NE. (/J,  J/)))      ERROR STOP 23
      IF (ANY( Tar1(1:(J-I+1)*(J-I+1))%ID .NE.  -I*J ))  ERROR STOP 24
    END ASSOCIATE
    IF (.NOT. ASSOCIATED(Ptr))                   ERROR STOP 21
    IF (ANY( Ptr%ModFun() .NE.  -I*J ))          ERROR STOP 15

  END DO
  END DO


  END


