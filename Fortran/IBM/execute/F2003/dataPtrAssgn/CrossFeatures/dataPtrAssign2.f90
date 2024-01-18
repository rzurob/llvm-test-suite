!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrAssign2.f
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

  TYPE :: DT
    CHARACTER(3), PRIVATE :: C="???"
    INTEGER               :: ID
    CLASS(DT), POINTER    :: Ptr(:, :)
  END TYPE

  TYPE, EXTENDS(DT) :: DT1
    CHARACTER(3), PRIVATE :: CC="???"
  END TYPE

  END MODULE

  PROGRAM dataPtrAssign2
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
    SELECT TYPE(Ptr)
    TYPE IS (DT)
      Ptr = DT(ID=I*J, Ptr=Tar2)
    END SELECT

    IF (.NOT. ASSOCIATED(Ptr, Tar2))             STOP 11
    IF (ANY( LBOUND(Ptr) .NE. (/I, J /)))        STOP 12
    IF (ANY( UBOUND(Ptr) .NE. (/I+N-1, J+N-1/))) STOP 13
    IF (ANY( Tar2%ID     .NE.  I*J ))            STOP 14

    Ptr(I:J, I:J) => Tar1
    SELECT TYPE (Ptr)
    TYPE IS (DT1)
      Ptr = DT1(ID=-I*J, Ptr=Tar2)
    END SELECT

    IF (.NOT. ASSOCIATED(Ptr))                 STOP 21
    IF (ANY( LBOUND(Ptr) .NE. (/I,  I/)))      STOP 22
    IF (ANY( UBOUND(Ptr) .NE. (/J,  J/)))      STOP 23
    IF (ANY( Tar1(1:(J-I+1)*(J-I+1))%ID .NE.  -I*J ))  STOP 24

  END DO
  END DO


  END


