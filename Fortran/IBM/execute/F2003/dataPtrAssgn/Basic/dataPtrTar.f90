!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 04, 2006
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
!*   If data-target is not a pointer, data-pointer-object becomes pointer associated with
!*   the assignment target. Otherwise, the pointer association status of data-pointer-object
!*   becomes that of data-target; if data-target is associated with an object,
!*   data-pointer-object becomes associated with the assignment target.  If data
!*   target is allocatable, it shall be allocated.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dataPtrTar
  IMPLICIT NONE
  INTEGER :: N1=10

  CALL S(10)

  CONTAINS

  SUBROUTINE S(Arg)
  INTEGER    :: Arg
  CHARACTER(Arg),   TARGET  :: CArr(Arg, Arg)
  CHARACTER(Arg),   TARGET  :: CArr1(Arg*Arg)
  CHARACTER(Arg),   POINTER :: PCArr(:, :)
  CHARACTER(Arg-1), POINTER :: PCArr1(:, :)
  CHARACTER(Arg),   POINTER :: PCArr2(:, :)

  INTEGER               :: I, J

  CArr = RESHAPE((/(REPEAT(CHAR(I), Arg), I=1, Arg*Arg)/), (/Arg, Arg/))

  I = 0
  PCArr(I:, I: ) => CArr

  IF (ANY(LBOUND(PCArr) .NE. (/I,    I    /) ))   STOP 11
  IF (ANY(UBOUND(PCArr) .NE. (/Arg-1, Arg-1 /) )) STOP 12
  IF ( .NOT. ASSOCIATED(PCArr, CArr))             STOP 13
  IF (ANY(PCArr         .NE. CArr ))              STOP 14

  I = 3; J=Arg+2
  CArr1 = RESHAPE(CArr, (/Arg*Arg/))
  PCArr(I:J, I:J ) => CArr1(2:Arg*Arg)

  IF (ANY(LBOUND(PCArr) .NE. (/I,    I    /) ))   STOP 21
  IF (ANY(UBOUND(PCArr) .NE. (/J,    J    /) ))   STOP 22
! IF ( .NOT. ASSOCIATED(PCArr, CArr1(2:(J-I+1)*(J-I+1))))    STOP 23  !associated not support array with diff shape
  IF (ANY(PCArr         .NE. RESHAPE( CArr1(2:(J-I+1)*(J-I+1)), (/J-I+1, J-I+1/))))  STOP 24

  i = 1
  PCArr2(I:, I: ) => CArr(1:0, 2:3)
  PCArr(I:, I: ) =>  PCArr2

  IF (ANY(LBOUND(PCArr) .NE. (/I,    I    /) ))   STOP 31
  IF (ANY(UBOUND(PCArr) .NE. (/0,    2    /) ))   STOP 32
  IF (ASSOCIATED(PCArr1, CArr(1:0, 2:3)))         STOP 33

  I = 3; J=Arg
  CArr1 = RESHAPE(CArr, (/Arg*Arg/))
  PCArr1(I:J, I:J ) => CArr1(:)(1:Arg-1)

  CArr1(1:(J-I+1)*(J-I+1)) = RESHAPE(CArr(:, :)(1:Arg-1), (/(J-I+1)*(J-I+1)/))

  IF (ANY(LBOUND(PCArr1) .NE. (/I,    I    /) ))            STOP 41
  IF (ANY(UBOUND(PCArr1) .NE. (/J,    J    /) ))            STOP 42
! iF ( .NOT. ASSOCIATED(PCArr1, CArr1(1:(J-I+1)*(J-I+1))(1:Arg-1))    STOP 43
  IF (ANY(PCArr         .NE. RESHAPE(CArr1(1:(J-I+1)*(J-I+1))(1:Arg-1), (/J-I+1, J-I+1/)) ))  STOP 44



  END SUBROUTINE

  END


