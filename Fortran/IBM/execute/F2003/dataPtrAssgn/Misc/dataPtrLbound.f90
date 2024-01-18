!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 21, 2006
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
!*  Lower bound
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM dataPtrLbound
  IMPLICIT NONE

  INTEGER, PARAMETER :: LB=-(2**31)
  INTEGER, PARAMETER :: UB=(2**31)-1

  INTEGER,  TARGET  :: Tar2(LB:LB, UB:UB), Tar22(UB:UB, UB:UB)
  INTEGER,  TARGET ::  Tar1(LB:LB), Tar11(UB:UB)
  INTEGER,  POINTER :: Ptr(:, :)
  INTEGER    :: I, J, K, N

  N = 1; K = 0

  Tar2 = UB
  Tar1 = LB

  Tar22 = UB
  Tar11 = LB

    Ptr(LB:, LB:) => Tar2
    CALL Check2(LB, LB, LB, LB, 1, UB)

    Ptr(UB:, UB:) => Tar2
    CALL Check2(UB, UB, UB, UB, 1, UB)

    Ptr(UB:, LB:) => Tar2
    CALL Check2(UB, LB, UB, LB, 1, UB)

    Ptr(LB:, LB:) => Tar22
    CALL Check2(LB, LB, LB, LB, 1, UB)

    Ptr(UB:, UB:) => Tar22
    CALL Check2(UB, UB, UB, UB, 1, UB)

    Ptr(UB:, LB:) => Tar22
    CALL Check2(UB, LB, UB, LB, 1, UB)


    Ptr(LB:LB, LB:LB) => Tar1
    CALL Check1(LB, LB, LB, LB, 1, LB)

    Ptr(UB:UB, UB:UB) => Tar1
    CALL Check1(UB, UB, UB, UB, 1, LB)

    Ptr(LB:LB, UB:UB) => Tar1
    CALL Check1(LB, LB, UB, UB, 1, LB)

    Ptr(LB:LB, LB:LB) => Tar11
    CALL Check1(LB, LB, LB, LB, 1, LB)

    Ptr(UB:UB, UB:UB) => Tar11
    CALL Check1(UB, UB, UB, UB, 1, LB)

    Ptr(LB:LB, UB:UB) => Tar11
    CALL Check1(LB, LB, UB, UB, 1, LB)

  CONTAINS

  SUBROUTINE Check2(L1, L2, U1, U2, SIZ, Value)
  INTEGER :: L1, L2, U1, U2, SIZ, Value

    IF (.NOT. ASSOCIATED(Ptr))         STOP 10
    IF (SIZE(Ptr)        .NE. Siz )          STOP 11
    IF (ANY( LBOUND(Ptr) .NE. (/L1, L2/)))   STOP 12
    IF (ANY( UBOUND(Ptr) .NE. (/U1, U2/)))   STOP 13
    IF (ANY( Ptr         .NE. VALUE))        STOP 14

  END SUBROUTINE

  SUBROUTINE Check1(L1, U1, L2, U2, SIZ, Value)
  INTEGER :: L1, L2, U1, U2, SIZ, Value

    IF (.NOT. ASSOCIATED(Ptr))               STOP 20
    IF (SIZE(Ptr)        .NE. Siz )          STOP 21
    IF (ANY( LBOUND(Ptr) .NE. (/L1, L2/)))   STOP 22
    IF (ANY( UBOUND(Ptr) .NE. (/U1, U2/)))   STOP 23
    IF (ANY( Ptr         .NE. VALUE))        STOP 24

  END SUBROUTINE

  END



