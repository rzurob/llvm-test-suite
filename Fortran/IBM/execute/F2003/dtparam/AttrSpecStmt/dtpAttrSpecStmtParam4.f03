!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 15, 2007
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Data Object Declaration
!*
!*  REFERENCE                  : Feature Number 289057
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  -- PARAMETER statement
!*  The value is converted according to the rules of intrinsic assignment
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtpAttrSpecStmtParam4

  TYPE :: DT(K, L)
    INTEGER(1), KIND :: K=1
    INTEGER(8), LEN  :: L=1
    INTEGER(K)    :: I(L:L+4)
    REAL   (K)    :: R(L:L+4)
    COMPLEX(K)    :: Z(L:L+4)
    LOGICAL(K)    :: T(L:L+4)
  END TYPE

  INTEGER, PARAMETER :: N=2049

  TYPE(DT(4_4, 5_1)) :: T(N)=DT(4_8,5_2)(I=1._8,R=2_8,Z=(3_8,4._16),T=.TRUE._1)

  DO I=1, N

    IF ( T(I)%I%KIND .NE. 4 ) ERROR STOP 11
    IF ( T(I)%R%KIND .NE. 4 ) ERROR STOP 12
    IF ( T(I)%Z%KIND .NE. 4 ) ERROR STOP 13
    IF ( T(I)%T%KIND .NE. 4 ) ERROR STOP 14

    IF ( SIZE(T(I)%I) .NE. 5 ) ERROR STOP 21
    IF ( SIZE(T(I)%R) .NE. 5 ) ERROR STOP 22
    IF ( SIZE(T(I)%Z) .NE. 5 ) ERROR STOP 23
    IF ( SIZE(T(I)%T) .NE. 5 ) ERROR STOP 24

    IF ( LBOUND(T(I)%I, 1) .NE. 5 ) ERROR STOP 21
    IF ( LBOUND(T(I)%R, 1) .NE. 5 ) ERROR STOP 22
    IF ( LBOUND(T(I)%Z, 1) .NE. 5 ) ERROR STOP 23
    IF ( LBOUND(T(I)%T, 1) .NE. 5 ) ERROR STOP 24

    IF ( ANY (T(I)%I   .NE. 1        ) ) ERROR STOP 41
    IF ( ANY (T(I)%R   .NE. 2        ) ) ERROR STOP 42
    IF ( ANY (T(I)%Z   .NE. (3.,4.)  ) ) ERROR STOP 43
    IF ( ANY (T(I)%T   .NEQV. .TRUE. ) ) ERROR STOP 44

  END DO

  END

