!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jul. 09, 2007
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Data Object Declaration and specification
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
!*  -- The equivalence statement
!*
!*  C582 (R555) If an equivalence-object is of a sequence derived type that is not a numeric sequence or
!*  character sequence type, all of the objects in the equivalence set shall be of the same type with
!*  the same type parameter values.
!*  (THere is an IBM extension on this)
!*
!*  (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT(KR,KI,KC,KL,KZ,L)

    INTEGER, KIND :: KR=4
    INTEGER, KIND :: KI=4
    INTEGER, KIND :: KC=4
    INTEGER, KIND :: KL=4
    INTEGER, KIND :: KZ=4
    INTEGER, LEN  :: L=4

    SEQUENCE

    REAL(KR)                   :: R(L)=KR
    INTEGER(KI)                :: I(L)=KI
    CHARACTER(KIND=KC, LEN=L)  :: C(L)=CHAR(48+KC)
    LOGICAL(KL)                :: A(L)=.TRUE.
    COMPLEX(KZ)                :: Z(L)=(KZ,-KZ)

  END TYPE

  END MODULE

  PROGRAM dtpEquivC582
  USE M

  INTEGER, PARAMETER :: N=95

  TYPE(DT (16,2,1,8,8,7))  :: T(N), S(N), R(N)

  EQUIVALENCE(T, R)
  EQUIVALENCE(R, S)

  DO I=1, N

  IF ( ANY ( LBOUND(T(I)%R) .NE. 1          ) ) ERROR STOP 20
  IF ( SIZE( T(I)%R )       .NE. 7            ) ERROR STOP 21
  IF ( KIND( T(I)%R )       .NE. 16           ) ERROR STOP 22
  IF ( ANY ( T(I)%R         .NE. 16         ) ) ERROR STOP 23

  IF ( ANY ( LBOUND(T(I)%I) .NE. 1          ) ) ERROR STOP 24
  IF ( SIZE( T(I)%I )       .NE. 7            ) ERROR STOP 25
  IF ( KIND( T(I)%I )       .NE. 2            ) ERROR STOP 26
  IF ( ANY ( T(I)%I         .NE. 2          ) ) ERROR STOP 27

  IF ( ANY ( LBOUND(T(I)%C) .NE. 1          ) ) ERROR STOP 31
  IF (       T(I)%C%LEN     .NE. 7            ) ERROR STOP 32
  IF (       T(I)%C%KIND    .NE. 1            ) ERROR STOP 33
  IF ( SIZE( T(I)%C )       .NE. 7            ) ERROR STOP 34
  IF ( ANY ( T(I)%C         .NE. CHAR(48+1) ) ) ERROR STOP 35

  IF ( ANY ( LBOUND(T(I)%A) .NE. 1          ) ) ERROR STOP 36
  IF ( SIZE( T(I)%A )       .NE. 7            ) ERROR STOP 37
  IF ( KIND( T(I)%A )       .NE. 8            ) ERROR STOP 38
  IF ( ANY ( T(I)%A         .NEQV. .TRUE.   ) ) ERROR STOP 39

  IF ( ANY ( LBOUND(T(I)%Z) .NE. 1          ) ) ERROR STOP 41
  IF ( SIZE( T(I)%Z )       .NE. 7            ) ERROR STOP 42
  IF ( KIND( T(I)%Z )       .NE. 8            ) ERROR STOP 43
  IF ( ANY ( T(I)%Z         .NE. (8, -8)    ) ) ERROR STOP 43

  R(I)%R = -T(I)%R
  T(I)%C = CHAR(0)
  R(I)%I = -T(I)%I
  T(I)%Z = -T(I)%Z
  R(I)%A = .NOT. T(I)%A

  IF ( ANY ( LBOUND(S(I)%R) .NE. 1          ) ) ERROR STOP 50
  IF ( SIZE( S(I)%R )       .NE. 7            ) ERROR STOP 51
  IF ( KIND( S(I)%R )       .NE. 16           ) ERROR STOP 52
  IF ( ANY ( S(I)%R         .NE. -16        ) ) ERROR STOP 53

  IF ( ANY ( LBOUND(S(I)%I) .NE. 1          ) ) ERROR STOP 54
  IF ( SIZE( S(I)%I )       .NE. 7            ) ERROR STOP 55
  IF ( KIND( S(I)%I )       .NE. 2            ) ERROR STOP 56
  IF ( ANY ( S(I)%I         .NE. -2         ) ) ERROR STOP 57

  IF ( ANY ( LBOUND(S(I)%C) .NE. 1          ) ) ERROR STOP 61
  IF (       S(I)%C%LEN     .NE. 7            ) ERROR STOP 62
  IF (       S(I)%C%KIND    .NE. 1            ) ERROR STOP 63
  IF ( SIZE( S(I)%C )       .NE. 7            ) ERROR STOP 64
  IF ( ANY ( S(I)%C         .NE. CHAR(0)    ) ) ERROR STOP 65

  IF ( ANY ( LBOUND(S(I)%A) .NE. 1          ) ) ERROR STOP 66
  IF ( SIZE( S(I)%A )       .NE. 7            ) ERROR STOP 67
  IF ( KIND( S(I)%A )       .NE. 8            ) ERROR STOP 68
  IF ( ANY ( S(I)%A         .NEQV. .FALSE.  ) ) ERROR STOP 69

  IF ( ANY ( LBOUND(S(I)%Z) .NE. 1          ) ) ERROR STOP 71
  IF ( SIZE( S(I)%Z )       .NE. 7            ) ERROR STOP 72
  IF ( KIND( S(I)%Z )       .NE. 8            ) ERROR STOP 73
  IF ( ANY ( S(I)%Z         .NE. -(8, -8)   ) ) ERROR STOP 73


  END DO

  END


