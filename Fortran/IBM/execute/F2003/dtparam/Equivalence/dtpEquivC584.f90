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
!*  C584 584 (R556) If an equivalence-object has the PROTECTED attribute, all of the objects in
!*  the equivalence set shall have the PROTECTED attribute
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

  INTEGER, PARAMETER :: N=95

  SAVE
  TYPE(DT (16,2,1,8,8,7)), PROTECTED  :: T(N), S(N)
  TYPE(DT (16,2,1,8,8,7)), PROTECTED  :: R(N)

  EQUIVALENCE(T, R)
  EQUIVALENCE(R(1), S(1))

  CONTAINS

  SUBROUTINE Change()

  integer :: count = 0

  DO I=1, N
    R(I)%R = -R(I)%R
    if (mod(count,2) == 0) then
        R(I)%C = CHAR(0)
    else
        r(i)%c = char(49)
    end if
    R(I)%I = -R(I)%I
    R(I)%Z = -R(I)%Z
    R(I)%A = .NOT. R(I)%A
  END DO

  count = count + 1
  END SUBROUTINE

  END MODULE

  PROGRAM dtpEquivC584
  USE M

  DO I=1, N

  IF ( ANY ( LBOUND(T(I)%R) .NE. 1          ) ) STOP 20
  IF ( SIZE( T(I)%R )       .NE. 7            ) STOP 21
  IF ( KIND( T(I)%R )       .NE. 16           ) STOP 22
  IF ( ANY ( T(I)%R         .NE. 16         ) ) STOP 23

  IF ( ANY ( LBOUND(T(I)%I) .NE. 1          ) ) STOP 24
  IF ( SIZE( T(I)%I )       .NE. 7            ) STOP 25
  IF ( KIND( T(I)%I )       .NE. 2            ) STOP 26
  IF ( ANY ( T(I)%I         .NE. 2          ) ) STOP 27

  IF ( ANY ( LBOUND(T(I)%C) .NE. 1          ) ) STOP 31
  IF (       T(I)%C%LEN     .NE. 7            ) STOP 32
  IF (       T(I)%C%KIND    .NE. 1            ) STOP 33
  IF ( SIZE( T(I)%C )       .NE. 7            ) STOP 34
  IF ( ANY ( T(I)%C         .NE. CHAR(48+1) ) ) STOP 35

  IF ( ANY ( LBOUND(T(I)%A) .NE. 1          ) ) STOP 36
  IF ( SIZE( T(I)%A )       .NE. 7            ) STOP 37
  IF ( KIND( T(I)%A )       .NE. 8            ) STOP 38
  IF ( ANY ( T(I)%A         .NEQV. .TRUE.   ) ) STOP 39

  IF ( ANY ( LBOUND(T(I)%Z) .NE. 1          ) ) STOP 41
  IF ( SIZE( T(I)%Z )       .NE. 7            ) STOP 42
  IF ( KIND( T(I)%Z )       .NE. 8            ) STOP 43
  IF ( ANY ( T(I)%Z         .NE. (8, -8)    ) ) STOP 43

  CALL Change()

  IF ( ANY ( LBOUND(S(I)%R) .NE. 1          ) ) STOP 50
  IF ( SIZE( S(I)%R )       .NE. 7            ) STOP 51
  IF ( KIND( S(I)%R )       .NE. 16           ) STOP 52
  IF ( ANY ( S(I)%R         .NE. -16        ) ) STOP 53

  IF ( ANY ( LBOUND(S(I)%I) .NE. 1          ) ) STOP 54
  IF ( SIZE( S(I)%I )       .NE. 7            ) STOP 55
  IF ( KIND( S(I)%I )       .NE. 2            ) STOP 56
  IF ( ANY ( S(I)%I         .NE. -2         ) ) STOP 57

  IF ( ANY ( LBOUND(S(I)%C) .NE. 1          ) ) STOP 61
  IF (       S(I)%C%LEN     .NE. 7            ) STOP 62
  IF (       S(I)%C%KIND    .NE. 1            ) STOP 63
  IF ( SIZE( S(I)%C )       .NE. 7            ) STOP 64
  IF ( ANY ( S(I)%C         .NE. CHAR(0)    ) ) STOP 65

  IF ( ANY ( LBOUND(S(I)%A) .NE. 1          ) ) STOP 66
  IF ( SIZE( S(I)%A )       .NE. 7            ) STOP 67
  IF ( KIND( S(I)%A )       .NE. 8            ) STOP 68
  IF ( ANY ( S(I)%A         .NEQV. .FALSE.  ) ) STOP 69

  IF ( ANY ( LBOUND(S(I)%Z) .NE. 1          ) ) STOP 71
  IF ( SIZE( S(I)%Z )       .NE. 7            ) STOP 72
  IF ( KIND( S(I)%Z )       .NE. 8            ) STOP 73
  IF ( ANY ( S(I)%Z         .NE. -(8, -8)   ) ) STOP 73

  CALL Change()

  END DO

  END


