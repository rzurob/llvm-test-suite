!*  ===================================================================
!*
!*  DATE                       : April 20, 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : Explicit Init Expression - PARAMETER
!*  SECONDARY FUNCTIONS TESTED : Array constructor
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod
      IMPLICIT NONE

      TYPE Base (k1,l1)
        INTEGER, KIND :: k1 = 4
        INTEGER, LEN  :: l1 = 10

        INTEGER(k1)   :: A0(l1) = -1
        CHARACTER(l1) :: C0(l1) = 'XLF'
        REAL(k1)      :: R0(l1) = -0.1
      END TYPE

      TYPE, EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2 = 4
        INTEGER, LEN  :: l2 = 10

        CLASS(Base(k2,l2)), POINTER :: cmp1
      END TYPE

      TYPE,  EXTENDS(Child) :: NextGen (l3)
        INTEGER, LEN  :: l3 = 10

        CLASS(Base(k1,l3)), POINTER :: cmp2
      END TYPE

END MODULE
PROGRAM DTP_PARAMETER_01b
      USE Mod

      INTEGER, PARAMETER :: M = 10, K = 4

      INTEGER,      PARAMETER :: Iconst(M) = [(I, I = 1, M)]
      CHARACTER(M), PARAMETER :: Cconst(M) = [(CHAR(I)//" ", I = 1, M)]
      REAL,         PARAMETER :: Rconst(M) = [(I/REAL(M), I = 1, M)]

      TYPE(Base),    PARAMETER :: b1 = Base ( Iconst, Cconst, Rconst )
      TYPE(Child),   PARAMETER :: c1 = Child ( 2*Iconst, Cconst, EXP(Rconst) , NULL() )
      TYPE(NextGen), PARAMETER :: n1 = NextGen ( b1%A0, b1%C0, b1%R0, NULL(), NULL() )

      CLASS(Base(K,:)), ALLOCATABLE :: poly
      LOGICAL(4), EXTERNAL :: precision_r4

      IF ( SIZE(n1%A0) .NE. M ) ERROR STOP 10
      IF ( SIZE(n1%C0) .NE. M ) ERROR STOP 11
      IF ( SIZE(n1%R0) .NE. M ) ERROR STOP 12
      IF ( LBOUND(n1%A0,1) .NE. 1 ) ERROR STOP 13
      IF ( LBOUND(n1%C0,1) .NE. 1 ) ERROR STOP 14
      IF ( LBOUND(n1%R0,1) .NE. 1 ) ERROR STOP 15
      IF ( UBOUND(n1%A0,1) .NE. M ) ERROR STOP 16
      IF ( UBOUND(n1%C0,1) .NE. M ) ERROR STOP 17
      IF ( UBOUND(n1%R0,1) .NE. M ) ERROR STOP 18
      IF ( ANY(n1%A0 .NE. [(I, I = 1, M)]) ) ERROR STOP 19
      DO I = 1, M
        IF ( TRIM(n1%C0(I)) .NE. CHAR(I) ) ERROR STOP 20
        IF ( .NOT. precision_r4(n1%R0(I), I/REAL(M)) ) ERROR STOP 21
      END DO

      IF ( ASSOCIATED(n1%cmp1) ) ERROR STOP 22
      ALLOCATE ( n1%cmp1, SOURCE = c1 )
      IF ( SIZE(n1%cmp1%A0) .NE. M ) ERROR STOP 23
      IF ( SIZE(n1%cmp1%C0) .NE. M ) ERROR STOP 24
      IF ( SIZE(n1%cmp1%R0) .NE. M ) ERROR STOP 25
      IF ( LBOUND(n1%cmp1%A0,1) .NE. 1 ) ERROR STOP 26
      IF ( LBOUND(n1%cmp1%C0,1) .NE. 1 ) ERROR STOP 27
      IF ( LBOUND(n1%cmp1%R0,1) .NE. 1 ) ERROR STOP 28
      IF ( UBOUND(n1%cmp1%A0,1) .NE. M ) ERROR STOP 29
      IF ( UBOUND(n1%cmp1%C0,1) .NE. M ) ERROR STOP 30
      IF ( UBOUND(n1%cmp1%R0,1) .NE. M ) ERROR STOP 31
      IF ( ANY(n1%cmp1%A0 .NE. [(2*I, I = 1, M)]) ) ERROR STOP 32
      DO I = 1, M
        IF ( TRIM(n1%cmp1%C0(I)) .NE. CHAR(I) ) ERROR STOP 33
        IF ( .NOT. precision_r4(n1%cmp1%R0(I), EXP(I/REAL(M))) ) ERROR STOP 34
      END DO

      IF ( ASSOCIATED(n1%cmp2) ) ERROR STOP 35
      ALLOCATE ( n1%cmp2, SOURCE = b1 )
      IF ( SIZE(n1%cmp2%A0) .NE. M ) ERROR STOP 36
      IF ( SIZE(n1%cmp2%C0) .NE. M ) ERROR STOP 37
      IF ( SIZE(n1%cmp2%R0) .NE. M ) ERROR STOP 38
      IF ( LBOUND(n1%cmp2%A0,1) .NE. 1 ) ERROR STOP 39
      IF ( LBOUND(n1%cmp2%C0,1) .NE. 1 ) ERROR STOP 40
      IF ( LBOUND(n1%cmp2%R0,1) .NE. 1 ) ERROR STOP 41
      IF ( UBOUND(n1%cmp2%A0,1) .NE. M ) ERROR STOP 42
      IF ( UBOUND(n1%cmp2%C0,1) .NE. M ) ERROR STOP 43
      IF ( UBOUND(n1%cmp2%R0,1) .NE. M ) ERROR STOP 44
      IF ( ANY(n1%cmp2%A0 .NE. [(I, I = 1, M)]) ) ERROR STOP 45
      DO I = 1, M
        IF ( TRIM(n1%cmp2%C0(I)) .NE. CHAR(I) ) ERROR STOP 46
        IF ( .NOT. precision_r4(n1%cmp2%R0(I), I/REAL(M)) ) ERROR STOP 47
      END DO

      ALLOCATE( poly, source = n1 )
      SELECT TYPE ( s => poly )
          TYPE IS (NextGen(4,*,4,*,*))
             IF ( SIZE(s%A0) .NE. M ) ERROR STOP 50
             IF ( SIZE(s%C0) .NE. M ) ERROR STOP 51
             IF ( SIZE(s%R0) .NE. M ) ERROR STOP 52
             IF ( LBOUND(s%A0,1) .NE. 1 ) ERROR STOP 53
             IF ( LBOUND(s%C0,1) .NE. 1 ) ERROR STOP 54
             IF ( LBOUND(s%R0,1) .NE. 1 ) ERROR STOP 55
             IF ( UBOUND(s%A0,1) .NE. M ) ERROR STOP 56
             IF ( UBOUND(s%C0,1) .NE. M ) ERROR STOP 57
             IF ( UBOUND(s%R0,1) .NE. M ) ERROR STOP 58
             IF ( ANY(s%A0 .NE. [(I, I = 1, M)]) ) ERROR STOP 59
             DO I = 1, M
               IF ( TRIM(s%C0(I)) .NE. CHAR(I) ) ERROR STOP 60
               IF ( .NOT. precision_r4(s%R0(I), I/REAL(M)) ) ERROR STOP 61
             END DO

             IF ( .NOT. ASSOCIATED(s%cmp1) ) ERROR STOP 62
             IF ( SIZE(s%cmp1%A0) .NE. M ) ERROR STOP 63
             IF ( SIZE(s%cmp1%C0) .NE. M ) ERROR STOP 64
             IF ( SIZE(s%cmp1%R0) .NE. M ) ERROR STOP 65
             IF ( LBOUND(s%cmp1%A0,1) .NE. 1 ) ERROR STOP 66
             IF ( LBOUND(s%cmp1%C0,1) .NE. 1 ) ERROR STOP 67
             IF ( LBOUND(s%cmp1%R0,1) .NE. 1 ) ERROR STOP 68
             IF ( UBOUND(s%cmp1%A0,1) .NE. M ) ERROR STOP 69
             IF ( UBOUND(s%cmp1%C0,1) .NE. M ) ERROR STOP 70
             IF ( UBOUND(s%cmp1%R0,1) .NE. M ) ERROR STOP 71
             IF ( ANY(s%cmp1%A0 .NE. [(2*I, I = 1, M)]) ) ERROR STOP 72
             DO I = 1, M
               IF ( TRIM(s%cmp1%C0(I)) .NE. CHAR(I) ) ERROR STOP 73
               IF ( .NOT. precision_r4(s%cmp1%R0(I), EXP(I/REAL(M))) ) ERROR STOP 74
             END DO

             IF ( .NOT. ASSOCIATED(s%cmp2) ) ERROR STOP 75
             IF ( SIZE(s%cmp2%A0) .NE. M ) ERROR STOP 76
             IF ( SIZE(s%cmp2%C0) .NE. M ) ERROR STOP 77
             IF ( SIZE(s%cmp2%R0) .NE. M ) ERROR STOP 78
             IF ( LBOUND(s%cmp2%A0,1) .NE. 1 ) ERROR STOP 79
             IF ( LBOUND(s%cmp2%C0,1) .NE. 1 ) ERROR STOP 80
             IF ( LBOUND(s%cmp2%R0,1) .NE. 1 ) ERROR STOP 81
             IF ( UBOUND(s%cmp2%A0,1) .NE. M ) ERROR STOP 82
             IF ( UBOUND(s%cmp2%C0,1) .NE. M ) ERROR STOP 83
             IF ( UBOUND(s%cmp2%R0,1) .NE. M ) ERROR STOP 84
             IF ( ANY(s%cmp2%A0 .NE. [(I, I = 1, M)]) ) ERROR STOP 85
             DO I = 1, M
               IF ( TRIM(s%cmp2%C0(I)) .NE. CHAR(I) ) ERROR STOP 86
               IF ( .NOT. precision_r4(s%cmp2%R0(I), I/REAL(M)) ) ERROR STOP 87
             END DO

          CLASS DEFAULT
             STOP 88
      END SELECT
      DEALLOCATE( poly )

END PROGRAM DTP_PARAMETER_01b