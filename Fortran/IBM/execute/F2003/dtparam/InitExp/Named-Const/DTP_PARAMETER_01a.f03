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
!*  Defect 362080
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
END MODULE
PROGRAM DTP_PARAMETER_01a
      USE Mod

      INTEGER, PARAMETER :: M = 10, K = 4

      INTEGER,      PARAMETER :: Iconst(M) = [(I, I = 1, M)]
      CHARACTER(M), PARAMETER :: Cconst(M) = [(CHAR(I)//" ", I = 1, M)]
      REAL,         PARAMETER :: Rconst(M) = [(I/REAL(M), I = 1, M)]

      TYPE(Base),    PARAMETER :: b1 = Base ( Iconst, Cconst, Rconst )
      TYPE(Child),   PARAMETER :: c1 = Child ( 2*Iconst, Cconst, EXP(Rconst) , NULL() )

      CLASS(Base(K,:)), ALLOCATABLE :: poly
      LOGICAL(4), EXTERNAL :: precision_r4

      IF ( SIZE(b1%A0) .NE. M ) ERROR STOP 10
      IF ( SIZE(b1%C0) .NE. M ) ERROR STOP 11
      IF ( SIZE(b1%R0) .NE. M ) ERROR STOP 12
      IF ( LBOUND(b1%A0,1) .NE. 1 ) ERROR STOP 13
      IF ( LBOUND(b1%C0,1) .NE. 1 ) ERROR STOP 14
      IF ( LBOUND(b1%R0,1) .NE. 1 ) ERROR STOP 15
      IF ( UBOUND(b1%A0,1) .NE. M ) ERROR STOP 16
      IF ( UBOUND(b1%C0,1) .NE. M ) ERROR STOP 17
      IF ( UBOUND(b1%R0,1) .NE. M ) ERROR STOP 18
      IF ( ANY(b1%A0 .NE. [(I, I = 1, M)]) ) ERROR STOP 19
      DO I = 1, M
        IF ( TRIM(b1%C0(I)) .NE. CHAR(I) ) ERROR STOP 20
        IF ( .NOT. precision_r4(b1%R0(I), I/REAL(M)) ) ERROR STOP 21
      END DO

      ALLOCATE( poly, source = b1 )
      IF ( SIZE(poly%A0) .NE. M ) ERROR STOP 30
      IF ( SIZE(poly%C0) .NE. M ) ERROR STOP 31
      IF ( SIZE(poly%R0) .NE. M ) ERROR STOP 32
      IF ( LBOUND(poly%A0,1) .NE. 1 ) ERROR STOP 33
      IF ( LBOUND(poly%C0,1) .NE. 1 ) ERROR STOP 34
      IF ( LBOUND(poly%R0,1) .NE. 1 ) ERROR STOP 35
      IF ( UBOUND(poly%A0,1) .NE. M ) ERROR STOP 36
      IF ( UBOUND(poly%C0,1) .NE. M ) ERROR STOP 37
      IF ( UBOUND(poly%R0,1) .NE. M ) ERROR STOP 38
      IF ( ANY(poly%A0 .NE. [(I, I = 1, M)]) ) ERROR STOP 39
      DO I = 1, M
        IF ( TRIM(poly%C0(I)) .NE. CHAR(I) ) ERROR STOP 40
        IF ( .NOT. precision_r4(poly%R0(I), I/REAL(M)) ) ERROR STOP 41
      END DO
      DEALLOCATE( poly )

      IF ( SIZE(c1%A0) .NE. M ) ERROR STOP 50
      IF ( SIZE(c1%C0) .NE. M ) ERROR STOP 51
      IF ( SIZE(c1%R0) .NE. M ) ERROR STOP 52
      IF ( LBOUND(c1%A0,1) .NE. 1 ) ERROR STOP 53
      IF ( LBOUND(c1%C0,1) .NE. 1 ) ERROR STOP 54
      IF ( LBOUND(c1%R0,1) .NE. 1 ) ERROR STOP 55
      IF ( UBOUND(c1%A0,1) .NE. M ) ERROR STOP 56
      IF ( UBOUND(c1%C0,1) .NE. M ) ERROR STOP 57
      IF ( UBOUND(c1%R0,1) .NE. M ) ERROR STOP 58
      IF ( ANY(c1%A0 .NE. [(2*I, I = 1, M)]) ) ERROR STOP 59
      DO I = 1, M
        IF ( TRIM(c1%C0(I)) .NE. CHAR(I) ) ERROR STOP 60
        IF ( .NOT. precision_r4(c1%R0(I), EXP(I/REAL(M))) ) ERROR STOP 61
      END DO

      IF ( ASSOCIATED(c1%cmp1) ) ERROR STOP 62
      ALLOCATE ( c1%cmp1, SOURCE = b1 )
      IF ( SIZE(c1%cmp1%A0) .NE. M ) ERROR STOP 63
      IF ( SIZE(c1%cmp1%C0) .NE. M ) ERROR STOP 64
      IF ( SIZE(c1%cmp1%R0) .NE. M ) ERROR STOP 65
      IF ( LBOUND(c1%cmp1%A0,1) .NE. 1 ) ERROR STOP 66
      IF ( LBOUND(c1%cmp1%C0,1) .NE. 1 ) ERROR STOP 67
      IF ( LBOUND(c1%cmp1%R0,1) .NE. 1 ) ERROR STOP 68
      IF ( UBOUND(c1%cmp1%A0,1) .NE. M ) ERROR STOP 69
      IF ( UBOUND(c1%cmp1%C0,1) .NE. M ) ERROR STOP 70
      IF ( UBOUND(c1%cmp1%R0,1) .NE. M ) ERROR STOP 71
      IF ( ANY(c1%cmp1%A0 .NE. [(I, I = 1, M)]) ) ERROR STOP 72
      DO I = 1, M
        IF ( TRIM(c1%cmp1%C0(I)) .NE. CHAR(I) ) ERROR STOP 73
        IF ( .NOT. precision_r4(c1%cmp1%R0(I), I/REAL(M))) ERROR STOP 74
      END DO

      ALLOCATE( poly, source = c1 )
      SELECT TYPE ( s => poly )
          TYPE IS (Child(4,*,4,*))
            IF ( SIZE(s%A0) .NE. M ) ERROR STOP 80
            IF ( SIZE(s%C0) .NE. M ) ERROR STOP 81
            IF ( SIZE(s%R0) .NE. M ) ERROR STOP 82
            IF ( LBOUND(s%A0,1) .NE. 1 ) ERROR STOP 83
            IF ( LBOUND(s%C0,1) .NE. 1 ) ERROR STOP 84
            IF ( LBOUND(s%R0,1) .NE. 1 ) ERROR STOP 85
            IF ( UBOUND(s%A0,1) .NE. M ) ERROR STOP 86
            IF ( UBOUND(s%C0,1) .NE. M ) ERROR STOP 87
            IF ( UBOUND(s%R0,1) .NE. M ) ERROR STOP 88
            IF ( ANY(s%A0 .NE. [(2*I, I = 1, M)]) ) ERROR STOP 89
            DO I = 1, M
              IF ( TRIM(s%C0(I)) .NE. CHAR(I) ) ERROR STOP 90
              IF ( .NOT. precision_r4(s%R0(I), EXP(I/REAL(M))) ) ERROR STOP 91
            END DO

            IF ( SIZE(s%cmp1%A0) .NE. M ) ERROR STOP 92
            IF ( SIZE(s%cmp1%C0) .NE. M ) ERROR STOP 93
            IF ( SIZE(s%cmp1%R0) .NE. M ) ERROR STOP 94
            IF ( LBOUND(s%cmp1%A0,1) .NE. 1 ) ERROR STOP 95
            IF ( LBOUND(s%cmp1%C0,1) .NE. 1 ) ERROR STOP 96
            IF ( LBOUND(s%cmp1%R0,1) .NE. 1 ) ERROR STOP 97
            IF ( UBOUND(s%cmp1%A0,1) .NE. M ) ERROR STOP 98
            IF ( UBOUND(s%cmp1%C0,1) .NE. M ) ERROR STOP 99
            IF ( UBOUND(s%cmp1%R0,1) .NE. M ) ERROR STOP 100
            IF ( ANY(s%cmp1%A0 .NE. [(I, I = 1, M)]) ) ERROR STOP 101
            DO I = 1, M
              IF ( TRIM(s%cmp1%C0(I)) .NE. CHAR(I) ) ERROR STOP 102
              IF ( .NOT. precision_r4(s%cmp1%R0(I), I/REAL(M))) ERROR STOP 103
            END DO

          CLASS DEFAULT
             STOP 104
      END SELECT
      DEALLOCATE( poly )

END PROGRAM DTP_PARAMETER_01a
