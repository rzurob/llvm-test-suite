!*  ===================================================================
!*
!*  DATE                       : April 13, 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : Function result
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
!* Defect : 359977
!*
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod
      IMPLICIT NONE

      TYPE Base (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN  :: l1

        INTEGER(k1)   :: A0(l1) = -1
        CHARACTER(l1) :: C0(l1) = 'XLF'
        REAL(k1)      :: R0(l1) = -0.1

        CONTAINS
        PROCEDURE :: print => printBase
      END TYPE

      TYPE,  EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2
        INTEGER, LEN  :: l2

        TYPE(Base(k2,l2+1)) :: cmp1

        CONTAINS
        PROCEDURE :: print => printChild
      END TYPE

      CONTAINS

      SUBROUTINE printBase (Arg)
        CLASS(Base(4,*)), INTENT(IN) :: Arg

        print *, Arg%A0, Arg%C0, Arg%R0
      END SUBROUTINE

      SUBROUTINE printChild (Arg)
        CLASS(Child(4,*,4,*)), INTENT(IN) :: Arg

        print *, Arg%A0, Arg%C0, Arg%R0
        print *, Arg%cmp1%A0, Arg%cmp1%C0, Arg%cmp1%R0
      END SUBROUTINE

END MODULE

PROGRAM ExplicitInitExp03
      USE Mod

      INTEGER, PARAMETER :: M = 3, K = 4
      TYPE(Base(K,M)), PARAMETER :: bconst = Base(K,M) &
                  ( [(I, I = 1, M)], [(CHAR(I+64), I = 1, M)], [(I/REAL(M), I = 1, M)] )
      TYPE(Child(K,2*M,K,M-1)) :: c1 = Child(K,2*M,K,M-1) &
                  ( [(2*I, I = 1, 2*M)], [(CHAR(I+64), I = 1, 2*M)], [(COS(I/REAL(M)), I = 1, 2*M)], bconst )

      CLASS(Base(K,:)), POINTER :: poly
      LOGICAL(4), EXTERNAL :: precision_r4

      call bconst%print
      IF ( SIZE(bconst%A0) .NE. M ) ERROR STOP 10
      IF ( SIZE(bconst%C0) .NE. M ) ERROR STOP 11
      IF ( SIZE(bconst%R0) .NE. M ) ERROR STOP 12
      IF ( LBOUND(bconst%A0,1) .NE. 1 ) ERROR STOP 13
      IF ( LBOUND(bconst%C0,1) .NE. 1 ) ERROR STOP 14
      IF ( LBOUND(bconst%R0,1) .NE. 1 ) ERROR STOP 15
      IF ( UBOUND(bconst%A0,1) .NE. M ) ERROR STOP 16
      IF ( UBOUND(bconst%C0,1) .NE. M ) ERROR STOP 17
      IF ( UBOUND(bconst%R0,1) .NE. M ) ERROR STOP 18
      IF ( ANY(bconst%A0 .NE.          [(I, I = 1, M)]) ) ERROR STOP 19
      IF ( ANY(bconst%C0 .NE. [(CHAR(I+64), I = 1, M)]) ) ERROR STOP 20
      DO I = 1, M
        IF ( .NOT. precision_r4(bconst%R0(I), I/REAL(M)) ) ERROR STOP 21
      END DO

      ALLOCATE( poly, source = bconst)
      call poly%print
      IF ( SIZE(poly%A0) .NE. M ) ERROR STOP 22
      IF ( SIZE(poly%C0) .NE. M ) ERROR STOP 23
      IF ( SIZE(poly%R0) .NE. M ) ERROR STOP 24
      IF ( LBOUND(poly%A0,1) .NE. 1 ) ERROR STOP 25
      IF ( LBOUND(poly%C0,1) .NE. 1 ) ERROR STOP 26
      IF ( LBOUND(poly%R0,1) .NE. 1 ) ERROR STOP 27
      IF ( UBOUND(poly%A0,1) .NE. M ) ERROR STOP 28
      IF ( UBOUND(poly%C0,1) .NE. M ) ERROR STOP 29
      IF ( UBOUND(poly%R0,1) .NE. M ) ERROR STOP 30
      IF ( ANY(poly%A0 .NE.           [(I, I = 1, M)]) ) ERROR STOP 31
      IF ( ANY(poly%C0 .NE.  [(CHAR(I+64), I = 1, M)]) ) ERROR STOP 32
      DO I = 1, M
        IF ( .NOT. precision_r4(poly%R0(I), I/REAL(M)) ) ERROR STOP 33
      END DO

      poly%C0 = 'IBM'
      call poly%print
      IF ( SIZE(poly%A0) .NE. M ) ERROR STOP 34
      IF ( SIZE(poly%C0) .NE. M ) ERROR STOP 35
      IF ( SIZE(poly%R0) .NE. M ) ERROR STOP 36
      IF ( LBOUND(poly%A0,1) .NE. 1 ) ERROR STOP 37
      IF ( LBOUND(poly%C0,1) .NE. 1 ) ERROR STOP 38
      IF ( LBOUND(poly%R0,1) .NE. 1 ) ERROR STOP 39
      IF ( UBOUND(poly%A0,1) .NE. M ) ERROR STOP 40
      IF ( UBOUND(poly%C0,1) .NE. M ) ERROR STOP 41
      IF ( UBOUND(poly%R0,1) .NE. M ) ERROR STOP 42
      IF ( ANY(poly%A0 .NE.  [(I, I = 1, M)]) ) ERROR STOP 43
      IF ( ANY(poly%C0 .NE.            'IBM') ) ERROR STOP 44
      DO I = 1, M
        IF ( .NOT. precision_r4(poly%R0(I), I/REAL(M)) ) ERROR STOP 45
      END DO

      poly%R0 = [0.3, 0.33, 0.333]
      call poly%print
      IF ( SIZE(poly%A0) .NE. M ) ERROR STOP 46
      IF ( SIZE(poly%C0) .NE. M ) ERROR STOP 47
      IF ( SIZE(poly%R0) .NE. M ) ERROR STOP 48
      IF ( LBOUND(poly%A0,1) .NE. 1 ) ERROR STOP 50
      IF ( LBOUND(poly%C0,1) .NE. 1 ) ERROR STOP 51
      IF ( LBOUND(poly%R0,1) .NE. 1 ) ERROR STOP 52
      IF ( UBOUND(poly%A0,1) .NE. M ) ERROR STOP 53
      IF ( UBOUND(poly%C0,1) .NE. M ) ERROR STOP 54
      IF ( UBOUND(poly%R0,1) .NE. M ) ERROR STOP 55
      IF ( ANY(poly%A0 .NE.     [(I, I = 1, M)]) ) ERROR STOP 56
      IF ( ANY(poly%C0 .NE.               'IBM') ) ERROR STOP 57
      IF ( .NOT. precision_r4(poly%R0(1), 0.3)   ) ERROR STOP 58
      IF ( .NOT. precision_r4(poly%R0(2), 0.33)  ) ERROR STOP 58
      IF ( .NOT. precision_r4(poly%R0(3), 0.333) ) ERROR STOP 58

      call c1%print
      IF ( SIZE(c1%A0) .NE. 2*M ) ERROR STOP 59
      IF ( SIZE(c1%C0) .NE. 2*M ) ERROR STOP 60
      IF ( SIZE(c1%R0) .NE. 2*M ) ERROR STOP 61
      IF ( LBOUND(c1%A0,1) .NE. 1 ) ERROR STOP 62
      IF ( LBOUND(c1%C0,1) .NE. 1 ) ERROR STOP 63
      IF ( LBOUND(c1%R0,1) .NE. 1 ) ERROR STOP 64
      IF ( UBOUND(c1%A0,1) .NE. 2*M ) ERROR STOP 65
      IF ( UBOUND(c1%C0,1) .NE. 2*M ) ERROR STOP 66
      IF ( UBOUND(c1%R0,1) .NE. 2*M ) ERROR STOP 67
      IF ( ANY(c1%A0 .NE.            [(2*I, I = 1, 2*M)]) ) ERROR STOP 68
      IF ( ANY(c1%C0 .NE.     [(CHAR(I+64), I = 1, 2*M)]) ) ERROR STOP 69
      DO I = 1, 2*M
        IF ( .NOT. precision_r4(c1%R0(I), COS(I/REAL(M))) ) ERROR STOP 70
      END DO

      IF ( SIZE(c1%cmp1%A0) .NE. M ) ERROR STOP 71
      IF ( SIZE(c1%cmp1%C0) .NE. M ) ERROR STOP 72
      IF ( SIZE(c1%cmp1%R0) .NE. M ) ERROR STOP 73
      IF ( LBOUND(c1%cmp1%A0,1) .NE. 1 ) ERROR STOP 74
      IF ( LBOUND(c1%cmp1%C0,1) .NE. 1 ) ERROR STOP 75
      IF ( LBOUND(c1%cmp1%R0,1) .NE. 1 ) ERROR STOP 76
      IF ( UBOUND(c1%cmp1%A0,1) .NE. M ) ERROR STOP 77
      IF ( UBOUND(c1%cmp1%C0,1) .NE. M ) ERROR STOP 78
      IF ( UBOUND(c1%cmp1%R0,1) .NE. M ) ERROR STOP 79
      IF ( ANY(c1%cmp1%A0 .NE.          [(I, I = 1, M)]) ) ERROR STOP 80
      IF ( ANY(c1%cmp1%C0 .NE. [(CHAR(I+64), I = 1, M)]) ) ERROR STOP 81
      DO I = 1, M
        IF ( .NOT. precision_r4(c1%cmp1%R0(I), I/REAL(M))) ERROR STOP 82
      END DO

      ALLOCATE( poly, source = c1 )
      call poly%print
      SELECT TYPE ( poly )
          TYPE IS (Child(4,*,4,*))
            IF ( SIZE(poly%A0) .NE. 2*M ) ERROR STOP 83
            IF ( SIZE(poly%C0) .NE. 2*M ) ERROR STOP 84
            IF ( SIZE(poly%R0) .NE. 2*M ) ERROR STOP 85
            IF ( LBOUND(poly%A0,1) .NE. 1 ) ERROR STOP 86
            IF ( LBOUND(poly%C0,1) .NE. 1 ) ERROR STOP 87
            IF ( LBOUND(poly%R0,1) .NE. 1 ) ERROR STOP 88
            IF ( UBOUND(poly%A0,1) .NE. 2*M ) ERROR STOP 89
            IF ( UBOUND(poly%C0,1) .NE. 2*M ) ERROR STOP 90
            IF ( UBOUND(poly%R0,1) .NE. 2*M ) ERROR STOP 91
            IF ( ANY(poly%A0 .NE.            [(2*I, I = 1, 2*M)]) ) ERROR STOP 92
            IF ( ANY(poly%C0 .NE.     [(CHAR(I+64), I = 1, 2*M)]) ) ERROR STOP 93
            DO I = 1, 2*M
              IF ( .NOT. precision_r4(poly%R0(I), COS(I/REAL(M))) ) ERROR STOP 94
            END DO

            IF ( SIZE(poly%cmp1%A0) .NE. M ) ERROR STOP 95
            IF ( SIZE(poly%cmp1%C0) .NE. M ) ERROR STOP 96
            IF ( SIZE(poly%cmp1%R0) .NE. M ) ERROR STOP 97
            IF ( LBOUND(poly%cmp1%A0,1) .NE. 1 ) ERROR STOP 98
            IF ( LBOUND(poly%cmp1%C0,1) .NE. 1 ) ERROR STOP 99
            IF ( LBOUND(poly%cmp1%R0,1) .NE. 1 ) ERROR STOP 100
            IF ( UBOUND(poly%cmp1%A0,1) .NE. M ) ERROR STOP 101
            IF ( UBOUND(poly%cmp1%C0,1) .NE. M ) ERROR STOP 102
            IF ( UBOUND(c1%cmp1%R0,1) .NE. M ) ERROR STOP 103
            IF ( ANY(poly%cmp1%A0 .NE.          [(I, I = 1, M)]) ) ERROR STOP 104
            IF ( ANY(poly%cmp1%C0 .NE. [(CHAR(I+64), I = 1, M)]) ) ERROR STOP 105
            DO I = 1, M
              IF ( .NOT. precision_r4(poly%cmp1%R0(I), I/REAL(M))) ERROR STOP 106
            END DO

          CLASS DEFAULT
             STOP 107
      END SELECT

END PROGRAM ExplicitInitExp03
