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
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod
      IMPLICIT NONE

      TYPE Base (k1,l1)
        INTEGER, KIND :: k1 = 4
        INTEGER, LEN  :: l1 = 10

        INTEGER(k1)   :: A0(l1) = -1
        CHARACTER(l1) :: C0(l1) = 'XLF'
        INTEGER(k1)   :: A1(l1) = -2
      END TYPE

      TYPE,  EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2
        INTEGER, LEN  :: l2

        TYPE(Base(k2,l2)) :: cmp1
      END TYPE

      TYPE,  EXTENDS(Child) :: NextGen (l3)
        INTEGER, LEN  :: l3

        CLASS(Base(k1,l3)), ALLOCATABLE :: cmp2
      END TYPE
END MODULE

PROGRAM ExplicitInitExp04
      USE Mod

      INTEGER, PARAMETER :: M = 5, K = 4
      TYPE(Base), PARAMETER :: bconst = Base(K,2*M) &
                  ( [(I, I = 1, 2*M)], [(CHAR(I+64), I = 1, 2*M)], [(I, I = 1, 2*M)] )
      TYPE(NextGen(K,M,K,2*M,2*M)) :: n1 = NextGen(K,M,K,2*M,2*M) &
                  ( [(2*I, I = 1, M)], [(CHAR(I+64), I = 1, M)], [(I, I = 1, M)], bconst, NULL() )
      CLASS(Base(K,:)), POINTER :: poly

      IF ( SIZE(n1%A0) .NE. M ) STOP 10
      IF ( SIZE(n1%C0) .NE. M ) STOP 11
      IF ( SIZE(n1%A1) .NE. M ) STOP 12
      IF ( LBOUND(n1%A0,1) .NE. 1 ) STOP 13
      IF ( LBOUND(n1%C0,1) .NE. 1 ) STOP 14
      IF ( LBOUND(n1%A1,1) .NE. 1 ) STOP 15
      IF ( UBOUND(n1%A0,1) .NE. M ) STOP 16
      IF ( UBOUND(n1%C0,1) .NE. M ) STOP 17
      IF ( UBOUND(n1%A1,1) .NE. M ) STOP 18
      IF ( ANY(n1%A0 .NE.        [(2*I, I = 1, M)]) ) STOP 19
      IF ( ANY(n1%C0 .NE. [(CHAR(I+64), I = 1, M)]) ) STOP 20
      IF ( ANY(n1%A1 .NE.          [(I, I = 1, M)]) ) STOP 21

      IF ( SIZE(n1%cmp1%A0) .NE. 2*M ) STOP 22
      IF ( SIZE(n1%cmp1%C0) .NE. 2*M ) STOP 23
      IF ( SIZE(n1%cmp1%A1) .NE. 2*M ) STOP 24
      IF ( LBOUND(n1%cmp1%A0,1) .NE. 1 ) STOP 25
      IF ( LBOUND(n1%cmp1%C0,1) .NE. 1 ) STOP 26
      IF ( LBOUND(n1%cmp1%A1,1) .NE. 1 ) STOP 27
      IF ( UBOUND(n1%cmp1%A0,1) .NE. 2*M ) STOP 28
      IF ( UBOUND(n1%cmp1%C0,1) .NE. 2*M ) STOP 29
      IF ( UBOUND(n1%cmp1%A1,1) .NE. 2*M ) STOP 30
      IF ( ANY(n1%cmp1%A0 .NE.          [(I, I = 1, 2*M)]) ) STOP 31
      IF ( ANY(n1%cmp1%C0 .NE. [(CHAR(I+64), I = 1, 2*M)]) ) STOP 32
      IF ( ANY(n1%cmp1%A1 .NE.          [(I, I = 1, 2*M)]) ) STOP 33

      IF ( ALLOCATED(n1%cmp2) ) STOP 34

      ALLOCATE( n1%cmp2, source = bconst )
      IF ( SIZE(n1%cmp2%A0) .NE. 2*M ) STOP 35
      IF ( SIZE(n1%cmp2%C0) .NE. 2*M ) STOP 36
      IF ( SIZE(n1%cmp2%A1) .NE. 2*M ) STOP 37
      IF ( LBOUND(n1%cmp2%A0,1) .NE. 1 ) STOP 38
      IF ( LBOUND(n1%cmp2%C0,1) .NE. 1 ) STOP 39
      IF ( LBOUND(n1%cmp2%A1,1) .NE. 1 ) STOP 40
      IF ( UBOUND(n1%cmp2%A0,1) .NE. 2*M ) STOP 41
      IF ( UBOUND(n1%cmp2%C0,1) .NE. 2*M ) STOP 42
      IF ( UBOUND(n1%cmp2%A1,1) .NE. 2*M ) STOP 43
      IF ( ANY(n1%cmp2%A0 .NE.          [(I, I = 1, 2*M)]) ) STOP 44
      IF ( ANY(n1%cmp2%C0 .NE. [(CHAR(I+64), I = 1, 2*M)]) ) STOP 45
      IF ( ANY(n1%cmp2%A1 .NE.          [(I, I = 1, 2*M)]) ) STOP 46

      ALLOCATE( poly, source = n1 )
      SELECT TYPE ( poly )
          TYPE IS (NextGen(4,*,4,*,*))
             IF ( SIZE(poly%A0) .NE. M ) STOP 50
             IF ( SIZE(poly%C0) .NE. M ) STOP 51
             IF ( SIZE(poly%A1) .NE. M ) STOP 52
             IF ( LBOUND(poly%A0,1) .NE. 1 ) STOP 53
             IF ( LBOUND(poly%C0,1) .NE. 1 ) STOP 54
             IF ( LBOUND(poly%A1,1) .NE. 1 ) STOP 55
             IF ( UBOUND(poly%A0,1) .NE. M ) STOP 56
             IF ( UBOUND(poly%C0,1) .NE. M ) STOP 57
             IF ( UBOUND(poly%A1,1) .NE. M ) STOP 58
             IF ( ANY(poly%A0 .NE.        [(2*I, I = 1, M)]) ) STOP 59
             IF ( ANY(poly%C0 .NE. [(CHAR(I+64), I = 1, M)]) ) STOP 60
             IF ( ANY(poly%A1 .NE.          [(I, I = 1, M)]) ) STOP 61

             IF ( SIZE(poly%cmp1%A0) .NE. 2*M ) STOP 62
             IF ( SIZE(poly%cmp1%C0) .NE. 2*M ) STOP 63
             IF ( SIZE(poly%cmp1%A1) .NE. 2*M ) STOP 64
             IF ( LBOUND(poly%cmp1%A0,1) .NE. 1 ) STOP 65
             IF ( LBOUND(poly%cmp1%C0,1) .NE. 1 ) STOP 66
             IF ( LBOUND(poly%cmp1%A1,1) .NE. 1 ) STOP 67
             IF ( UBOUND(poly%cmp1%A0,1) .NE. 2*M ) STOP 68
             IF ( UBOUND(poly%cmp1%C0,1) .NE. 2*M ) STOP 69
             IF ( UBOUND(poly%cmp1%A1,1) .NE. 2*M ) STOP 70
             IF ( ANY(poly%cmp1%A0 .NE.          [(I, I = 1, 2*M)]) ) STOP 71
             IF ( ANY(poly%cmp1%C0 .NE. [(CHAR(I+64), I = 1, 2*M)]) ) STOP 72
             IF ( ANY(poly%cmp1%A1 .NE.          [(I, I = 1, 2*M)]) ) STOP 73

             IF ( .NOT. ALLOCATED(poly%cmp2) ) STOP 74

             IF ( SIZE(poly%cmp2%A0) .NE. 2*M ) STOP 75
             IF ( SIZE(poly%cmp2%C0) .NE. 2*M ) STOP 76
             IF ( SIZE(poly%cmp2%A1) .NE. 2*M ) STOP 77
             IF ( LBOUND(poly%cmp2%A0,1) .NE. 1 ) STOP 78
             IF ( LBOUND(poly%cmp2%C0,1) .NE. 1 ) STOP 79
             IF ( LBOUND(poly%cmp2%A1,1) .NE. 1 ) STOP 80
             IF ( UBOUND(poly%cmp2%A0,1) .NE. 2*M ) STOP 81
             IF ( UBOUND(poly%cmp2%C0,1) .NE. 2*M ) STOP 82
             IF ( UBOUND(poly%cmp2%A1,1) .NE. 2*M ) STOP 83
             IF ( ANY(poly%cmp2%A0 .NE.          [(I, I = 1, 2*M)]) ) STOP 84
             IF ( ANY(poly%cmp2%C0 .NE. [(CHAR(I+64), I = 1, 2*M)]) ) STOP 85
             IF ( ANY(poly%cmp2%A1 .NE.          [(I, I = 1, 2*M)]) ) STOP 86

          CLASS DEFAULT
             STOP 87
      END SELECT

END PROGRAM ExplicitInitExp04
