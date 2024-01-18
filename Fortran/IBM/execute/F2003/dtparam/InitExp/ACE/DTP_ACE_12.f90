!*  ===================================================================
!*
!*  DATE                       : April 24, 2009
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Array constructor with Type Specification
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*  Defect 339592
!*
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod
      TYPE Base (k1,l1)
        INTEGER, KIND :: k1 = 4
        INTEGER, LEN  :: l1 = 10

        INTEGER(k1) :: A01(1:l1-1) = -10, A02(-1:l1+1) = -20
      END TYPE

      TYPE :: Child (k1,l1)
        INTEGER, KIND :: k1 = 4
        INTEGER, LEN  :: l1 = 5

        INTEGER(k1) :: A2(l1) = -2
        TYPE(Base(k1,k1)) :: arr(4) = Base(k1,k1) (88, 77)
      END TYPE

      TYPE, EXTENDS(Child) :: NextGen
        INTEGER(k1) :: A3(2*l1) = -3
      END TYPE

END MODULE
PROGRAM DTP_ACE_12
      USE Mod
      IMPLICIT TYPE (Base) (b)
      IMPLICIT TYPE (Child) (c)
      IMPLICIT TYPE (NextGen) (n)

      IF ( b1%k1 .NE.  4 ) STOP 10
      IF ( b1%l1 .NE. 10 ) STOP 11
      IF ( SIZE(b1%A01)     .NE.    9 ) STOP 12
      IF ( LBOUND(b1%A01,1) .NE.    1 ) STOP 13
      IF ( UBOUND(b1%A01,1) .NE.    9 ) STOP 14
      IF ( SIZE(b1%A02)     .NE.   13 ) STOP 15
      IF ( LBOUND(b1%A02,1) .NE.   -1 ) STOP 16
      IF ( UBOUND(b1%A02,1) .NE.   11 ) STOP 17
      IF ( ANY(b1%A01       .NE. -10) ) STOP 18
      IF ( ANY(b1%A02       .NE. -20) ) STOP 19

      IF ( c1%k1 .NE. 4 ) STOP 20
      IF ( c1%l1 .NE. 5 ) STOP 21
      IF ( SIZE(c1%A2)     .NE.   5 ) STOP 22
      IF ( LBOUND(c1%A2,1) .NE.   1 ) STOP 23
      IF ( UBOUND(c1%A2,1) .NE.   5 ) STOP 24
      IF ( ANY(c1%A2       .NE. -2) ) STOP 25
      DO I = 1, c1%k1
         IF ( c1%arr(I)%k1 .NE. 4 ) STOP 26
         IF ( c1%arr(I)%l1 .NE. 4 ) STOP 27
         IF ( SIZE(c1%arr(I)%A01)     .NE.   3 ) STOP 28
         IF ( LBOUND(c1%arr(I)%A01,1) .NE.   1 ) STOP 29
         IF ( UBOUND(c1%arr(I)%A01,1) .NE.   3 ) STOP 30
         IF ( SIZE(c1%arr(I)%A02)     .NE.   7 ) STOP 31
         IF ( LBOUND(c1%arr(I)%A02,1) .NE.  -1 ) STOP 32
         IF ( UBOUND(c1%arr(I)%A02,1) .NE.   5 ) STOP 33
         IF ( ANY(c1%arr(I)%A01       .NE. 88) ) STOP 34
         IF ( ANY(c1%arr(I)%A02       .NE. 77) ) STOP 35
      END DO

      IF ( n1%k1 .NE. 4 ) STOP 40
      IF ( n1%l1 .NE. 5 ) STOP 41
      IF ( SIZE(n1%A2)     .NE.   5 ) STOP 42
      IF ( LBOUND(n1%A2,1) .NE.   1 ) STOP 43
      IF ( UBOUND(n1%A2,1) .NE.   5 ) STOP 44
      IF ( SIZE(n1%A3)     .NE.  10 ) STOP 45
      IF ( LBOUND(n1%A3,1) .NE.   1 ) STOP 46
      IF ( UBOUND(n1%A3,1) .NE.  10 ) STOP 47
      IF ( ANY(n1%A2       .NE. -2) ) STOP 48
      IF ( ANY(n1%A3       .NE. -3) ) STOP 49
      DO I = 1, n1%k1
         IF ( n1%arr(I)%k1 .NE. 4 ) STOP 50
         IF ( n1%arr(I)%l1 .NE. 4 ) STOP 51
         IF ( SIZE(n1%arr(I)%A01)     .NE.   3 ) STOP 52
         IF ( LBOUND(n1%arr(I)%A01,1) .NE.   1 ) STOP 53
         IF ( UBOUND(n1%arr(I)%A01,1) .NE.   3 ) STOP 54
         IF ( SIZE(n1%arr(I)%A02)     .NE.   7 ) STOP 55
         IF ( LBOUND(n1%arr(I)%A02,1) .NE.  -1 ) STOP 56
         IF ( UBOUND(n1%arr(I)%A02,1) .NE.   5 ) STOP 57
         IF ( ANY(n1%arr(I)%A01       .NE. 88) ) STOP 58
         IF ( ANY(n1%arr(I)%A02       .NE. 77) ) STOP 59
      END DO

END PROGRAM DTP_ACE_12
