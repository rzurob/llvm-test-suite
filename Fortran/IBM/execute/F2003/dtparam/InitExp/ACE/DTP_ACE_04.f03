!*  ===================================================================
!*
!*  DATE                       : April 24, 2009
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
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod
      IMPLICIT NONE

      TYPE Base (k1,l1)
        INTEGER, KIND :: k1 = 4
        INTEGER, LEN  :: l1 = 10

        INTEGER(k1) :: A01(l1) = -1, A02(l1+1) = -2
        REAL(k1)    :: R0 = -0.1
      END TYPE

      TYPE, EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2 = 4
        INTEGER, LEN  :: l2 = 5

        INTEGER(k2) :: A2(l2) = -3
      END TYPE
END MODULE
PROGRAM DTP_ACE_04
      USE Mod
      IMPLICIT NONE

      INTEGER :: I
      TYPE(Base(4,10)) :: b1(2)
      TYPE(Child(4,3,4,3)) :: c1 =  Child(4,3,4,3)( [(1, I = 1, 3)],      &
         [(2, I = 1, 4)], 0.99, [(I, I = 1, 3)] ), c2 =  Child(4,3,4,3)   &
         ( A01=10, A02=20, A2=30 ), carr(5)
      LOGICAL, EXTERNAL :: precision_r4

      b1 = [Base(4,10) :: Base(4,10)(), Base(4,10)(2, 3, 0.2)]
      IF ( SIZE(b1) .NE. 2 ) ERROR STOP 10

      IF ( SIZE(b1(1)%A01) .NE.  10 ) ERROR STOP 11
      IF ( SIZE(b1(1)%A02) .NE.  11 ) ERROR STOP 12
      IF ( SIZE(b1(2)%A01) .NE.  10 ) ERROR STOP 13
      IF ( SIZE(b1(2)%A02) .NE.  11 ) ERROR STOP 14

      IF ( LBOUND(b1(1)%A01,1) .NE.  1 ) ERROR STOP 15
      IF ( LBOUND(b1(1)%A02,1) .NE.  1 ) ERROR STOP 16
      IF ( LBOUND(b1(2)%A01,1) .NE.  1 ) ERROR STOP 17
      IF ( LBOUND(b1(2)%A02,1) .NE.  1 ) ERROR STOP 18

      IF ( UBOUND(b1(1)%A01,1) .NE. 10 ) ERROR STOP 19
      IF ( UBOUND(b1(1)%A02,1) .NE. 11 ) ERROR STOP 20
      IF ( UBOUND(b1(2)%A01,1) .NE. 10 ) ERROR STOP 21
      IF ( UBOUND(b1(2)%A02,1) .NE. 11 ) ERROR STOP 22

      IF ( ANY(b1(1)%A01 .NE. -1) ) ERROR STOP 23
      IF ( ANY(b1(1)%A02 .NE. -2) ) ERROR STOP 24
      IF ( ANY(b1(2)%A01 .NE.  2) ) ERROR STOP 25
      IF ( ANY(b1(2)%A02 .NE.  3) ) ERROR STOP 26
      IF ( .NOT. precision_r4(b1(1)%R0, -0.1) ) ERROR STOP 27
      IF ( .NOT. precision_r4(b1(2)%R0,  0.2) ) ERROR STOP 28

      carr = [Child(4,3,4,3) :: Child(4,3,4,3)(), c1, Child(4,3,4,3)(), c2, Child(4,3,4,3)()]
      IF ( SIZE(carr) .NE. 5 ) ERROR STOP 30
      DO I = 1, 5
        IF ( SIZE(carr(I)%A01) .NE. 3 ) ERROR STOP 31
        IF ( SIZE(carr(I)%A02) .NE. 4 ) ERROR STOP 32
        IF ( SIZE(carr(I)%A2)  .NE. 3 ) ERROR STOP 33
        IF ( LBOUND(carr(I)%A01,1) .NE. 1 ) ERROR STOP 34
        IF ( LBOUND(carr(I)%A02,1) .NE. 1 ) ERROR STOP 35
        IF ( LBOUND(carr(I)%A2,1)  .NE. 1 ) ERROR STOP 36
        IF ( UBOUND(carr(I)%A01,1) .NE. 3 ) ERROR STOP 37
        IF ( UBOUND(carr(I)%A02,1) .NE. 4 ) ERROR STOP 38
        IF ( UBOUND(carr(I)%A2,1)  .NE. 3 ) ERROR STOP 39
      END DO
      IF ( ANY(carr(1)%A01 .NE. -1) ) ERROR STOP 40
      IF ( ANY(carr(2)%A01 .NE.  1) ) ERROR STOP 41
      IF ( ANY(carr(3)%A01 .NE. -1) ) ERROR STOP 42
      IF ( ANY(carr(4)%A01 .NE. 10) ) ERROR STOP 43
      IF ( ANY(carr(5)%A01 .NE. -1) ) ERROR STOP 44

      IF ( ANY(carr(1)%A02 .NE. -2) ) ERROR STOP 45
      IF ( ANY(carr(2)%A02 .NE.  2) ) ERROR STOP 46
      IF ( ANY(carr(3)%A02 .NE. -2) ) ERROR STOP 47
      IF ( ANY(carr(4)%A02 .NE. 20) ) ERROR STOP 48
      IF ( ANY(carr(5)%A02 .NE. -2) ) ERROR STOP 49

      IF ( ANY(carr(1)%A2  .NE.        -3) ) ERROR STOP 50
      IF ( ANY(carr(2)%A2  .NE. [1, 2, 3]) ) ERROR STOP 51
      IF ( ANY(carr(3)%A2  .NE.        -3) ) ERROR STOP 52
      IF ( ANY(carr(4)%A2  .NE.        30) ) ERROR STOP 53
      IF ( ANY(carr(5)%A2  .NE.        -3) ) ERROR STOP 54

      IF ( .NOT. precision_r4(carr(1)%R0, -0.1) ) ERROR STOP 55
      IF ( .NOT. precision_r4(carr(2)%R0, 0.99) ) ERROR STOP 56
      IF ( .NOT. precision_r4(carr(3)%R0, -0.1) ) ERROR STOP 57
      IF ( .NOT. precision_r4(carr(4)%R0, -0.1) ) ERROR STOP 58
      IF ( .NOT. precision_r4(carr(5)%R0, -0.1) ) ERROR STOP 59

END PROGRAM DTP_ACE_04