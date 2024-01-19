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
        INTEGER, LEN  :: l1 = 1

        INTEGER(k1) :: A0 = -1
        REAL(k1)    :: R0 = -0.1
      END TYPE

      TYPE, EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2 = 4
        INTEGER, LEN  :: l2 = 1

        INTEGER(k2) :: A1 = -2
        REAL(k2)    :: R1 = -0.2
      END TYPE

      TYPE,  EXTENDS(Child) :: NextGen (k3,l3)
        INTEGER, KIND :: k3 = 4
        INTEGER, LEN  :: l3 = 1

        INTEGER(k3) :: A2 = -3
        REAL(k3)    :: R2 = -0.3
        TYPE(Base(k3,k3)) :: bcomp = Base(k3,k3)()
      END TYPE
END MODULE
PROGRAM DTP_ACE_01
      USE Mod
      IMPLICIT NONE

      INTEGER :: I
      TYPE (Base(4,10)) :: b1(2), b2(10)
      TYPE (Child(4,3,4,3)) :: c1(5)
      TYPE (NextGen) :: n1(1)
      LOGICAL, EXTERNAL :: precision_r4

      b1 = [(Base(4,10)(), I=1,2)]
      DO I = 1, 2
        IF ( b1(I)%A0 .NE. -1 ) ERROR STOP 10
        IF ( .NOT. precision_r4(b1(I)%R0, -0.1) ) ERROR STOP 11
      END DO

      b2 = [(Base(4,10)(2, 0.2), I=1,10)]
      DO I = 1, 10
        IF ( b2(I)%A0 .NE. 2 ) ERROR STOP 12
        IF ( .NOT. precision_r4(b2(I)%R0, 0.2) ) ERROR STOP 13
      END DO

      c1 = [(Child(4,3,4,3)(A0=10, A1=20), I=1,5)]
      DO I = 1, 5
        IF ( c1(I)%A0 .NE. 10 ) ERROR STOP 14
        IF ( c1(I)%A1 .NE. 20 ) ERROR STOP 15
        IF ( .NOT. precision_r4(c1(I)%R0, -0.1) ) ERROR STOP 16
        IF ( .NOT. precision_r4(c1(I)%R1, -0.2) ) ERROR STOP 17
      END DO

      n1 = [(NextGen(A0=11, A1=22, A2=33), I=1,1)]
      IF ( n1(1)%A0 .NE. 11 ) ERROR STOP 18
      IF ( n1(1)%A1 .NE. 22 ) ERROR STOP 19
      IF ( n1(1)%A2 .NE. 33 ) ERROR STOP 20
      IF ( .NOT. precision_r4(n1(1)%R0, -0.1) ) ERROR STOP 21
      IF ( .NOT. precision_r4(n1(1)%R1, -0.2) ) ERROR STOP 22
      IF ( .NOT. precision_r4(n1(1)%R2, -0.3) ) ERROR STOP 23

      IF ( n1(1)%bcomp%k1 .NE. n1(1)%k3 ) ERROR STOP 24
      IF ( n1(1)%bcomp%l1 .NE. n1(1)%k3 ) ERROR STOP 25
      IF ( n1(1)%bcomp%A0 .NE. -1 ) ERROR STOP 26
      IF ( .NOT. precision_r4(n1(1)%bcomp%R0, -0.1) ) ERROR STOP 27
END PROGRAM DTP_ACE_01
