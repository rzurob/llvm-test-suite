!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : DTP_ACE_02.f
!*
!*  PROGRAMMER                 : Dorra Bouchiha
!*  DATE                       : April 24, 2009
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array constructor with Type Specification 
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
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

      TYPE,  EXTENDS(Child) :: NextGen (l3)
        INTEGER, LEN  :: l3 = 2

        INTEGER(k2) :: A31(l1) = -4, A32(l1+l2) = -5 
        TYPE(Base(k1,l3)) :: bcomp
      END TYPE

END MODULE
PROGRAM DTP_ACE_02
      USE Mod
      IMPLICIT NONE

      INTEGER :: I
      TYPE (Base(4,10)) :: b1(2), b2(10)
      TYPE (Child(4,3,4,3)) :: c1(5)
      TYPE (NextGen) :: n1(1)
      LOGICAL, EXTERNAL :: precision_r4

      b1 = [(Base(4,10)(), I=1,2)]
      IF ( SIZE(b1) .NE. 2 ) STOP 10
      DO I = 1, 2
        IF ( SIZE(b1(I)%A01) .NE.  10 ) STOP 11
        IF ( SIZE(b1(I)%A02) .NE.  11 ) STOP 12
        IF ( LBOUND(b1(I)%A01,1) .NE.  1 ) STOP 13
        IF ( LBOUND(b1(I)%A02,1) .NE.  1 ) STOP 14
        IF ( UBOUND(b1(I)%A01,1) .NE. 10 ) STOP 15
        IF ( UBOUND(b1(I)%A02,1) .NE. 11 ) STOP 16
        IF ( ANY(b1(I)%A01   .NE. -1) ) STOP 17
        IF ( ANY(b1(I)%A02   .NE. -2) ) STOP 18
        IF ( .NOT. precision_r4(b1(I)%R0, -0.1) ) STOP 19
      END DO

      b2 = [(Base(4,10)(2, 3, 0.2), I=1,10)]
      IF ( SIZE(b2) .NE. 10 ) STOP 20
      DO I = 1, 10
        IF ( SIZE(b2(I)%A01) .NE.  10 ) STOP 21
        IF ( SIZE(b2(I)%A02) .NE.  11 ) STOP 22
        IF ( LBOUND(b2(I)%A01,1) .NE.  1 ) STOP 23
        IF ( LBOUND(b2(I)%A02,1) .NE.  1 ) STOP 24
        IF ( UBOUND(b2(I)%A01,1) .NE. 10 ) STOP 25
        IF ( UBOUND(b2(I)%A02,1) .NE. 11 ) STOP 26
        IF ( ANY(b2(I)%A01 .NE. 2) ) STOP 27
        IF ( ANY(b2(I)%A02 .NE. 3) ) STOP 28
        IF ( .NOT. precision_r4(b2(I)%R0, 0.2) ) STOP 29
      END DO

      c1 = [(Child(4,3,4,3)(A01=10, A02=20, A2 = 30), I=1,5)]
      IF ( SIZE(c1) .NE. 5 ) STOP 30
      DO I = 1, 5
        IF ( SIZE(c1(I)%A01) .NE. 3 ) STOP 31
        IF ( SIZE(c1(I)%A02) .NE. 4 ) STOP 32
        IF ( SIZE(c1(I)%A2)  .NE. 3 ) STOP 33
        IF ( LBOUND(c1(I)%A01,1) .NE. 1 ) STOP 34
        IF ( LBOUND(c1(I)%A02,1) .NE. 1 ) STOP 35
        IF ( LBOUND(c1(I)%A2,1)  .NE. 1 ) STOP 36
        IF ( UBOUND(c1(I)%A01,1) .NE. 3 ) STOP 37
        IF ( UBOUND(c1(I)%A02,1) .NE. 4 ) STOP 38
        IF ( UBOUND(c1(I)%A2,1)  .NE. 3 ) STOP 39
        IF ( ANY(c1(I)%A01 .NE. 10) ) STOP 40
        IF ( ANY(c1(I)%A02 .NE. 20) ) STOP 41
        IF ( ANY(c1(I)%A2  .NE. 30) ) STOP 42
        IF ( .NOT. precision_r4(c1(I)%R0, -0.1) ) STOP 43
      END DO

      n1 = [(NextGen(11, 22, 0.12, 33, 44, 55, Base(4,2)() ), I=1,1)]
      IF ( SIZE(n1) .NE. 1 ) STOP 50
      IF ( SIZE(n1(1)%A01) .NE. 10 ) STOP 51
      IF ( SIZE(n1(1)%A02) .NE. 11 ) STOP 52
      IF ( SIZE(n1(1)%A2)  .NE.  5 ) STOP 53
      IF ( SIZE(n1(1)%A31) .NE. 10 ) STOP 54
      IF ( SIZE(n1(1)%A32) .NE. 15 ) STOP 55
      IF ( LBOUND(n1(1)%A01,1) .NE.  1 ) STOP 56
      IF ( LBOUND(n1(1)%A02,1) .NE.  1 ) STOP 57
      IF ( LBOUND(n1(1)%A2,1)  .NE.  1 ) STOP 58
      IF ( LBOUND(n1(1)%A31,1) .NE.  1 ) STOP 59
      IF ( LBOUND(n1(1)%A32,1) .NE.  1 ) STOP 60
      IF ( UBOUND(n1(1)%A01,1) .NE. 10 ) STOP 61
      IF ( UBOUND(n1(1)%A02,1) .NE. 11 ) STOP 62
      IF ( UBOUND(n1(1)%A2,1)  .NE.  5 ) STOP 63
      IF ( UBOUND(n1(1)%A31,1) .NE. 10 ) STOP 64
      IF ( UBOUND(n1(1)%A32,1) .NE. 15 ) STOP 65
      IF ( ANY(n1(1)%A01 .NE. 11) ) STOP 66
      IF ( ANY(n1(1)%A02 .NE. 22) ) STOP 67
      IF ( ANY(n1(1)%A2  .NE. 33) ) STOP 68
      IF ( ANY(n1(1)%A31 .NE. 44) ) STOP 69
      IF ( ANY(n1(1)%A32 .NE. 55) ) STOP 70
      IF ( .NOT. precision_r4(n1(1)%R0, 0.12) ) STOP 71

      IF ( n1(1)%bcomp%k1 .NE. n1(1)%k1 ) STOP 72
      IF ( n1(1)%bcomp%l1 .NE. n1(1)%l3 ) STOP 73
      IF ( SIZE(n1(1)%bcomp%A01) .NE. 2 ) STOP 74
      IF ( SIZE(n1(1)%bcomp%A02) .NE. 3 ) STOP 75
      IF ( LBOUND(n1(1)%bcomp%A01,1) .NE. 1 ) STOP 76
      IF ( LBOUND(n1(1)%bcomp%A02,1) .NE. 1 ) STOP 77
      IF ( UBOUND(n1(1)%bcomp%A01,1) .NE. 2 ) STOP 78
      IF ( UBOUND(n1(1)%bcomp%A02,1) .NE. 3 ) STOP 79
      IF ( ANY(n1(1)%bcomp%A01 .NE. -1) ) STOP 80
      IF ( ANY(n1(1)%bcomp%A02 .NE. -2) ) STOP 81
      IF ( .NOT. precision_r4(n1(1)%bcomp%R0, -0.1) ) STOP 82

END PROGRAM DTP_ACE_02
