!*  ===================================================================
!*
!*  DATE                       : March 25, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : Function result
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
        INTEGER, KIND :: k1
        INTEGER, LEN  :: l1

        INTEGER(k1) :: A0(1:l1), A1(0:2*l1), A2(2:l1,2:l1)
      END TYPE

      TYPE,  EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2
        INTEGER, LEN  :: l2

        TYPE(Base(k2,2*l2+1)) :: b_cmp
      END TYPE

      CONTAINS

      CLASS(Base(4,:)) FUNCTION BuildBase(K)
        INTEGER :: I, K
        POINTER :: BuildBase

        ALLOCATE( Base(4,K) :: BuildBase )

      END FUNCTION

      CLASS(Child(4,:,4,:)) FUNCTION BuildChild(K,L)
        INTEGER :: I, K, L
        POINTER :: BuildChild

        ALLOCATE( Child(4,K,4,L) :: BuildChild )

      END FUNCTION
END MODULE

PROGRAM FunctionResult05
      USE Mod
      IMPLICIT NONE

      TYPE(Base(4,:)), POINTER :: b1
      TYPE(Child(4,:,4,:)), POINTER :: c1
      CLASS(Base(4,:)), POINTER :: b2

      b1 => BuildBase(10)
      IF (b1%l1 .NE. 10) ERROR STOP 10
      IF (SIZE(b1%A0) .NE.  10) ERROR STOP 11
      IF (SIZE(b1%A1) .NE.  21) ERROR STOP 12
      IF (SIZE(b1%A2) .NE.  81) ERROR STOP 13

      c1 => BuildChild(2,4)
      IF (c1%l1 .NE. 2) ERROR STOP 14
      IF (c1%l2 .NE. 4) ERROR STOP 15
      IF (SIZE(c1%A0) .NE.  2) ERROR STOP 16
      IF (SIZE(c1%A1) .NE.  5) ERROR STOP 17
      IF (SIZE(c1%A2) .NE.  1) ERROR STOP 18
      IF (c1%b_cmp%l1 .NE. 9) ERROR STOP 19
      IF (SIZE(c1%b_cmp%A0) .NE. 9) ERROR STOP 20
      IF (SIZE(c1%b_cmp%A1) .NE. 19) ERROR STOP 21
      IF (SIZE(c1%b_cmp%A2) .NE. 64) ERROR STOP 22

      b2 => BuildBase(20)
      IF (b2%l1 .NE. 20) ERROR STOP 23
      IF (SIZE(b2%A0) .NE.  20) ERROR STOP 24
      IF (SIZE(b2%A1) .NE.  41) ERROR STOP 25
      IF (SIZE(b2%A2) .NE. 361) ERROR STOP 26

      b2 => BuildChild(3,6)
      SELECT TYPE ( b2 )
        CLASS IS (Child(4,*,4,*))
          IF (b2%l1 .NE. 3) ERROR STOP 27
          IF (b2%l2 .NE. 6) ERROR STOP 28
          IF (SIZE(b2%A0) .NE.  3) ERROR STOP 29
          IF (SIZE(b2%A1) .NE.  7) ERROR STOP 30
          IF (SIZE(b2%A2) .NE.  4) ERROR STOP 31
          IF (b2%b_cmp%l1 .NE. 13) ERROR STOP 32
          IF (SIZE(b2%b_cmp%A0) .NE.  13) ERROR STOP 33
          IF (SIZE(b2%b_cmp%A1) .NE.  27) ERROR STOP 34
          IF (SIZE(b2%b_cmp%A2) .NE. 144) ERROR STOP 35

        CLASS DEFAULT
           STOP 36
      END SELECT

END PROGRAM FunctionResult05
