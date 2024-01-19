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
!* Defect 362586
!*
!234567890123456789012345678901234567890123456789012345678901234567890

MODULE Mod
      IMPLICIT NONE

      TYPE Base (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN  :: l1

        INTEGER(k1) :: A0(1:l1), A1(0:2*l1), A2(l1,l1)

        CONTAINS

        PROCEDURE :: print => printBase
      END TYPE

      TYPE,  EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2
        INTEGER, LEN  :: l2

        TYPE(Base(k2,2*l2+1)) :: cmp

        CONTAINS

        PROCEDURE :: print => printChild
      END TYPE

      CONTAINS

      FUNCTION BuildBase(K)
        CLASS(Base(4,:)), POINTER :: BuildBase
        INTEGER :: I, J, K

!        ALLOCATE( BuildBase, SOURCE = Base(4,K) ( (/ (1, I = 1, K) /),      &
!             (/ (2, I = 0, 2*K) /), RESHAPE([((3, J=1,K), I=1,K)], [K,K]) ) )
        allocate (base(4,k) :: buildbase)

        buildbase%a0 = (/ (1, I = 1, K) /)
        buildbase%a1 = 2
        buildbase%a2 = 3

      END FUNCTION

      FUNCTION BuildChild(K,L)
        CLASS(Child(4,:,4,:)), POINTER :: BuildChild
        INTEGER :: I, J, K, L, N

        N = 2*L + 1

        allocate (child(4,k,4,l) :: buildChild)

        buildChild%a0 = 4
        buildChild%a1 = 5
        buildChild%a2 = 6
!        buildChild%cmp = Base(4,N) ( 7, 8,9)
        buildChild%cmp%a0 = 7
        buildChild%cmp%a1 = 8
        buildChild%cmp%a2 = 9

      END FUNCTION

      SUBROUTINE printBase (arg)
        CLASS(Base(4,*)), INTENT(IN) :: arg

        print *, arg%A0
        print *, arg%A1
        print *, arg%A2
      END SUBROUTINE

      SUBROUTINE printChild (arg)
        CLASS(Child(4,*,4,*)), INTENT(IN) :: arg

        print *, arg%A0
        print *, arg%A1
        print *, arg%A2
        call arg%cmp%print
      END SUBROUTINE

END MODULE

PROGRAM FunctionResult09
      USE Mod
      IMPLICIT NONE

      TYPE(Base(4,:)), POINTER :: b1
      TYPE(Child(4,:,4,:)), POINTER :: c1
      CLASS(Base(4,:)), POINTER :: b2

      b1 => BuildBase(10)
      call b1%print
      IF (b1%l1 .NE. 10) ERROR STOP 10
      IF (SIZE(b1%A0) .NE.  10) ERROR STOP 11
      IF (SIZE(b1%A1) .NE.  21) ERROR STOP 12
      IF (SIZE(b1%A2) .NE. 100) ERROR STOP 13
      IF (ANY(b1%A0   .NE.  1)) ERROR STOP 14
      IF (ANY(b1%A1   .NE.  2)) ERROR STOP 15
      IF (ANY(b1%A2   .NE.  3)) ERROR STOP 16

      c1 => BuildChild(2,4)
      call c1%print
      IF (c1%l1 .NE. 2) ERROR STOP 20
      IF (c1%l2 .NE. 4) ERROR STOP 21
      IF (SIZE(c1%A0) .NE.  2) ERROR STOP 22
      IF (SIZE(c1%A1) .NE.  5) ERROR STOP 23
      IF (SIZE(c1%A2) .NE.  4) ERROR STOP 24
      IF (ANY(c1%A0   .NE.  4)) ERROR STOP 25
      IF (ANY(c1%A1   .NE.  5)) ERROR STOP 26
      IF (ANY(c1%A2   .NE.  6)) ERROR STOP 27

      IF (c1%cmp%l1 .NE. 9) ERROR STOP 28
      IF (SIZE(c1%cmp%A0) .NE.  9) ERROR STOP 29
      IF (SIZE(c1%cmp%A1) .NE. 19) ERROR STOP 30
      IF (SIZE(c1%cmp%A2) .NE. 81) ERROR STOP 31
      IF (ANY(c1%cmp%A0   .NE. 7)) ERROR STOP 32
      IF (ANY(c1%cmp%A1   .NE. 8)) ERROR STOP 33
      IF (ANY(c1%cmp%A2   .NE. 9)) ERROR STOP 34

      b2 => BuildBase(20)
      call b2%print
      IF (b2%l1 .NE. 20) ERROR STOP 40
      IF (SIZE(b2%A0) .NE.  20) ERROR STOP 41
      IF (SIZE(b2%A1) .NE.  41) ERROR STOP 42
      IF (SIZE(b2%A2) .NE. 400) ERROR STOP 43
      IF (ANY(b2%A0   .NE.  1)) ERROR STOP 44
      IF (ANY(b2%A1   .NE.  2)) ERROR STOP 45
      IF (ANY(b2%A2   .NE.  3)) ERROR STOP 46

      b2 => BuildChild(3,6)
      call b2%print
      SELECT TYPE ( b2 )
        CLASS IS (Child(4,*,4,*))
          IF (b2%l1 .NE. 3) ERROR STOP 50
          IF (b2%l2 .NE. 6) ERROR STOP 51
          IF (SIZE(b2%A0) .NE.  3) ERROR STOP 52
          IF (SIZE(b2%A1) .NE.  7) ERROR STOP 53
          IF (SIZE(b2%A2) .NE.  9) ERROR STOP 54
          IF (ANY(b2%A0   .NE.  4)) ERROR STOP 55
          IF (ANY(b2%A1   .NE.  5)) ERROR STOP 56
          IF (ANY(b2%A2   .NE.  6)) ERROR STOP 57

          IF (b2%cmp%l1 .NE. 13) ERROR STOP 58
          IF (SIZE(b2%cmp%A0) .NE.  13) ERROR STOP 59
          IF (SIZE(b2%cmp%A1) .NE.  27) ERROR STOP 60
          IF (SIZE(b2%cmp%A2) .NE. 169) ERROR STOP 61
          IF (ANY(b2%cmp%A0   .NE.  7)) ERROR STOP 62
          IF (ANY(b2%cmp%A1   .NE.  8)) ERROR STOP 63
          IF (ANY(b2%cmp%A2   .NE.  9)) ERROR STOP 64

        CLASS DEFAULT
           STOP 65
      END SELECT
END PROGRAM FunctionResult09
