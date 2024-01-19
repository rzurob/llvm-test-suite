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
!* Defect : 362586
!*
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod
      IMPLICIT NONE

      TYPE Base (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN  :: l1

        INTEGER(k1) :: A0(1:l1), A1(0:2*l1)

        CONTAINS
        PROCEDURE :: print => printBase
      END TYPE

      TYPE,  EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2
        INTEGER, LEN  :: l2

        TYPE(Base(k2,l2)) :: cmp

        CONTAINS
        PROCEDURE :: print => printChild
      END TYPE

      CONTAINS

      CLASS(Base(4,:)) FUNCTION BuildBase(Arg)
        CLASS(Base(4,*)) :: Arg
        POINTER :: BuildBase

        ALLOCATE( BuildBase, SOURCE = Arg )

      END FUNCTION

      CLASS(Child(4,:,4,:)) FUNCTION BuildChild(Arg)
        CLASS(Child(4,*,4,*)) :: Arg
        POINTER :: BuildChild

        ALLOCATE( BuildChild, SOURCE = Arg )

      END FUNCTION

      SUBROUTINE printBase (Arg)
        CLASS(Base(4,*)), INTENT(IN) :: Arg

        print *, Arg%A0
        print *, Arg%A1
      END SUBROUTINE

      SUBROUTINE printChild (Arg)
        CLASS(Child(4,*,4,*)), INTENT(IN) :: Arg

        print *, Arg%A0
        print *, Arg%A1
        print *, Arg%cmp%A0
        print *, Arg%cmp%A1
      END SUBROUTINE

END MODULE

PROGRAM ExplicitInitExp01
      USE Mod
      IMPLICIT NONE
      INTEGER, PARAMETER :: K =10 , N = 6, M = 3
      INTEGER :: I, J
      TYPE(Base(4,K)) :: b1 = Base(4,K) ( [(1, I = 1, K)], [(2, I = 0, 2*K)] )
      TYPE(Child(4,M,4,N)) :: c1 =  Child(4,M,4,N) ( [(3, I = 1, M)] , &
        [(4, I = 0, 2*M)], Base(4,N) ( [(5, I = 1, N)] , [(6, I = 0, 2*N)] ) )
      CLASS(Base(4,:)), POINTER :: upoly

      upoly => BuildBase(b1)
      IF (b1%l1 .NE. K) ERROR STOP 10
      IF (SIZE(b1%A0) .NE.   K) ERROR STOP 11
      IF (SIZE(b1%A1) .NE.  21) ERROR STOP 12
      IF (ANY(b1%A0   .NE.  1)) ERROR STOP 13
      IF (ANY(b1%A1   .NE.  2)) ERROR STOP 14

      call upoly%print

      SELECT TYPE ( upoly )
        CLASS IS (Base(4,*))
          IF (upoly%l1 .NE. K) ERROR STOP 15
          IF (SIZE(upoly%A0) .NE.   K) ERROR STOP 16
          IF (SIZE(upoly%A1) .NE.  21) ERROR STOP 17
          IF (ANY(upoly%A0   .NE.  1)) ERROR STOP 18
          IF (ANY(upoly%A1   .NE.  2)) ERROR STOP 19

        CLASS DEFAULT
           STOP 20
      END SELECT

      upoly => BuildChild(c1)
      IF (c1%l1 .NE. M) ERROR STOP 21
      IF (c1%l2 .NE. N) ERROR STOP 22
      IF (SIZE(c1%A0) .NE.  M) ERROR STOP 23
      IF (SIZE(c1%A1) .NE.  7) ERROR STOP 24
      IF (ANY(c1%A0   .NE. 3)) ERROR STOP 25
      IF (ANY(c1%A1   .NE. 4)) ERROR STOP 26

      IF (c1%cmp%l1 .NE. N) ERROR STOP 27
      IF (SIZE(c1%cmp%A0) .NE.   N) ERROR STOP 28
      IF (SIZE(c1%cmp%A1) .NE.  13) ERROR STOP 29
      IF (ANY(c1%cmp%A0   .NE.  5)) ERROR STOP 30
      IF (ANY(c1%cmp%A1   .NE.  6)) ERROR STOP 31

      call upoly%print

      SELECT TYPE ( upoly )
        CLASS IS (Child(4,*,4,*))
          IF (upoly%l1 .NE. M) ERROR STOP 32
          IF (upoly%l2 .NE. N) ERROR STOP 33
          IF (SIZE(upoly%A0) .NE.  M) ERROR STOP 34
          IF (SIZE(upoly%A1) .NE.  7) ERROR STOP 35
          IF (ANY(upoly%A0   .NE. 3)) ERROR STOP 36
          IF (ANY(upoly%A1   .NE. 4)) ERROR STOP 37

          IF (upoly%cmp%l1 .NE. N) ERROR STOP 38
          IF (SIZE(upoly%cmp%A0) .NE.   N) ERROR STOP 39
          IF (SIZE(upoly%cmp%A1) .NE.  13) ERROR STOP 40
          IF (ANY(upoly%cmp%A0   .NE.  5)) ERROR STOP 41
          IF (ANY(upoly%cmp%A1   .NE.  6)) ERROR STOP 42

        CLASS DEFAULT
           STOP 43
      END SELECT
END PROGRAM ExplicitInitExp01
