!*  ===================================================================
!*
!*                               DTP - Generic Interface
!*
!*  DATE                       : October 02, 2008
!*
!*  PRIMARY SUBTIONS TESTED   : Generic Resolution - Derived-type parameters
!*  SECONDARY SUBTIONS TESTED : Resolution based on KIND type parameter
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : GENERIC
!*
!*  DESCRIPTION                :
!*
!* defect 340741
!234567890123456789012345678901234567890123456789012345678901234567890
      MODULE Mod1
      IMPLICIT NONE

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1

        INTEGER(k1), ALLOCATABLE :: my_arr(:)
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (l2)
        INTEGER, LEN :: l2

        CLASS(Base(k1,l2)), POINTER :: Cmp
      END TYPE Child

      INTERFACE SUB
        SUBROUTINE SUB_BASE4(Obj)
          IMPORT BASE
          INTEGER :: K, N
          CLASS(Base(4,*)), INTENT(INOUT) :: Obj
        END SUBROUTINE
        SUBROUTINE SUB_BASE8(Obj)
          IMPORT BASE
          INTEGER :: K, N
          CLASS(Base(8,*)), INTENT(INOUT) :: Obj
        END SUBROUTINE
      END INTERFACE

      END MODULE Mod1
!*
      SUBROUTINE SUB_BASE4(Obj)
        USE MOD1, ONLY: BASE
        INTEGER :: K, N
        CLASS(Base(4,*)), INTENT(INOUT) :: Obj

        N = Obj%l1*Obj%k1

        Obj%my_arr = (/ (K, K = 1, N ) /)
      END SUBROUTINE

      SUBROUTINE SUB_BASE8(Obj)
        USE MOD1, ONLY: BASE
        INTEGER :: K, N
        CLASS(Base(8,*)), INTENT(INOUT) :: Obj

        N = Obj%l1*Obj%k1

        Obj%my_arr = (/ (K, K = 1, N ) /)
      END SUBROUTINE

!*
      PROGRAM Generic_Interface02a
      USE MOD1
      IMPLICIT NONE

      TYPE(Child(4,10,5)) :: child1
      TYPE(Child(8,10,5)) :: child2
      TYPE(Child(4,5,10)), TARGET :: tgt1
      TYPE(Child(8,5,10)), TARGET :: tgt2
      TYPE(Base(4,5))  :: base1
      TYPE(Base(8,5))  :: base2
      INTEGER :: test1, test2

! The value returned by SUB depends on the KIND*LEN to make sure we pick the
! right function when we distinguih by KIND type parameter

      call SUB(base1)
      test1 = SUM(base1%my_arr)

      call SUB(base2)
      test2 = SUM(base2%my_arr)

      ALLOCATE(Base(4,5):: child1%Cmp)
      IF ( .NOT. ASSOCIATED(child1%Cmp)) ERROR STOP 10

      ALLOCATE(Base(8,5):: child2%Cmp)
      IF ( .NOT. ASSOCIATED(child2%Cmp)) ERROR STOP 11

      call SUB(child1%Cmp)
      IF( SUM(child1%Cmp%my_arr) .NE. test1) ERROR STOP 12

      call SUB(child2%Cmp)
      IF( SUM(child2%Cmp%my_arr) .NE. test2) ERROR STOP 13

      child1%Cmp => tgt1
      IF ( .NOT. ASSOCIATED(child1%Cmp)) ERROR STOP 14

      child2%Cmp => tgt2
      IF ( .NOT. ASSOCIATED(child2%Cmp)) ERROR STOP 15

      call SUB(child1%Cmp)
      IF( SUM(child1%Cmp%my_arr) .NE. test1) ERROR STOP 16

      call SUB(child2%Cmp)
      IF( SUM(child2%Cmp%my_arr) .NE. test2) ERROR STOP 17

      CALL SUB1(child1%Cmp)

      CALL SUB2(child2%Cmp)

      CONTAINS

      SUBROUTINE Sub1 (Arg)
      CLASS(Base(4,5)), POINTER ::  Arg

      ALLOCATE(Base(4,5):: Arg)
      IF ( .NOT. ASSOCIATED(Arg)) ERROR STOP 18

      call SUB(Arg)
      IF( SUM(Arg%my_arr) .NE. test1) ERROR STOP 19

      Arg => tgt1
      IF ( .NOT. ASSOCIATED(Arg)) ERROR STOP 20

      call SUB(Arg)
      IF( SUM(Arg%my_arr) .NE. test1) ERROR STOP 21

      END SUBROUTINE SUB1

      SUBROUTINE Sub2 (Arg)
      CLASS(Base(8,*)), POINTER ::  Arg


      !ALLOCATE(Base(8,*):: Arg)    ! problem with TYPESPEC see defect 340741
      ALLOCATE(Arg)
      IF ( .NOT. ASSOCIATED(Arg)) ERROR STOP 22

      call SUB(Arg)
      IF( SUM(Arg%my_arr) .NE. test2) ERROR STOP 23

      Arg => tgt2
      IF ( .NOT. ASSOCIATED(Arg)) ERROR STOP 24

      call SUB(Arg)
      IF( SUM(Arg%my_arr) .NE. test2) ERROR STOP 25

      END SUBROUTINE SUB2

      END PROGRAM Generic_Interface02a
