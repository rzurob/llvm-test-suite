!*  ===================================================================
!*
!*                               DTP - Generic Type-Bound
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
!*  R448 type-bound-procedure-part is contains-stmt
!*                                     [ binding-private-stmt ]
!*                                     proc-binding-stmt
!*                                     [ proc-binding-stmt ] ...
!*
!*  R450 proc-binding-stmt is specific-binding
!*                         or generic-binding
!*                         or final-binding
!*
!*  R451 specific-binding is PROCEDURE [ (interface-name) ] &
!*                                    & [ [ , binding-attr -list ] :: ] &
!*                                    & binding-name [ => procedure-name ]
!*
!*  R452 generic-binding is GENERIC [, access-spec ] :: generic-spec => binding-name-list
!*
!* defect 340741
!234567890123456789012345678901234567890123456789012345678901234567890
      MODULE Mod1
      IMPLICIT NONE

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1

        INTEGER(k1), ALLOCATABLE :: my_arr(:)

        CONTAINS
         PROCEDURE :: A => SUB_BASE4
         PROCEDURE :: B => SUB_BASE8
         GENERIC :: SUB =>  A, B

      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (l2)
        INTEGER, LEN :: l2

        CLASS(Base(k1,l2)), POINTER :: Cmp
      END TYPE Child

      CONTAINS
!*
      SUBROUTINE SUB_BASE4(Obj)
        INTEGER :: K, N
        CLASS(Base(4,*)), INTENT(INOUT) :: Obj

        N = Obj%l1*Obj%k1

        Obj%my_arr = (/ (K, K = 1, N ) /)
      END SUBROUTINE

      SUBROUTINE SUB_BASE8(Obj)
        INTEGER :: K, N
        CLASS(Base(8,*)), INTENT(INOUT) :: Obj

        N = Obj%l1*Obj%k1

        Obj%my_arr = (/ (K, K = 1, N ) /)
      END SUBROUTINE

      END MODULE Mod1
!*
      PROGRAM Generic_TypeBound02a
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

      call base1%SUB ()
      test1 = SUM(base1%my_arr)

      call base2%SUB ()
      test2 = SUM(base2%my_arr)

      ALLOCATE(Base(4,5):: child1%Cmp)
      IF ( .NOT. ASSOCIATED(child1%Cmp)) ERROR STOP 10

      ALLOCATE(Base(8,5):: child2%Cmp)
      IF ( .NOT. ASSOCIATED(child2%Cmp)) ERROR STOP 11

      call child1%Cmp%SUB ()
      IF( SUM(child1%Cmp%my_arr) .NE. test1) ERROR STOP 12

      call child2%Cmp%SUB ()
      IF( SUM(child2%Cmp%my_arr) .NE. test2) ERROR STOP 13

      child1%Cmp => tgt1
      IF ( .NOT. ASSOCIATED(child1%Cmp)) ERROR STOP 14

      child2%Cmp => tgt2
      IF ( .NOT. ASSOCIATED(child2%Cmp)) ERROR STOP 15

      call child1%Cmp%SUB ()
      IF( SUM(child1%Cmp%my_arr) .NE. test1) ERROR STOP 16

      call child2%Cmp%SUB ()
      IF( SUM(child2%Cmp%my_arr) .NE. test2) ERROR STOP 17

      CALL SUB1(child1%Cmp)

      CALL SUB2(child2%Cmp)

      CONTAINS

      SUBROUTINE Sub1 (Arg)
      CLASS(Base(4,5)), POINTER ::  Arg

      ALLOCATE(Base(4,5):: Arg)
      IF ( .NOT. ASSOCIATED(Arg)) ERROR STOP 18

      call Arg%SUB ()
      IF( SUM(Arg%my_arr) .NE. test1) ERROR STOP 19

      Arg => tgt1
      IF ( .NOT. ASSOCIATED(Arg)) ERROR STOP 20

      call Arg%SUB ()
      IF( SUM(Arg%my_arr) .NE. test1) ERROR STOP 21

      END SUBROUTINE SUB1

      SUBROUTINE Sub2 (Arg)
      CLASS(Base(8,*)), POINTER ::  Arg


      !ALLOCATE(Base(8,*):: Arg)    ! problem with TYPESPEC see defect 340741
      ALLOCATE(Arg)
      IF ( .NOT. ASSOCIATED(Arg)) ERROR STOP 22

      call Arg%SUB ()
      IF( SUM(Arg%my_arr) .NE. test2) ERROR STOP 23

      Arg => tgt2
      IF ( .NOT. ASSOCIATED(Arg)) ERROR STOP 24

      call Arg%SUB ()
      IF( SUM(Arg%my_arr) .NE. test2) ERROR STOP 25

      END SUBROUTINE SUB2

      END PROGRAM Generic_TypeBound02a