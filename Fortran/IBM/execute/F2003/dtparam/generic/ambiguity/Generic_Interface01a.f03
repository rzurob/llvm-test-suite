!*  ===================================================================
!*
!*                               DTP - Generic Interface
!*
!*  DATE                       : October 02, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : Generic Resolution - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : Resolution based on KIND type parameter
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : GENERIC
!*
!*  DESCRIPTION                :
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      MODULE Mod1
      IMPLICIT NONE

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (l2)
        INTEGER, LEN :: l2
        CLASS(Base(k1,l2)), POINTER :: Cmp
      END TYPE Child

      INTERFACE FUNC
         INTEGER FUNCTION FUNC_BASE4(Obj)
           IMPORT BASE
           INTEGER :: K, N
           CLASS(Base(4,*)), INTENT(IN) :: Obj
         END FUNCTION
         INTEGER FUNCTION FUNC_BASE8(Obj)
           IMPORT BASE
           INTEGER :: K, N
           CLASS(Base(8,*)), INTENT(IN) :: Obj
         END FUNCTION
      END INTERFACE

      END MODULE Mod1

      INTEGER FUNCTION FUNC_BASE4(Obj)
        USE MOD1, ONLY: BASE
        INTEGER :: K, N
        CLASS(Base(4,*)), INTENT(IN) :: Obj

        N = Obj%k1

        FUNC_BASE4 = PRODUCT ((/ (K, K = 2, N ) /)) ! 24
      END FUNCTION

      INTEGER FUNCTION FUNC_BASE8(Obj)
        USE MOD1, ONLY: BASE
        INTEGER :: K, N
        CLASS(Base(8,*)), INTENT(IN) :: Obj

        N = Obj%k1

        FUNC_BASE8 = PRODUCT ((/ (K, K = 2, N ) /)) ! 40320
      END FUNCTION
!*
      PROGRAM Generic_Interface01a
      USE MOD1
      IMPLICIT NONE

      TYPE(Child(4,10,5)) :: child1
      TYPE(Child(8,10,5)) :: child2
      TYPE(Child(4,5,10)), TARGET :: tgt1
      TYPE(Child(8,5,10)), TARGET :: tgt2
      TYPE(Base(4,5))  :: base1
      TYPE(Base(8,5))  :: base2
      INTEGER :: test1, test2

! The value of FUNC depends on the KIND type parameter to make sure we pick the right function
! when we distinguih by KIND type parameter

      test1 = FUNC(base1)  ! 24
      test2 = FUNC(base2)  ! 40320

      ALLOCATE(Base(4,5):: child1%Cmp)
      IF ( .NOT. ASSOCIATED(child1%Cmp)) ERROR STOP 10
      ALLOCATE(Base(8,5):: child2%Cmp)
      IF ( .NOT. ASSOCIATED(child2%Cmp)) ERROR STOP 11

      IF( test1 .NE. FUNC(child1%Cmp)) ERROR STOP 20
      IF( test2 .NE. FUNC(child2%Cmp)) ERROR STOP 21

      child1%Cmp => tgt1
      IF ( .NOT. ASSOCIATED(child1%Cmp)) ERROR STOP 12
      child2%Cmp => tgt2
      IF ( .NOT. ASSOCIATED(child2%Cmp)) ERROR STOP 13

      IF( test1 .NE. FUNC(child1%Cmp)) ERROR STOP 22
      IF( test2 .NE. FUNC(child2%Cmp)) ERROR STOP 23

      child1%Cmp => NULL()

      CALL SUB1(child1%Cmp)

      CALL SUB2(child2%Cmp)

      CONTAINS

      SUBROUTINE Sub1 (Arg)
      CLASS(Base(4,5)), POINTER ::  Arg

      ALLOCATE(Base(4,5):: Arg)
      IF ( .NOT. ASSOCIATED(Arg)) ERROR STOP 14

      IF( test1 .NE. FUNC(Arg)) ERROR STOP 24

      Arg => tgt1
      IF ( .NOT. ASSOCIATED(Arg)) ERROR STOP 15

      IF( test1 .NE. FUNC(Arg)) ERROR STOP 25

      END SUBROUTINE SUB1

      SUBROUTINE Sub2 (Arg)
      CLASS(Base(8,5)), POINTER ::  Arg

      ALLOCATE(Base(8,5):: Arg)
      IF ( .NOT. ASSOCIATED(Arg)) ERROR STOP 16

      IF( test2 .NE. FUNC(Arg)) ERROR STOP 26

      Arg => tgt2
      IF ( .NOT. ASSOCIATED(Arg)) ERROR STOP 17

      IF( test2 .NE. FUNC(Arg)) ERROR STOP 27

      END SUBROUTINE SUB2

      END PROGRAM Generic_Interface01a