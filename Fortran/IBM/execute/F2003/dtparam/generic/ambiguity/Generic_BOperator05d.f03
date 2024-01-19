!*  ===================================================================
!*
!*                               DTP - Generic Operator (binary)
!*
!*  DATE                       : October 03, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : Generic Resolution - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : Resolution based on rank - assumed size array
!*                               Call is possible with arrays of any shape to first function only with specific
!*                               No ambiguity because resolution is based on rank
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

      TYPE Base (k,l)
        INTEGER, KIND :: k
        INTEGER, LEN :: l

        INTEGER :: value
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1
      END TYPE Child

      TYPE, EXTENDS(Child) :: NextGen (k13,l13)
        INTEGER, KIND :: k13
        INTEGER, LEN :: l13
      END TYPE NextGen

      INTERFACE operator(*)
         module procedure mut1
         module procedure mut2
      END INTERFACE

      CONTAINS
!*
      TYPE(Base(4,:)) FUNCTION mut1(arg1,arg2)
      CLASS(Base(4,*)), INTENT(IN) :: arg1
      CLASS(Base(4,*)), DIMENSION(*), INTENT(IN) :: arg2   ! rank 1
      POINTER :: mut1

      ALLOCATE(BASE(arg2%k,arg2%l) :: mut1)
      mut1%value = 14

      END FUNCTION mut1

      TYPE(Base(4,:)) FUNCTION mut2(arg1,arg2)
      CLASS(Base(4,*)), INTENT(IN) :: arg1
      CLASS(Base(4,*)), INTENT(IN) :: arg2(:,:)            ! rank 2
      POINTER :: mut2

      ALLOCATE(BASE(arg2%k,arg2%l) :: mut2)
      mut2%value = 24

      END FUNCTION mut2

      END MODULE Mod1
!*
      PROGRAM Generic_BOperator03c
      USE MOD1
      IMPLICIT CLASS(Base(4,:))(B)
      IMPLICIT CLASS(Child(4,:,4,:))(C)
      IMPLICIT CLASS(NextGen(4,:,4,:,8,:))(N)

      POINTER :: b_var  , b2_var
      POINTER :: C0, N0
      POINTER :: B1(:), C1(:), B2(:,:), N2(:,:)

      ALLOCATE ( Base(4,100) :: B1(100) )
      ALLOCATE ( Child(4,10,4,10) :: C0, C1(10), B2(10,10) )
      ALLOCATE ( NextGen(4,1,4,1,8,1) :: N0, N2(1,1) )

      B_var => C0 * B1
      IF ( B_var%value .NE. 14 ) ERROR STOP 3
      B_var => C0 * C1
      IF ( B_var%value .NE. 14 ) ERROR STOP 4

      B_var => C0 * B2              ! generic call to mut2
      B2_var => mut1(C0,B2)         ! call to mut1 is possible only when calling with the specific
      IF ( B_var%value .NE. 24 ) ERROR STOP 5
      B_var => C0 * N2
      IF ( B_var%value .NE. 24 ) ERROR STOP 6

      B_var => N0 * B1
      IF ( B_var%value .NE. 14 ) ERROR STOP 9
      B_var => N0 * C1
      IF ( B_var%value .NE. 14 ) ERROR STOP 10

      B_var => N0 * B2
      IF ( B_var%value .NE. 24 ) ERROR STOP 11
      B_var => N0 * N2
      IF ( B_var%value .NE. 24 ) ERROR STOP 12

      END PROGRAM Generic_BOperator03c
