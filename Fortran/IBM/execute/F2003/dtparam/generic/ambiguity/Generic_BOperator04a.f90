!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Generic_BOperator04a
!*                               DTP - Generic Operator (binary)
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : October 02, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Generic Resolution - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : Resolution by kind parameter 
!*                     
!*
!*  DRIVER STANZA              : xlf2003
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

      TYPE, EXTENDS(Base) :: Child1 (k1,l1)
        INTEGER, KIND :: k1 
        INTEGER, LEN :: l1 
      END TYPE Child1 

      TYPE, EXTENDS(Base) :: Child2 (k2,l2)
        INTEGER, KIND :: k2 
        INTEGER, LEN :: l2 
      END TYPE Child2 

      TYPE, EXTENDS(Child1) :: NextGen1 (k13,l13)
        INTEGER, KIND :: k13
        INTEGER, LEN :: l13
      END TYPE NextGen1

      TYPE, EXTENDS(Child2) :: NextGen2 (k23,l23)
        INTEGER, KIND :: k23
        INTEGER, LEN :: l23
      END TYPE NextGen2

      INTERFACE operator(+)
         module procedure add11      
         module procedure add12      
         module procedure add21      
         module procedure add22      
      END INTERFACE

      CONTAINS 
!*
      TYPE(Base(4,10)) FUNCTION add11(arg1,arg2) 
      CLASS(Child1(4,*,4,*)), INTENT(IN) :: arg1
      CLASS(Base(4,*)), INTENT(IN) :: arg2

      add11%value = 14

      END FUNCTION add11

      TYPE(Base(8,10)) FUNCTION add12(arg1,arg2) 
      CLASS(Child1(8,*,4,*)), INTENT(IN) :: arg1
      CLASS(Base(8,*)), INTENT(IN) :: arg2

      add12%value = 18

      END FUNCTION add12

      TYPE(Base(4,10)) FUNCTION add21(arg1,arg2) 
      CLASS(Child2(4,*,4,*)), INTENT(IN) :: arg1
      CLASS(Base(4,*)), INTENT(IN) :: arg2

      add21%value = 24

      END FUNCTION add21

      TYPE(Base(8,10)) FUNCTION add22(arg1,arg2) 
      CLASS(Child2(8,*,4,*)), INTENT(IN) :: arg1
      CLASS(Base(8,*)), INTENT(IN) :: arg2

      add22%value = 28

      END FUNCTION add22

      END MODULE Mod1
!*
      MODULE test1
      USE MOD1
      IMPLICIT TYPE(Base(4,10))(B)
      IMPLICIT TYPE(Child1(4,1,4,1))(C)
      IMPLICIT TYPE(Base(8,10))(D)
      IMPLICIT TYPE(Child1(8,1,4,1))(K)

      CONTAINS 
!*
      SUBROUTINE test_1 ()

      b_var = C1 + C2 
      IF ( B_var%value .NE. 14 ) STOP 10
      b_var = C1 + B1 
      IF ( B_var%value .NE. 14 ) STOP 11

      d_var = K1 + D1 
      IF ( D_var%value .NE. 18 ) STOP 12
      d_var = K1 + K2 
      IF ( D_var%value .NE. 18 ) STOP 13

      END SUBROUTINE test_1

      END MODULE test1
!*
      MODULE test2
      USE MOD1
      IMPLICIT TYPE(Base(4,10))(B)
      IMPLICIT TYPE(Child2(4,1,4,1))(C)
      IMPLICIT TYPE(Base(8,10))(D)
      IMPLICIT TYPE(Child2(8,1,4,1))(K)

      CONTAINS 
!*
      SUBROUTINE test_2 ()

      b_var = C1 + C2 
      IF ( B_var%value .NE. 24 ) STOP 14
      b_var = C1 + B1 
      IF ( B_var%value .NE. 24 ) STOP 15

      d_var = K1 + D1 
      IF ( D_var%value .NE. 28 ) STOP 16
      d_var = K1 + K2 
      IF ( D_var%value .NE. 28 ) STOP 17

      END SUBROUTINE test_2

      END MODULE test2
!*
      PROGRAM Generic_BOperator04a
      USE test1
      USE test2

      CALL test_1 ()
      CALL test_2 ()

      END PROGRAM Generic_BOperator04a
