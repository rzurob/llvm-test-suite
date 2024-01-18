!*  ===================================================================
!*
!*                               DTP - Generic Operator (unary)
!*
!*  DATE                       : October 02, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Generic Resolution - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : Resolution by kind parameter
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

        INTEGER :: value, type
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
         module procedure incr11
         module procedure incr12
         module procedure incr21
         module procedure incr22
      END INTERFACE

      CONTAINS
!*
      TYPE(Base(4,10)) FUNCTION incr11(arg1)
      CLASS(Child1(4,*,4,*)), INTENT(IN) :: arg1

      incr11%value = arg1%value + 1
      incr11%type  = arg1%k

      END FUNCTION incr11

      TYPE(Base(8,10)) FUNCTION incr12(arg1)
      CLASS(Child1(8,*,4,*)), INTENT(IN) :: arg1

      incr12%value = arg1%value + 1
      incr12%type  = arg1%k

      END FUNCTION incr12

      TYPE(Base(4,10)) FUNCTION incr21(arg1)
      CLASS(Child2(4,*,4,*)), INTENT(IN) :: arg1

      incr21%value = arg1%value + 2
      incr21%type  = arg1%k

      END FUNCTION incr21

      TYPE(Base(8,10)) FUNCTION incr22(arg1)
      CLASS(Child2(8,*,4,*)), INTENT(IN) :: arg1

      incr22%value = arg1%value + 2
      incr22%type  = arg1%k

      END FUNCTION incr22

      END MODULE Mod1
!*
      MODULE test1
      USE MOD1
      IMPLICIT TYPE(Base(4,10))(B)
      IMPLICIT TYPE(Base(8,10))(D)
      IMPLICIT TYPE(Child1(4,1,4,1))(C)
      IMPLICIT TYPE(Child1(8,1,4,1))(K)

      CONTAINS
!*
      SUBROUTINE test_1 ()

      C1 = Child1(4,1,4,1)(value=5, type=8)
      K1 = Child1(8,1,4,1)(value=50, type=4)

      B = +C1
      IF (B%type .NE. 4) STOP 10
      IF (B%value .NE. 6) STOP 11

      D = +K1
      IF (D%type .NE. 8) STOP 12
      IF (D%value .NE. 51) STOP 13

      END SUBROUTINE test_1

      END MODULE test1
!*
      MODULE test2
      USE MOD1
      IMPLICIT TYPE(Base(4,10))(B)
      IMPLICIT TYPE(Base(8,10))(D)
      IMPLICIT TYPE(Child2(4,1,4,1))(C)
      IMPLICIT TYPE(Child2(8,1,4,1))(K)

      CONTAINS
!*
      SUBROUTINE test_2 ()

      C2 = Child2(4,1,4,1)(value=10, type=0)
      K2 = Child2(8,1,4,1)(value=100, type=-10)

      B = +C2
      IF (B%type .NE. 4) STOP 10
      IF (B%value .NE. 12) STOP 11

      D = +K2
      IF (D%type .NE. 8) STOP 12
      IF (D%value .NE. 102) STOP 13

      END SUBROUTINE test_2

      END MODULE test2
!*
      PROGRAM Generic_UOperator03a
      USE test1
      USE test2

      CALL test_1 ()
      CALL test_2 ()

      END PROGRAM Generic_UOperator03a
