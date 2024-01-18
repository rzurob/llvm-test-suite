!*  ===================================================================
!*
!*                               DTP - Generic Operator (unary)
!*
!*  DATE                       : October 02, 2008
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

      INTEGER, PARAMETER :: single = KIND(0.0), double = KIND(0d0), len = 10

      INTERFACE operator(+)
         module procedure incr1
         module procedure incr2
      END INTERFACE

      CONTAINS
!*
      TYPE(Base(single,len)) FUNCTION incr1(arg1)
      CLASS(Base(single,*)), INTENT(IN) :: arg1

      incr1%value = arg1%k

      END FUNCTION incr1

      TYPE(Base(double,len)) FUNCTION incr2(arg1)
      CLASS(Base(double,*)), INTENT(IN) :: arg1

      incr2%value = arg1%k

      END FUNCTION incr2

      END MODULE Mod1
!*
      PROGRAM Generic_UOperato03b
      USE MOD1, s => single, d => double
      IMPLICIT TYPE(Base(s,len))(B)
      IMPLICIT TYPE(Child(s,len,s,len))(C)
      IMPLICIT TYPE(Base(d,len))(D)
      IMPLICIT TYPE(Child(d,len,s,len))(K)

      b_var = +C1
      IF ( B_var%value .NE. 4 ) STOP 10
      b_var = +B1
      IF ( B_var%value .NE. 4 ) STOP 11

      d_var = +D1
      IF ( D_var%value .NE. 8 ) STOP 12
      d_var = +K1
      IF ( D_var%value .NE. 8 ) STOP 13

      END PROGRAM Generic_UOperato03b
