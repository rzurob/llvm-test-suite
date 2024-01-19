!*  ===================================================================
!*
!*                               DTP - Generic Operator (unary)
!*
!*  DATE                       : October 02, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : Generic Resolution - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : Resolution by type incompatibility
!*                               Function result is polymorphic
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

      INTERFACE operator(.incr.)
         module procedure incr11
         module procedure incr21
      END INTERFACE

      CONTAINS
!*
      CLASS(Base(4,:)) FUNCTION incr11(arg1)
      CLASS(Child1(4,*,4,*)), INTENT(IN) :: arg1
      ALLOCATABLE :: incr11

      ALLOCATE(BASE(arg1%k,arg1%l) :: incr11)
      incr11%value = 14

      END FUNCTION incr11

      CLASS(Base(4,:)) FUNCTION incr21(arg1)
      CLASS(Child2(4,*,4,*)), INTENT(IN) :: arg1
      ALLOCATABLE :: incr21

      ALLOCATE(BASE(arg1%k,arg1%l) :: incr21)
      incr21%value = 24

      END FUNCTION incr21

      END MODULE Mod1
!*
      PROGRAM Generic_UOperator04
      USE MOD1
      IMPLICIT CLASS(Child1(4,1,4,1))(C)
      IMPLICIT CLASS(Child2(4,4,4,4))(K)
      IMPLICIT CLASS(NextGen1(4,1,4,1,8,1))(N)
      IMPLICIT CLASS(NextGen2(4,10,4,15,4,20))(M)

      ALLOCATABLE :: C1, K1, N1, M1

      SELECT TYPE ( a => .incr. C1 )
        CLASS IS (BASE(4,*))
          IF (a%value .NE. 14) ERROR STOP 12

        CLASS DEFAULT
          STOP 13
      END SELECT

      SELECT TYPE ( a => .incr. K1 )
        CLASS IS (BASE(4,*))
          IF (a%value .NE. 24) ERROR STOP 14

        CLASS DEFAULT
          STOP 15
      END SELECT

      SELECT TYPE ( a => .incr. N1 )
        CLASS IS (BASE(4,*))
          IF (a%value .NE. 14) ERROR STOP 16

        CLASS DEFAULT
          STOP 17
      END SELECT

      SELECT TYPE ( a => .incr. M1 )
        CLASS IS (BASE(4,*))
          IF (a%value .NE. 24) ERROR STOP 18

        CLASS DEFAULT
          STOP 19
      END SELECT

      END PROGRAM Generic_UOperator04
