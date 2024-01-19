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
!*                                    & [ [, binding-attr -list ] :: ] &
!*                                    & binding-name [ => procedure-name ]
!*
!*  R452 generic-binding is GENERIC [, access-spec ] :: generic-spec => binding-name-list
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

        CONTAINS
         PROCEDURE, PASS :: incr11
         GENERIC :: operator(+) =>  incr11
      END TYPE Child1

      TYPE, EXTENDS(Base) :: Child2 (k2,l2)
        INTEGER, KIND :: k2
        INTEGER, LEN :: l2

        CONTAINS
         PROCEDURE, PASS :: incr21
         GENERIC :: operator(+) =>  incr21
      END TYPE Child2

      TYPE, EXTENDS(Child1) :: NextGen1 (k13,l13)
        INTEGER, KIND :: k13
        INTEGER, LEN :: l13
      END TYPE NextGen1

      TYPE, EXTENDS(Child2) :: NextGen2 (k23,l23)
        INTEGER, KIND :: k23
        INTEGER, LEN :: l23
      END TYPE NextGen2

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
      PROGRAM Generic_UOperator02
      USE MOD1, ONLY : Child1, Child2, NextGen1, NextGen2, Base
      IMPLICIT CLASS(Child1(4,1,4,1))(C)
      IMPLICIT CLASS(Child2(4,4,4,4))(K)
      IMPLICIT CLASS(NextGen1(4,1,4,1,8,1))(N)
      IMPLICIT CLASS(NextGen2(4,10,4,15,4,20))(M)

      ALLOCATABLE :: C1, K1, N1, M1

      SELECT TYPE ( a => +C1 )
        CLASS IS (BASE(4,*))
          IF (a%value .NE. 14) ERROR STOP 12

        CLASS DEFAULT
          STOP 13
      END SELECT

      SELECT TYPE ( a => +K1 )
        CLASS IS (BASE(4,*))
          IF (a%value .NE. 24) ERROR STOP 14

        CLASS DEFAULT
          STOP 15
      END SELECT

      SELECT TYPE ( a => +N1 )
        CLASS IS (BASE(4,*))
          IF (a%value .NE. 14) ERROR STOP 16

        CLASS DEFAULT
          STOP 17
      END SELECT

      SELECT TYPE ( a => +M1 )
        CLASS IS (BASE(4,*))
          IF (a%value .NE. 24) ERROR STOP 18

        CLASS DEFAULT
          STOP 19
      END SELECT

      END PROGRAM Generic_UOperator02
