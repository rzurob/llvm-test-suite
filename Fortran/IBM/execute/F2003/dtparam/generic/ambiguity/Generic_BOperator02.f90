!*  ===================================================================
!*
!*                               DTP - Generic Operator (binary)
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
         PROCEDURE, PASS :: mut11
         GENERIC :: operator(*) =>  mut11
      END TYPE Child1

      TYPE, EXTENDS(Base) :: Child2 (k2,l2)
        INTEGER, KIND :: k2
        INTEGER, LEN :: l2

        CONTAINS
         PROCEDURE, PASS :: mut21
         GENERIC :: operator(*) =>  mut21
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
      CLASS(Base(4,:)) FUNCTION mut11(arg1,arg2)
      CLASS(Child1(4,*,4,*)), INTENT(IN) :: arg1
      CLASS(Base(4,*)), INTENT(IN) :: arg2
      ALLOCATABLE :: mut11

      ALLOCATE(BASE(arg2%k,arg2%l) :: mut11)
      mut11%value = 14

      END FUNCTION mut11

      CLASS(Base(4,:)) FUNCTION mut21(arg1,arg2)
      CLASS(Child2(4,*,4,*)), INTENT(IN) :: arg1
      CLASS(Base(4,*)), INTENT(IN) :: arg2
      ALLOCATABLE :: mut21

      ALLOCATE(BASE(arg2%k,arg2%l) :: mut21)
      mut21%value = 24

      END FUNCTION mut21

      END MODULE Mod1
!*
      PROGRAM Generic_BOperator02
      USE MOD1
      IMPLICIT CLASS(Base(4,:))(B)
      IMPLICIT CLASS(Child1(4,1,4,1))(C)
      IMPLICIT CLASS(Child2(4,4,4,4))(K)
      IMPLICIT CLASS(NextGen1(4,1,4,1,8,1))(N)
      IMPLICIT CLASS(NextGen2(4,10,4,15,4,20))(M)

      ALLOCATABLE :: C1, B1, K1, N1, M1

      ALLOCATE (Base(4,100) :: B1)

!*  first argument Child1, call to mut11

      SELECT TYPE ( a => (C1 * B1) )
        CLASS IS (BASE(4,*))
          IF (a%value .NE. 14) STOP 10

        CLASS DEFAULT
          STOP 11
      END SELECT

      SELECT TYPE ( a => (C1 * C1) )
        CLASS IS (BASE(4,*))
          IF (a%value .NE. 14) STOP 12

        CLASS DEFAULT
          STOP 13
      END SELECT

      SELECT TYPE ( a => (C1 * K1) )
        CLASS IS (BASE(4,*))
          IF (a%value .NE. 14) STOP 14

        CLASS DEFAULT
          STOP 15
      END SELECT

      SELECT TYPE ( a => (C1 * N1) )
        CLASS IS (BASE(4,*))
          IF (a%value .NE. 14) STOP 16

        CLASS DEFAULT
          STOP 17
      END SELECT

      SELECT TYPE ( a => (C1 * M1) )
        CLASS IS (BASE(4,*))
          IF (a%value .NE. 14) STOP 18

        CLASS DEFAULT
          STOP 19
      END SELECT

!*  first argument NextGen1, call to mut11

      SELECT TYPE ( a => (N1 * B1) )
        CLASS IS (BASE(4,*))
          IF (a%value .NE. 14) STOP 20

        CLASS DEFAULT
          STOP 21
      END SELECT

      SELECT TYPE ( a => (N1 * C1) )
        CLASS IS (BASE(4,*))
          IF (a%value .NE. 14) STOP 22

        CLASS DEFAULT
          STOP 23
      END SELECT

      SELECT TYPE ( a => (N1 * K1) )
        CLASS IS (BASE(4,*))
          IF (a%value .NE. 14) STOP 24

        CLASS DEFAULT
          STOP 25
      END SELECT

      SELECT TYPE ( a => (N1 * N1) )
        CLASS IS (BASE(4,*))
          IF (a%value .NE. 14) STOP 26

        CLASS DEFAULT
          STOP 27
      END SELECT

      SELECT TYPE ( a => (N1 * M1) )
        CLASS IS (BASE(4,*))
          IF (a%value .NE. 14) STOP 28

        CLASS DEFAULT
          STOP 29
      END SELECT

!*  first argument NextGen2, call to mut21

      SELECT TYPE ( a => (M1 * B1) )
        CLASS IS (BASE(4,*))
          IF (a%value .NE. 24) STOP 30

        CLASS DEFAULT
          STOP 31
      END SELECT

      SELECT TYPE ( a => (M1 * C1) )
        CLASS IS (BASE(4,*))
          IF (a%value .NE. 24) STOP 32

        CLASS DEFAULT
          STOP 33
      END SELECT

      SELECT TYPE ( a => (M1 * K1) )
        CLASS IS (BASE(4,*))
          IF (a%value .NE. 24) STOP 34

        CLASS DEFAULT
          STOP 35
      END SELECT

      SELECT TYPE ( a => (M1 * N1) )
        CLASS IS (BASE(4,*))
          IF (a%value .NE. 24) STOP 36

        CLASS DEFAULT
          STOP 37
      END SELECT

      SELECT TYPE ( a => (M1 * M1) )
        CLASS IS (BASE(4,*))
          IF (a%value .NE. 24) STOP 38

        CLASS DEFAULT
          STOP 39
      END SELECT

!*  first argument Child2, call to mut21

      SELECT TYPE ( a => (K1 * B1) )
        CLASS IS (BASE(4,*))
          IF (a%value .NE. 24) STOP 40

        CLASS DEFAULT
          STOP 41
      END SELECT

      SELECT TYPE ( a => (K1 * C1) )
        CLASS IS (BASE(4,*))
          IF (a%value .NE. 24) STOP 42

        CLASS DEFAULT
          STOP 43
      END SELECT

      SELECT TYPE ( a => (K1 * K1) )
        CLASS IS (BASE(4,*))
          IF (a%value .NE. 24) STOP 44

        CLASS DEFAULT
          STOP 45
      END SELECT

      SELECT TYPE ( a => (K1 * N1) )
        CLASS IS (BASE(4,*))
          IF (a%value .NE. 24) STOP 46

        CLASS DEFAULT
          STOP 47
      END SELECT

      SELECT TYPE ( a => (K1 * M1) )
        CLASS IS (BASE(4,*))
          IF (a%value .NE. 24) STOP 48

        CLASS DEFAULT
          STOP 49
      END SELECT

      END PROGRAM Generic_BOperator02
