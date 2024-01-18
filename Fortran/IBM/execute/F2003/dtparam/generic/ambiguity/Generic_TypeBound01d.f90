!*  ===================================================================
!*
!*                               DTP - Generic Type-Bound
!*
!*  DATE                       : October 02, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : Generic Resolution - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : Resolution for polymorphic objects
!*                               based on the number of arguments
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

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1

        CONTAINS
         PROCEDURE, PASS :: foo1
         GENERIC :: FUNC =>  foo1
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2)
        INTEGER, KIND :: k2

        CONTAINS
         PROCEDURE, PASS :: foo2
         GENERIC :: FUNC =>  foo2
      END TYPE Child

      TYPE, EXTENDS(Child) :: NextGen(k3)
        INTEGER, KIND :: k3

        CONTAINS
         GENERIC :: REPT =>  foo1, foo2
      END TYPE NextGen

      CONTAINS
!*
      CLASS(Base(4,:)) FUNCTION foo1(Obj)
      CLASS(Base(4,*)) :: Obj
      ALLOCATABLE :: foo1

      ALLOCATE (foo1, source = Obj)
      IF ( .NOT. ALLOCATED(foo1)) STOP 100

      END FUNCTION foo1

      CLASS(Child(4,:,4)) FUNCTION foo2(Obj,Arg)
      CLASS(Child(4,*,4)) :: Obj
      CLASS(Base(4,*)) :: Arg
      ALLOCATABLE :: foo2

      ALLOCATE (foo2, source = Obj)
      IF ( .NOT. ALLOCATED(foo2)) STOP 101

      END FUNCTION foo2

      END MODULE Mod1
!*
      PROGRAM Generic_TypeBound01d
      USE MOD1
      IMPLICIT NONE

      CLASS(Base(4,:)), POINTER :: poly1
      TYPE(Child(4,5,4)), TARGET :: tgt1
      TYPE(Base(4,5))  :: base1
      TYPE(child(4,10,4)) :: child1
      TYPE(NextGen(4,10,4,4)) :: dtv

      IF (.NOT. ALLOCATED(base1%FUNC()) ) STOP 10

      ALLOCATE(Base(4,10):: poly1)          ! dynamic type BASE call foo1
      IF ( .NOT. ALLOCATED(poly1%FUNC()) ) STOP 11

      poly1 => tgt1                         ! dynamic type Child call foo1
      IF ( .NOT. ALLOCATED(poly1%FUNC()) ) STOP 12

      ALLOCATE(NextGen(4,10,4,4):: poly1)   ! dynamic type NextGen call foo1
      IF ( .NOT. ALLOCATED(poly1%FUNC()) ) STOP 13

      SELECT TYPE ( poly1) ! call possible only within select type
          CLASS IS (NextGen(4,*,4,4))
           IF ( .NOT. ALLOCATED(poly1%FUNC(base1)) ) STOP 14
           IF ( .NOT. ALLOCATED(poly1%FUNC(tgt1)) ) STOP 15
           IF ( .NOT. ALLOCATED(poly1%FUNC(poly1)) ) STOP 16

          CLASS DEFAULT
           STOP 102
      END SELECT

      ALLOCATE(NextGen(4,10,8,8):: poly1)   ! dynamic type NextGen with k2=k3=8, call foo1
      IF ( .NOT. ALLOCATED(poly1%FUNC())) STOP 17

      IF ( .NOT. ALLOCATED(child1%FUNC(base1)) ) STOP 18
      IF ( .NOT. ALLOCATED(child1%FUNC(tgt1)) ) STOP 19
      IF ( .NOT. ALLOCATED(child1%FUNC(poly1)) ) STOP 20
      IF ( .NOT. ALLOCATED(child1%FUNC(dtv)) ) STOP 21

      IF ( .NOT. ALLOCATED(dtv%FUNC(base1)) ) STOP 22
      IF ( .NOT. ALLOCATED(dtv%FUNC(tgt1)) ) STOP 23
      IF ( .NOT. ALLOCATED(dtv%FUNC(poly1)) ) STOP 24
      IF ( .NOT. ALLOCATED(dtv%FUNC(dtv)) ) STOP 25

      ALLOCATE(NextGen(4,10,4,4):: poly1)   ! dynamic type NextGen call foo1

      SELECT TYPE ( poly1) ! call possible only within select type
          CLASS IS (NextGen(4,*,4,4))
             IF ( .NOT. ALLOCATED(poly1%REPT()) ) STOP 26

          CLASS DEFAULT
           STOP 103
      END SELECT

      ALLOCATE(NextGen(4,10,8,8):: poly1)   ! dynamic type NextGen with k2=k3=8, call foo1

      SELECT TYPE ( poly1) ! call possible only within select type
          CLASS IS (NextGen(4,*,8,8))
             IF ( .NOT. ALLOCATED(poly1%REPT())) STOP 27

          CLASS DEFAULT
           STOP 104
      END SELECT


      IF ( .NOT. ALLOCATED(dtv%REPT(base1)) ) STOP 28
      IF ( .NOT. ALLOCATED(dtv%REPT(tgt1)) ) STOP 29
      IF ( .NOT. ALLOCATED(dtv%REPT(poly1)) ) STOP 30
      IF ( .NOT. ALLOCATED(dtv%REPT(dtv)) ) STOP 31

      END PROGRAM Generic_TypeBound01d
