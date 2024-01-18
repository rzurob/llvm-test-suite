!*  ===================================================================
!*
!*                               DTP - Generic Type-Bound
!*
!*  DATE                       : October 03, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Generic Resolution - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : Resolution for polymorphic objects
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
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2)
        INTEGER, KIND :: k2

        CONTAINS
         PROCEDURE, PASS :: foo2
      END TYPE Child

      TYPE, EXTENDS(Child) :: NextGen(k3)
        INTEGER, KIND :: k3

        CONTAINS
         GENERIC :: FUNC =>  foo1, foo2
      END TYPE NextGen

      CONTAINS
!*
      CLASS(Base(4,:)) FUNCTION foo1(Obj)
      CLASS(Base(4,*)) :: Obj
      ALLOCATABLE :: foo1

      ALLOCATE (foo1, source = Obj)
      IF ( .NOT. ALLOCATED(foo1)) STOP 30

      END FUNCTION foo1

      CLASS(Child(4,:,4)) FUNCTION foo2(Obj,Arg)
      CLASS(child(4,*,4)) :: Obj
      CLASS(Base(4,*)) :: Arg
      ALLOCATABLE :: foo2

      ALLOCATE (foo2, source = Obj)
      IF ( .NOT. ALLOCATED(foo2)) STOP 31

      END FUNCTION foo2

      END MODULE Mod1
!*
      PROGRAM Generic_TypeBound_diag03
      USE MOD1
      IMPLICIT NONE

      CLASS(Base(4,:)), POINTER :: poly1
      TYPE(Child(4,5,4)), TARGET :: tgt1
      TYPE(Base(4,5))  :: base1
      TYPE(child(4,10,4)) :: child1
      TYPE(NextGen(4,10,4,4)) :: dtv

! Valid code: should pass compilation

      ALLOCATE(NextGen(4,10,4,4):: poly1)   ! dynamic type NextGen call foo1

      SELECT TYPE ( poly1) ! call possible only within select type
          CLASS IS (NextGen(4,*,4,4))
             IF ( .NOT. ALLOCATED(poly1%FUNC()) ) STOP 13

          CLASS DEFAULT
           STOP 32
      END SELECT

      IF ( .NOT. ALLOCATED(dtv%FUNC()) ) STOP 13

      IF ( .NOT. ALLOCATED(dtv%FUNC(base1)) ) STOP 22  !Ok : call to foo2 with dtv of type NextGen
      IF ( .NOT. ALLOCATED(dtv%FUNC(tgt1)) ) STOP 23   !Ok
      IF ( .NOT. ALLOCATED(dtv%FUNC(poly1)) ) STOP 24  !Ok
      IF ( .NOT. ALLOCATED(dtv%FUNC(dtv)) ) STOP 25    !Ok

! Invalid code: should not pass compilation

      IF (.NOT. ALLOCATED(base1%FUNC()) ) STOP 10

      ALLOCATE(Base(4,10):: poly1)          ! dynamic type BASE
      IF ( .NOT. ALLOCATED(poly1%FUNC()) ) STOP 11

      poly1 => tgt1                         ! dynamic type Child
      IF ( .NOT. ALLOCATED(poly1%FUNC()) ) STOP 12

      IF ( .NOT. ALLOCATED(child1%FUNC(base1)) ) STOP 18
      IF ( .NOT. ALLOCATED(child1%FUNC(tgt1)) ) STOP 19
      IF ( .NOT. ALLOCATED(child1%FUNC(poly1)) ) STOP 20
      IF ( .NOT. ALLOCATED(child1%FUNC(dtv)) ) STOP 21

      END PROGRAM Generic_TypeBound_diag03
