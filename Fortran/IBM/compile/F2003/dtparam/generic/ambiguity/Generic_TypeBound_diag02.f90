!*  ===================================================================
!*
!*                               DTP - Generic Type-Bound
!*
!*  DATE                       : October 02, 2008
!*  ORIGIN                     : AIX Compiler Development,
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
      END TYPE Child

      TYPE, EXTENDS(Child) :: NextGen(k3)
        INTEGER, KIND :: k3

        CONTAINS
         PROCEDURE, PASS :: foo2
         GENERIC :: FUNC =>  foo2
      END TYPE NextGen

      CONTAINS
!*
      CLASS(Base(4,:)) FUNCTION foo1(Obj)
      CLASS(Base(4,*)) :: Obj
      ALLOCATABLE :: foo1

      ALLOCATE (foo1, source = Obj)
      IF ( .NOT. ALLOCATED(foo1)) STOP 30

      END FUNCTION foo1

      CLASS(NextGen(4,:,4,4)) FUNCTION foo2(Obj,Arg)
      CLASS(NextGen(4,*,4,4)) :: Obj
      CLASS(Base(4,*)), OPTIONAL :: Arg
      ALLOCATABLE :: foo2

      ALLOCATE (foo2, source = Obj)
      IF ( .NOT. ALLOCATED(foo2)) STOP 31

      END FUNCTION foo2

      END MODULE Mod1
!*
      PROGRAM Generic_TypeBound_diag02

      END PROGRAM Generic_TypeBound_diag02
