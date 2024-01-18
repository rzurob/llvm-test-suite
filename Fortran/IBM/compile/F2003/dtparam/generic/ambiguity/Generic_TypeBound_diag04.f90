!*  ===================================================================
!*
!*                               DTP - Generic Type-Bound
!*
!*  DATE                       : October 02, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : Generic Resolution - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : Resolution based on rank
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
         PROCEDURE :: foo1
         PROCEDURE :: foo2
         GENERIC :: FUNC =>  foo1 , foo2
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2)
        INTEGER, KIND :: k2
      END TYPE Child

      TYPE, EXTENDS(Child) :: NextGen(k3)
        INTEGER, KIND :: k3
      END TYPE NextGen

      CONTAINS
!*
      CLASS(Base(4,:)) FUNCTION foo1(Obj,Arg)
      CLASS(Base(4,*)) :: Obj
      CLASS(Base(4,*)), OPTIONAL :: Arg         ! Arg: rank is 0
      POINTER :: foo1

      ALLOCATE (foo1, source = Obj)
      IF ( .NOT. ASSOCIATED(foo1)) ERROR STOP 1

      END FUNCTION foo1

      CLASS(Base(4,:)) FUNCTION foo2(Obj,Arg)
      CLASS(Base(4,*)) :: Obj
      CLASS(Base(4,*)), OPTIONAL :: Arg(:)      ! Arg: rank is 1
      POINTER :: foo2

      ALLOCATE (foo2, source = Obj)
      IF ( .NOT. ASSOCIATED(foo2)) ERROR STOP 2

      END FUNCTION foo2

      END MODULE Mod1
!*
      PROGRAM Generic_TypeBound_diag04

      END PROGRAM Generic_TypeBound_diag04
