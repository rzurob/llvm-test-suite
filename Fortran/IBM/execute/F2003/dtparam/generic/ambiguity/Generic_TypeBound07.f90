!*  ===================================================================
!*
!*                               DTP - Generic Type-Bound
!*
!*  DATE                       : October 02, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : Generic Resolution - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : apparent ambiguity between elemental and nonelemental procedure
!*                               The reference is to the nonelemental procedure
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
         GENERIC :: FUNC =>  foo1, foo2
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2)
        INTEGER, KIND :: k2
      END TYPE Child

      TYPE, EXTENDS(Child) :: NextGen(k3)
        INTEGER, KIND :: k3
      END TYPE NextGen

      CONTAINS
!*
      ELEMENTAL FUNCTION foo1(Obj,Arg)
      CLASS(Base(4,*)), INTENT(IN) :: Obj, Arg         ! Arg: rank is 0
      CHARACTER(20)  :: foo1

      foo1 = "ele"

      END FUNCTION foo1

      FUNCTION foo2(Obj,Arg)
      CLASS(Base(4,*)), INTENT(IN) :: Obj, Arg(*)      ! Arg: rank is 1
      !TYPE(Base(4,20)) :: foo2
      CHARACTER(20)  :: foo2

      foo2 = "non-ele"

      END FUNCTION foo2

      END MODULE Mod1
!*
      PROGRAM Generic_TypeBound07
      USE MOD1
      IMPLICIT NONE

      CLASS(Base(4,:)), POINTER :: poly1
      TYPE(Child(4,5,4)), TARGET :: tgt1
      TYPE(Base(4,5))  :: base1
      TYPE(NextGen(4,10,4,4)) :: nxtg

      TYPE(child(4,10,4)) :: arr_child(10)
      TYPE(NextGen(4,10,4,4)) :: arr_nxtg(10)

      TYPE(Base(4,5)), TARGET :: tgt_base(1)
      TYPE(Child(4,5,4)), TARGET :: tgt_chd(2,2)
      TYPE(NextGen(4,10,4,4)), TARGET :: tgt_nxtg(100)

      CLASS(Base(4,:)), POINTER :: ptr_base(:)
      CLASS(Child(4,:,4)), POINTER :: ptr_chd(:,:)
      CLASS(NextGen(4,:,4,4)), POINTER :: ptr_nxtg(:)

!*
!  The following will call foo1
!*
      IF( base1%FUNC(base1) .NE. "ele" ) STOP 10
      IF( base1%FUNC(tgt1) .NE. "ele" ) STOP 11
      IF( base1%FUNC(nxtg) .NE. "ele" ) STOP 12

      ALLOCATE(Base(4,10):: poly1)
      IF( poly1%FUNC(base1) .NE. "ele" ) STOP 13
      IF( poly1%FUNC(tgt1) .NE. "ele" ) STOP 14
      IF( poly1%FUNC(nxtg) .NE. "ele" ) STOP 15
      IF( poly1%FUNC(poly1) .NE. "ele" ) STOP 16

      poly1 => tgt1
      IF( poly1%FUNC(base1) .NE. "ele" ) STOP 17
      IF( poly1%FUNC(tgt1) .NE. "ele" ) STOP 18
      IF( poly1%FUNC(nxtg) .NE. "ele" ) STOP 19
      IF( poly1%FUNC(poly1) .NE. "ele" ) STOP 20

!*
!  The following will call foo2
!*
      IF( nxtg%FUNC(arr_child) .NE. "non-ele" ) STOP 21
      IF( poly1%FUNC(arr_child) .NE. "non-ele" ) STOP 22
      IF( poly1%FUNC(arr_nxtg) .NE. "non-ele" ) STOP 23

      ALLOCATE(child(4,10,4) :: ptr_base(10), ptr_chd(5,5))
      IF( base1%FUNC(ptr_base) .NE. "non-ele" ) STOP 24
      IF( ANY(poly1%FUNC(ptr_chd) .NE. "ele" )) STOP 25
      IF( ANY(nxtg%FUNC(ptr_chd) .NE. "ele" )) STOP 26

      ptr_base => tgt_base; ptr_chd => tgt_chd; ptr_nxtg => tgt_nxtg
      IF( base1%FUNC(ptr_base) .NE. "non-ele" ) STOP 24
      IF( nxtg%FUNC(ptr_base) .NE. "non-ele" ) STOP 24
      IF( poly1%FUNC(ptr_base) .NE. "non-ele" ) STOP 24
      IF( ANY(base1%FUNC(ptr_chd) .NE. "ele" )) STOP 25
      IF( ANY(nxtg%FUNC(ptr_chd) .NE. "ele" )) STOP 26
      IF( ANY(poly1%FUNC(ptr_chd) .NE. "ele" )) STOP 25

     END PROGRAM  Generic_TypeBound07
