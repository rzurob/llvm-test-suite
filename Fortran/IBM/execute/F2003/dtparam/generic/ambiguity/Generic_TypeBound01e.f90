!*  ===================================================================
!*
!*                               DTP - Generic Type-Bound
!*
!*  DATE                       : October 02, 2008
!*  ORIGIN                     : AIX Compiler Development,
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
      CLASS(Base(4,*)) :: Obj, Arg         ! Arg: rank is 0
      POINTER :: foo1

      ALLOCATE (foo1, source = Obj)
      IF ( .NOT. ASSOCIATED(foo1)) STOP 1

      END FUNCTION foo1

      CLASS(Base(4,:)) FUNCTION foo2(Obj,Arg)
      CLASS(Base(4,*)) :: Obj, Arg(:)      ! Arg: rank is 1
      POINTER :: foo2

      ALLOCATE (foo2, source = Obj)
      IF ( .NOT. ASSOCIATED(foo2)) STOP 2

      END FUNCTION foo2

      END MODULE Mod1
!*
      PROGRAM Generic_TypeBound01e
      USE MOD1
      IMPLICIT NONE

      CLASS(Base(4,:)), POINTER :: poly1
      TYPE(Child(4,5,4)), TARGET :: tgt1
      TYPE(Base(4,5))  :: base1
      TYPE(child(4,10,4)) :: child1
      TYPE(NextGen(4,10,4,4)) :: nxtg

      TYPE(Base(4,5))  :: arr_base(10)
      TYPE(child(4,10,4)) :: arr_child(10)
      TYPE(NextGen(4,10,4,4)) :: arr_nxtg(10)

      TYPE(Base(4,5)), TARGET :: tgt_base(1)
      TYPE(Child(4,5,4)), TARGET :: tgt_chd(20)
      TYPE(NextGen(4,10,4,4)), TARGET :: tgt_nxtg(100)

      CLASS(Base(4,:)), POINTER :: ptr_base(:)
      CLASS(Child(4,:,4)), POINTER :: ptr_chd(:)
      CLASS(NextGen(4,:,4,4)), POINTER :: ptr_nxtg(:)

      CLASS(Base(4,:)), ALLOCATABLE :: allc_base(:)
      CLASS(Child(4,:,4)), ALLOCATABLE :: allc_chd(:)
      CLASS(NextGen(4,:,4,4)), ALLOCATABLE :: allc_nxtg(:)
!*
!  The following will call foo1
!*
      IF ( .NOT. ASSOCIATED(base1%FUNC(base1)) ) STOP 10
      IF ( .NOT. ASSOCIATED(base1%FUNC(tgt1)) ) STOP 11
      IF ( .NOT. ASSOCIATED(base1%FUNC(child1)) ) STOP 12
      IF ( .NOT. ASSOCIATED(base1%FUNC(nxtg)) ) STOP 13

      ALLOCATE(Base(4,10):: poly1)
      IF ( .NOT. ASSOCIATED(poly1%FUNC(base1)) ) STOP 14
      IF ( .NOT. ASSOCIATED(poly1%FUNC(tgt1)) ) STOP 15
      IF ( .NOT. ASSOCIATED(poly1%FUNC(poly1)) ) STOP 16
      IF ( .NOT. ASSOCIATED(poly1%FUNC(child1)) ) STOP 17
      IF ( .NOT. ASSOCIATED(poly1%FUNC(nxtg)) ) STOP 18

      poly1 => tgt1
      IF ( .NOT. ASSOCIATED(poly1%FUNC(base1)) ) STOP 19
      IF ( .NOT. ASSOCIATED(poly1%FUNC(tgt1)) ) STOP 20
      IF ( .NOT. ASSOCIATED(poly1%FUNC(poly1)) ) STOP 21
      IF ( .NOT. ASSOCIATED(poly1%FUNC(child1)) ) STOP 22
      IF ( .NOT. ASSOCIATED(poly1%FUNC(nxtg)) ) STOP 23

      ALLOCATE(NextGen(4,10,4,4):: poly1)
      IF ( .NOT. ASSOCIATED(poly1%FUNC(base1)) ) STOP 24
      IF ( .NOT. ASSOCIATED(poly1%FUNC(tgt1)) ) STOP 25
      IF ( .NOT. ASSOCIATED(poly1%FUNC(poly1)) ) STOP 26
      IF ( .NOT. ASSOCIATED(poly1%FUNC(child1)) ) STOP 27
      IF ( .NOT. ASSOCIATED(poly1%FUNC(nxtg)) ) STOP 28

      ALLOCATE(NextGen(4,10,8,8):: poly1)
      IF ( .NOT. ASSOCIATED(poly1%FUNC(base1)) ) STOP 29
      IF ( .NOT. ASSOCIATED(poly1%FUNC(tgt1)) ) STOP 30
      IF ( .NOT. ASSOCIATED(poly1%FUNC(poly1)) ) STOP 31
      IF ( .NOT. ASSOCIATED(poly1%FUNC(child1)) ) STOP 32
      IF ( .NOT. ASSOCIATED(poly1%FUNC(nxtg)) ) STOP 33

      IF ( .NOT. ASSOCIATED(child1%FUNC(base1)) ) STOP 34
      IF ( .NOT. ASSOCIATED(child1%FUNC(tgt1)) ) STOP 35
      IF ( .NOT. ASSOCIATED(child1%FUNC(poly1)) ) STOP 36
      IF ( .NOT. ASSOCIATED(child1%FUNC(nxtg)) ) STOP 37

      IF ( .NOT. ASSOCIATED(nxtg%FUNC(base1)) ) STOP 38
      IF ( .NOT. ASSOCIATED(nxtg%FUNC(tgt1)) ) STOP 39
      IF ( .NOT. ASSOCIATED(nxtg%FUNC(poly1)) ) STOP 40
      IF ( .NOT. ASSOCIATED(nxtg%FUNC(nxtg)) ) STOP 41
!*
!  The following will call foo2
!*
      IF ( .NOT. ASSOCIATED(base1%FUNC(arr_base)) ) STOP 42
      IF ( .NOT. ASSOCIATED(base1%FUNC(arr_child)) ) STOP 43
      IF ( .NOT. ASSOCIATED(base1%FUNC(arr_nxtg)) ) STOP 44

      IF ( .NOT. ASSOCIATED(child1%FUNC(arr_base)) ) STOP 45
      IF ( .NOT. ASSOCIATED(child1%FUNC(arr_child)) ) STOP 46
      IF ( .NOT. ASSOCIATED(child1%FUNC(arr_nxtg)) ) STOP 47

      IF ( .NOT. ASSOCIATED(nxtg%FUNC(arr_base)) ) STOP 48
      IF ( .NOT. ASSOCIATED(nxtg%FUNC(arr_child)) ) STOP 49
      IF ( .NOT. ASSOCIATED(nxtg%FUNC(arr_nxtg)) ) STOP 50

      IF ( .NOT. ASSOCIATED(poly1%FUNC(arr_base)) ) STOP 51
      IF ( .NOT. ASSOCIATED(poly1%FUNC(arr_child)) ) STOP 52
      IF ( .NOT. ASSOCIATED(poly1%FUNC(arr_nxtg)) ) STOP 53

      ALLOCATE(Base(4,10) :: ptr_base(10))
      IF ( .NOT. ASSOCIATED(base1%FUNC(ptr_base)) ) STOP 54
      IF ( .NOT. ASSOCIATED(child1%FUNC(ptr_base)) ) STOP 55
      IF ( .NOT. ASSOCIATED(nxtg%FUNC(ptr_base)) ) STOP 56
      IF ( .NOT. ASSOCIATED(poly1%FUNC(ptr_base)) ) STOP 57

      ALLOCATE(child(4,10,4) :: ptr_base(10), ptr_chd(5))
      IF ( .NOT. ASSOCIATED(base1%FUNC(ptr_base)) ) STOP 58
      IF ( .NOT. ASSOCIATED(child1%FUNC(ptr_base)) ) STOP 59
      IF ( .NOT. ASSOCIATED(nxtg%FUNC(ptr_base)) ) STOP 60
      IF ( .NOT. ASSOCIATED(poly1%FUNC(ptr_base)) ) STOP 61
      IF ( .NOT. ASSOCIATED(base1%FUNC(ptr_chd)) ) STOP 62
      IF ( .NOT. ASSOCIATED(child1%FUNC(ptr_chd)) ) STOP 63
      IF ( .NOT. ASSOCIATED(nxtg%FUNC(ptr_chd)) ) STOP 64
      IF ( .NOT. ASSOCIATED(poly1%FUNC(ptr_chd)) ) STOP 65

      ALLOCATE(NextGen(4,10,4,4) :: ptr_base(10), ptr_chd(5), ptr_nxtg(2) )
      IF ( .NOT. ASSOCIATED(base1%FUNC(ptr_base)) ) STOP 66
      IF ( .NOT. ASSOCIATED(child1%FUNC(ptr_base)) ) STOP 67
      IF ( .NOT. ASSOCIATED(nxtg%FUNC(ptr_base)) ) STOP 68
      IF ( .NOT. ASSOCIATED(poly1%FUNC(ptr_base)) ) STOP 69
      IF ( .NOT. ASSOCIATED(base1%FUNC(ptr_chd)) ) STOP 70
      IF ( .NOT. ASSOCIATED(child1%FUNC(ptr_chd)) ) STOP 71
      IF ( .NOT. ASSOCIATED(nxtg%FUNC(ptr_chd)) ) STOP 72
      IF ( .NOT. ASSOCIATED(poly1%FUNC(ptr_chd)) ) STOP 73
      IF ( .NOT. ASSOCIATED(base1%FUNC(ptr_nxtg)) ) STOP 74
      IF ( .NOT. ASSOCIATED(child1%FUNC(ptr_nxtg)) ) STOP 75
      IF ( .NOT. ASSOCIATED(nxtg%FUNC(ptr_nxtg)) ) STOP 76
      IF ( .NOT. ASSOCIATED(poly1%FUNC(ptr_nxtg)) ) STOP 77

      ptr_base => tgt_base; ptr_chd => tgt_chd; ptr_nxtg => tgt_nxtg
      IF ( .NOT. ASSOCIATED(base1%FUNC(ptr_base)) ) STOP 78
      IF ( .NOT. ASSOCIATED(child1%FUNC(ptr_base)) ) STOP 79
      IF ( .NOT. ASSOCIATED(nxtg%FUNC(ptr_base)) ) STOP 80
      IF ( .NOT. ASSOCIATED(poly1%FUNC(ptr_base)) ) STOP 81
      IF ( .NOT. ASSOCIATED(base1%FUNC(ptr_chd)) ) STOP 82
      IF ( .NOT. ASSOCIATED(child1%FUNC(ptr_chd)) ) STOP 83
      IF ( .NOT. ASSOCIATED(nxtg%FUNC(ptr_chd)) ) STOP 84
      IF ( .NOT. ASSOCIATED(poly1%FUNC(ptr_chd)) ) STOP 85
      IF ( .NOT. ASSOCIATED(base1%FUNC(ptr_nxtg)) ) STOP 86
      IF ( .NOT. ASSOCIATED(child1%FUNC(ptr_nxtg)) ) STOP 87
      IF ( .NOT. ASSOCIATED(nxtg%FUNC(ptr_nxtg)) ) STOP 88
      IF ( .NOT. ASSOCIATED(poly1%FUNC(ptr_nxtg)) ) STOP 89

      ALLOCATE(Base(4,10) :: allc_base(10))
      IF ( .NOT. ASSOCIATED(base1%FUNC(allc_base)) ) STOP 90
      IF ( .NOT. ASSOCIATED(child1%FUNC(allc_base)) ) STOP 91
      IF ( .NOT. ASSOCIATED(nxtg%FUNC(allc_base)) ) STOP 92
      IF ( .NOT. ASSOCIATED(poly1%FUNC(allc_base)) ) STOP 93

      ALLOCATE(child(4,10,4) :: allc_chd(5))
      IF ( .NOT. ASSOCIATED(base1%FUNC(allc_chd)) ) STOP 94
      IF ( .NOT. ASSOCIATED(child1%FUNC(allc_chd)) ) STOP 95
      IF ( .NOT. ASSOCIATED(nxtg%FUNC(allc_chd)) ) STOP 96
      IF ( .NOT. ASSOCIATED(poly1%FUNC(allc_chd)) ) STOP 97

      ALLOCATE(NextGen(4,10,4,4) :: allc_nxtg(2) )
      IF ( .NOT. ASSOCIATED(base1%FUNC(allc_nxtg)) ) STOP 98
      IF ( .NOT. ASSOCIATED(child1%FUNC(allc_nxtg)) ) STOP 99
      IF ( .NOT. ASSOCIATED(nxtg%FUNC(allc_nxtg)) ) STOP 100
      IF ( .NOT. ASSOCIATED(poly1%FUNC(allc_nxtg)) ) STOP 101

      END PROGRAM Generic_TypeBound01e
