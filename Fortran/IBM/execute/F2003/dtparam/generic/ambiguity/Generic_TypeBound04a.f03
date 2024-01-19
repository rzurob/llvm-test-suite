!*  ===================================================================
!*
!*                               DTP - Generic Type-Bound
!*
!*  DATE                       : October 02, 2008
!*
!*  PRIMARY SUBROUTINES TESTED   : Generic Resolution - Derived-type parameters
!*  SECONDARY SUBROUTINES TESTED : Resolution for non-polymorphic objects
!*                                 based on type incompatibility and number of arguments
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

        CONTAINS
         PROCEDURE :: sub1
         PROCEDURE, PASS(Arg1) :: sub2
         PROCEDURE, PASS(Arg2) :: sub3
         GENERIC :: SUB =>  sub1, sub2, sub3
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

      CHARACTER(10) :: tag

      CONTAINS
!*
      SUBROUTINE sub1(Obj)
      CLASS(Base(4,*)) :: Obj
      CLASS(Base(4,:)), POINTER  :: pntr

      ALLOCATE (pntr, source = Obj)
      IF ( .NOT. ASSOCIATED(pntr)) ERROR STOP 1

      tag = '1'

      END SUBROUTINE sub1

      SUBROUTINE sub2(Obj, Arg1, Arg2)
      CLASS(Base(4,*)) ::  Arg1, Arg2
      CLASS(Child1(4,*,4,*)) :: Obj
      CLASS(Base(4,:)), POINTER :: pntr

      ALLOCATE (pntr, source = Arg1)
      IF ( .NOT. ASSOCIATED(pntr)) ERROR STOP 2

      tag = '2'

      END SUBROUTINE sub2

      SUBROUTINE sub3(Obj, Arg1, Arg2)
      CLASS(Base(4,*)) ::  Arg1, Arg2
      CLASS(Child2(4,*,4,*)) :: Obj
      CLASS(Base(4,:)), POINTER :: pntr

      ALLOCATE (pntr, source = Arg2)
      IF ( .NOT. ASSOCIATED(pntr)) ERROR STOP 3

      tag = '3'

      END SUBROUTINE sub3

      END MODULE Mod1
!*
      PROGRAM Generic_TypeBound04a
      USE MOD1
      IMPLICIT NONE

      TYPE(Base(4,5)) :: b1
      TYPE(Child1(4,5,4,10)) :: c1
      TYPE(Child2(4,5,4,10)) :: c2
      TYPE(NextGen1(4,10,4,2,4,2)) :: n1
      TYPE(NextGen2(4,10,4,2,4,2)) :: n2

!*  Only one passed object dummy argument : call to sub1
      CALL b1%SUB()
      IF ( tag .NE. '1' ) ERROR STOP 10
      CALL c1%SUB()
      IF ( tag .NE. '1' ) ERROR STOP 11
      CALL c2%SUB()
      IF ( tag .NE. '1' ) ERROR STOP 12
      CALL n1%SUB()
      IF ( tag .NE. '1' ) ERROR STOP 13
      CALL n2%SUB()
      IF ( tag .NE. '1' ) ERROR STOP 14

!*  The first non-passed argument c1 is of type Child1 : call to sub2
      CALL b1%SUB(c1,b1)
      IF ( tag .NE. '2' ) ERROR STOP 15
      CALL b1%SUB(c1,c1)
      IF ( tag .NE. '2' ) ERROR STOP 16
      CALL b1%SUB(c1,c2)
      IF ( tag .NE. '2' ) ERROR STOP 17
      CALL b1%SUB(c1,n1)
      IF ( tag .NE. '2' ) ERROR STOP 18
      CALL b1%SUB(c1,n2)
      IF ( tag .NE. '2' ) ERROR STOP 19
      CALL c1%SUB(c1,b1)
      IF ( tag .NE. '2' ) ERROR STOP 20
      CALL c1%SUB(c1,c1)
      IF ( tag .NE. '2' ) ERROR STOP 21
      CALL c1%SUB(c1,c2)
      IF ( tag .NE. '2' ) ERROR STOP 22
      CALL c1%SUB(c1,n1)
      IF ( tag .NE. '2' ) ERROR STOP 23
      CALL c1%SUB(c1,n2)
      IF ( tag .NE. '2' ) ERROR STOP 24
      CALL c2%SUB(c1,b1)
      IF ( tag .NE. '2' ) ERROR STOP 25
      CALL c2%SUB(c1,c1)
      IF ( tag .NE. '2' ) ERROR STOP 26
      CALL c2%SUB(c1,c2)
      IF ( tag .NE. '2' ) ERROR STOP 27
      CALL c2%SUB(c1,n1)
      IF ( tag .NE. '2' ) ERROR STOP 28
      CALL c2%SUB(c1,n2)
      IF ( tag .NE. '2' ) ERROR STOP 29
      CALL n1%SUB(c1,b1)
      IF ( tag .NE. '2' ) ERROR STOP 30
      CALL n1%SUB(c1,c1)
      IF ( tag .NE. '2' ) ERROR STOP 31
      CALL n1%SUB(c1,c2)
      IF ( tag .NE. '2' ) ERROR STOP 32
      CALL n1%SUB(c1,n1)
      IF ( tag .NE. '2' ) ERROR STOP 33
      CALL n1%SUB(c1,n2)
      IF ( tag .NE. '2' ) ERROR STOP 34
      CALL n2%SUB(c1,b1)
      IF ( tag .NE. '2' ) ERROR STOP 35
      CALL n2%SUB(c1,c1)
      IF ( tag .NE. '2' ) ERROR STOP 36
      CALL n2%SUB(c1,c2)
      IF ( tag .NE. '2' ) ERROR STOP 37
      CALL n2%SUB(c1,n1)
      IF ( tag .NE. '2' ) ERROR STOP 38
      CALL n2%SUB(c1,n2)
      IF ( tag .NE. '2' ) ERROR STOP 39

!*  The first non-passed argument n1 is of type NextGen1 : call to sub2
      CALL b1%SUB(n1,b1)
      IF ( tag .NE. '2' ) ERROR STOP 40
      CALL b1%SUB(n1,c1)
      IF ( tag .NE. '2' ) ERROR STOP 41
      CALL b1%SUB(n1,c2)
      IF ( tag .NE. '2' ) ERROR STOP 42
      CALL b1%SUB(n1,n1)
      IF ( tag .NE. '2' ) ERROR STOP 43
      CALL b1%SUB(n1,n2)
      IF ( tag .NE. '2' ) ERROR STOP 44
      CALL c1%SUB(n1,b1)
      IF ( tag .NE. '2' ) ERROR STOP 45
      CALL c1%SUB(n1,c1)
      IF ( tag .NE. '2' ) ERROR STOP 46
      CALL c1%SUB(n1,c2)
      IF ( tag .NE. '2' ) ERROR STOP 47
      CALL c1%SUB(n1,n1)
      IF ( tag .NE. '2' ) ERROR STOP 48
      CALL c1%SUB(n1,n2)
      IF ( tag .NE. '2' ) ERROR STOP 49
      CALL c2%SUB(n1,b1)
      IF ( tag .NE. '2' ) ERROR STOP 50
      CALL c2%SUB(n1,c1)
      IF ( tag .NE. '2' ) ERROR STOP 51
      CALL c2%SUB(n1,c2)
      IF ( tag .NE. '2' ) ERROR STOP 52
      CALL c2%SUB(n1,n1)
      IF ( tag .NE. '2' ) ERROR STOP 53
      CALL c2%SUB(n1,n2)
      IF ( tag .NE. '2' ) ERROR STOP 54
      CALL n1%SUB(n1,b1)
      IF ( tag .NE. '2' ) ERROR STOP 55
      CALL n1%SUB(n1,c1)
      IF ( tag .NE. '2' ) ERROR STOP 56
      CALL n1%SUB(n1,c2)
      IF ( tag .NE. '2' ) ERROR STOP 57
      CALL n1%SUB(n1,n1)
      IF ( tag .NE. '2' ) ERROR STOP 58
      CALL n1%SUB(n1,n2)
      IF ( tag .NE. '2' ) ERROR STOP 59
      CALL n2%SUB(n1,b1)
      IF ( tag .NE. '2' ) ERROR STOP 60
      CALL n2%SUB(n1,c1)
      IF ( tag .NE. '2' ) ERROR STOP 61
      CALL n2%SUB(n1,c2)
      IF ( tag .NE. '2' ) ERROR STOP 62
      CALL n2%SUB(n1,n1)
      IF ( tag .NE. '2' ) ERROR STOP 63
      CALL n2%SUB(n1,n2)
      IF ( tag .NE. '2' ) ERROR STOP 64

!*  The first non-passed argument c2 is of type Child2 : call to sub3
      CALL b1%SUB(c2,b1)
      IF ( tag .NE. '3' ) ERROR STOP 65
      CALL b1%SUB(c2,c1)
      IF ( tag .NE. '3' ) ERROR STOP 66
      CALL b1%SUB(c2,c2)
      IF ( tag .NE. '3' ) ERROR STOP 67
      CALL b1%SUB(c2,n1)
      IF ( tag .NE. '3' ) ERROR STOP 68
      CALL b1%SUB(c2,n2)
      IF ( tag .NE. '3' ) ERROR STOP 69
      CALL c1%SUB(c2,b1)
      IF ( tag .NE. '3' ) ERROR STOP 70
      CALL c1%SUB(c2,c1)
      IF ( tag .NE. '3' ) ERROR STOP 71
      CALL c1%SUB(c2,c2)
      IF ( tag .NE. '3' ) ERROR STOP 72
      CALL c1%SUB(c2,n1)
      IF ( tag .NE. '3' ) ERROR STOP 73
      CALL c1%SUB(c2,n2)
      IF ( tag .NE. '3' ) ERROR STOP 74
      CALL c2%SUB(c2,b1)
      IF ( tag .NE. '3' ) ERROR STOP 75
      CALL c2%SUB(c2,c1)
      IF ( tag .NE. '3' ) ERROR STOP 76
      CALL c2%SUB(c2,c2)
      IF ( tag .NE. '3' ) ERROR STOP 77
      CALL c2%SUB(c2,n1)
      IF ( tag .NE. '3' ) ERROR STOP 78
      CALL c2%SUB(c2,n2)
      IF ( tag .NE. '3' ) ERROR STOP 79
      CALL n1%SUB(c2,b1)
      IF ( tag .NE. '3' ) ERROR STOP 80
      CALL n1%SUB(c2,c1)
      IF ( tag .NE. '3' ) ERROR STOP 81
      CALL n1%SUB(c2,c2)
      IF ( tag .NE. '3' ) ERROR STOP 82
      CALL n1%SUB(c2,n1)
      IF ( tag .NE. '3' ) ERROR STOP 83
      CALL n1%SUB(c2,n2)
      IF ( tag .NE. '3' ) ERROR STOP 84
      CALL n2%SUB(c2,b1)
      IF ( tag .NE. '3' ) ERROR STOP 85
      CALL n2%SUB(c2,c1)
      IF ( tag .NE. '3' ) ERROR STOP 86
      CALL n2%SUB(c2,c2)
      IF ( tag .NE. '3' ) ERROR STOP 87
      CALL n2%SUB(c2,n1)
      IF ( tag .NE. '3' ) ERROR STOP 88
      CALL n2%SUB(c2,n2)
      IF ( tag .NE. '3' ) ERROR STOP 89

!*  The first non-passed argument n2 is of type NextGen2 : call to sub3
      CALL b1%SUB(n2,b1)
      IF ( tag .NE. '3' ) ERROR STOP 90
      CALL b1%SUB(c2,c1)
      IF ( tag .NE. '3' ) ERROR STOP 91
      CALL b1%SUB(n2,c2)
      IF ( tag .NE. '3' ) ERROR STOP 92
      CALL b1%SUB(n2,n1)
      IF ( tag .NE. '3' ) ERROR STOP 93
      CALL b1%SUB(n2,n2)
      IF ( tag .NE. '3' ) ERROR STOP 94
      CALL c1%SUB(n2,b1)
      IF ( tag .NE. '3' ) ERROR STOP 95
      CALL c1%SUB(n2,c1)
      IF ( tag .NE. '3' ) ERROR STOP 96
      CALL c1%SUB(n2,c2)
      IF ( tag .NE. '3' ) ERROR STOP 97
      CALL c1%SUB(n2,n1)
      IF ( tag .NE. '3' ) ERROR STOP 98
      CALL c1%SUB(n2,n2)
      IF ( tag .NE. '3' ) ERROR STOP 99
      CALL c2%SUB(n2,b1)
      IF ( tag .NE. '3' ) ERROR STOP 100
      CALL c2%SUB(n2,c1)
      IF ( tag .NE. '3' ) ERROR STOP 101
      CALL c2%SUB(n2,c2)
      IF ( tag .NE. '3' ) ERROR STOP 102
      CALL c2%SUB(n2,n1)
      IF ( tag .NE. '3' ) ERROR STOP 103
      CALL c2%SUB(n2,n2)
      IF ( tag .NE. '3' ) ERROR STOP 104
      CALL n1%SUB(n2,b1)
      IF ( tag .NE. '3' ) ERROR STOP 105
      CALL n1%SUB(n2,c1)
      IF ( tag .NE. '3' ) ERROR STOP 106
      CALL n1%SUB(n2,c2)
      IF ( tag .NE. '3' ) ERROR STOP 107
      CALL n1%SUB(n2,n1)
      IF ( tag .NE. '3' ) ERROR STOP 108
      CALL n1%SUB(n2,n2)
      IF ( tag .NE. '3' ) ERROR STOP 109
      CALL n2%SUB(n2,b1)
      IF ( tag .NE. '3' ) ERROR STOP 110
      CALL n2%SUB(n2,c1)
      IF ( tag .NE. '3' ) ERROR STOP 111
      CALL n2%SUB(n2,c2)
      IF ( tag .NE. '3' ) ERROR STOP 112
      CALL n2%SUB(n2,n1)
      IF ( tag .NE. '3' ) ERROR STOP 113
      CALL n2%SUB(n2,n2)
      IF ( tag .NE. '3' ) ERROR STOP 114

      END PROGRAM Generic_TypeBound04a
