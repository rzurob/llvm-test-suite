!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Generic_TypeBound04b
!*                               DTP - Generic Type-Bound
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : October 02, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY SUBROUTINES TESTED   : Generic Resolution - Derived-type parameters
!*  SECONDARY SUBROUTINES TESTED : Resolution for polymorphic objects
!*                                 based on type incompatibility and number of arguments 
!*                     
!*
!*  DRIVER STANZA              : xlf2003
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
      IF ( .NOT. ASSOCIATED(pntr)) STOP 1

      tag = '1'

      END SUBROUTINE sub1

      SUBROUTINE sub2(Obj, Arg1, Arg2)
      CLASS(Base(4,*)) ::  Arg1, Arg2
      CLASS(Child1(4,*,4,*)) :: Obj
      CLASS(Base(4,:)), POINTER :: pntr

      ALLOCATE (pntr, source = Arg1)
      IF ( .NOT. ASSOCIATED(pntr)) STOP 2

      tag = '2'

      END SUBROUTINE sub2

      SUBROUTINE sub3(Obj, Arg1, Arg2)
      CLASS(Base(4,*)) ::  Arg1, Arg2
      CLASS(Child2(4,*,4,*)) :: Obj
      CLASS(Base(4,:)), POINTER :: pntr

      ALLOCATE (pntr, source = Arg2)
      IF ( .NOT. ASSOCIATED(pntr)) STOP 3

      tag = '3'

      END SUBROUTINE sub3

      END MODULE Mod1
!*
      PROGRAM Generic_TypeBound04b
      USE MOD1
      IMPLICIT NONE 

      TYPE(Child1(4,5,4,10)), TARGET  :: c1 
      TYPE(NextGen2(4,10,4,2,8,20)) :: n2     

      CLASS(Base(4,:)), POINTER :: poly_b1 
      CLASS(Child1(4,5,4,10)), POINTER :: poly_c1 
      CLASS(NextGen2(4,:,4,:,8,:)), ALLOCATABLE :: poly_n2     

!* Test 1: allocation of dynamic objects 
      ALLOCATE(Base(4,10):: poly_b1)
      poly_c1 =>  c1
      ALLOCATE(poly_n2, source=n2)

!*  Only one passed object dummy argument : call to sub1      
      CALL poly_b1%SUB()
      IF ( tag .NE. '1' ) STOP 10
      CALL poly_c1%SUB()
      IF ( tag .NE. '1' ) STOP 11
      CALL poly_n2%SUB()
      IF ( tag .NE. '1' ) STOP 12

!*  The first non-passed argument poly_c1 is of dynamic type Child1 : call to sub2
      CALL poly_b1%SUB(poly_c1,poly_b1)
      IF ( tag .NE. '2' ) STOP 13
      CALL poly_b1%SUB(poly_c1,poly_c1)
      IF ( tag .NE. '2' ) STOP 14
      CALL poly_b1%SUB(poly_c1,poly_n2)
      IF ( tag .NE. '2' ) STOP 15

      CALL poly_c1%SUB(poly_c1,poly_b1)
      IF ( tag .NE. '2' ) STOP 16
      CALL poly_c1%SUB(poly_c1,poly_c1)
      IF ( tag .NE. '2' ) STOP 17
      CALL poly_c1%SUB(poly_c1,poly_n2)
      IF ( tag .NE. '2' ) STOP 18

      CALL poly_n2%SUB(poly_c1,poly_b1)
      IF ( tag .NE. '2' ) STOP 19
      CALL poly_n2%SUB(poly_c1,poly_c1)
      IF ( tag .NE. '2' ) STOP 20
      CALL poly_n2%SUB(poly_c1,poly_n2)
      IF ( tag .NE. '2' ) STOP 21

!*  The first non-passed argument poly_n2 is of dynamic type NextGen2 : call to sub3
      CALL poly_b1%SUB(poly_n2,poly_b1)
      IF ( tag .NE. '3' ) STOP 22
      CALL poly_b1%SUB(poly_n2,poly_c1)
      IF ( tag .NE. '3' ) STOP 23
      CALL poly_b1%SUB(poly_n2,poly_n2)
      IF ( tag .NE. '3' ) STOP 24

      CALL poly_c1%SUB(poly_n2,poly_b1)
      IF ( tag .NE. '3' ) STOP 25
      CALL poly_c1%SUB(poly_n2,poly_c1)
      IF ( tag .NE. '3' ) STOP 26
      CALL poly_c1%SUB(poly_n2,poly_n2)
      IF ( tag .NE. '3' ) STOP 27

      CALL poly_n2%SUB(poly_n2,poly_b1)
      IF ( tag .NE. '3' ) STOP 28
      CALL poly_n2%SUB(poly_n2,poly_c1)
      IF ( tag .NE. '3' ) STOP 29
      CALL poly_n2%SUB(poly_n2,poly_n2)
      IF ( tag .NE. '3' ) STOP 30

      END PROGRAM Generic_TypeBound04b
