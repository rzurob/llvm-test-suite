!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Generic_TypeBound04c
!*                               DTP - Generic Type-Bound
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : October 02, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY SUBROUTINES TESTED   : Generic Resolution - Derived-type parameters
!*  SECONDARY SUBROUTINES TESTED : Resolution for polymorphic objects based on
!*                                 type incompatibility of the passed object dummy argument
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

        CHARACTER(l) :: tag
      END TYPE Base 

      TYPE, EXTENDS(Base) :: Child1 (k1,l1)
        INTEGER, KIND :: k1 
        INTEGER, LEN :: l1 

        CONTAINS 
         PROCEDURE, PASS :: sub1
         GENERIC :: SUB =>  sub1
      END TYPE Child1 

      TYPE, EXTENDS(Base) :: Child2 (k2,l2)
        INTEGER, KIND :: k2 
        INTEGER, LEN :: l2 

        CONTAINS 
         PROCEDURE, PASS :: sub2
         GENERIC :: SUB =>  sub2
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
      SUBROUTINE sub1(this,Obj)
      CLASS(Child1(4,*,4,*)) :: this
      CLASS(Base(4,*)) :: Obj

      Obj%tag = '1'

      END SUBROUTINE sub1

      SUBROUTINE sub2(this,Obj)
      CLASS(Child2(4,*,4,*)) :: this
      CLASS(Base(4,*)) :: Obj

      Obj%tag = '2'

      END SUBROUTINE sub2

      END MODULE Mod1
!*
      PROGRAM Generic_TypeBound04c
      USE MOD1
      IMPLICIT NONE 

      TYPE(BAse(4,10)) :: b1 
      TYPE(Child1(4,5,4,10)), TARGET  :: c1 
      TYPE(NextGen2(4,10,4,2,8,20)) :: n2     

      CLASS(Base(4,:)), POINTER :: poly_b1 
      CLASS(Child1(4,5,4,10)), POINTER :: poly_c1 
      CLASS(NextGen2(4,:,4,:,8,:)), POINTER :: poly_n2     

!* Test 1: allocation of dynamic objects 
      ALLOCATE(Base(4,10):: poly_b1)
      poly_c1 =>  c1
      ALLOCATE(poly_n2, source=n2)

!* call to sub1      
      CALL c1%SUB(b1)
      IF ( b1%tag .NE. '1' ) STOP 10
      CALL c1%SUB(c1)
      IF ( c1%tag .NE. '1' ) STOP 11
      CALL c1%SUB(n2)
      IF ( n2%tag .NE. '1' ) STOP 12

      CALL poly_c1%SUB(poly_b1)
      IF ( poly_b1%tag .NE. '1' ) STOP 13
      CALL poly_c1%SUB(b1)
      IF ( b1%tag .NE. '1' ) STOP 14
      CALL poly_c1%SUB(poly_c1)
      IF ( poly_c1%tag .NE. '1' ) STOP 15
      CALL poly_c1%SUB(poly_n2)
      IF ( poly_n2%tag .NE. '1' ) STOP 16

!* call to sub2      
      CALL n2%SUB(b1)
      IF ( b1%tag .NE. '2' ) STOP 17
      CALL n2%SUB(c1)
      IF ( c1%tag .NE. '2' ) STOP 18
      CALL n2%SUB(n2)
      IF ( n2%tag .NE. '2' ) STOP 19

      CALL poly_n2%SUB(poly_b1)
      IF ( poly_b1%tag .NE. '2' ) STOP 20
      CALL poly_n2%SUB(poly_c1)
      IF ( poly_b1%tag .NE. '2' ) STOP 21
      CALL poly_n2%SUB(poly_n2)
      IF ( poly_n2%tag .NE. '2' ) STOP 22

      END PROGRAM Generic_TypeBound04c
