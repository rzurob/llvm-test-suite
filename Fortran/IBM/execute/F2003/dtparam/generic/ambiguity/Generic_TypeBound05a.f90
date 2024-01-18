!*  ===================================================================
!*
!*                               DTP - Generic Type-Bound
!*
!*  DATE                       : October 02, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY SUBROUTINES TESTED   : Generic Resolution - Derived-type parameters
!*  SECONDARY SUBROUTINES TESTED : Resolution for polymorphic objects
!*                                 non-passed object dummy argument is an assumed-shape array
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

        CHARACTER(l1) :: tag

        CONTAINS
         PROCEDURE, NOPASS :: sub2
         PROCEDURE, NOPASS :: sub1
         GENERIC :: SUB => sub1, sub2
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2)
        INTEGER, KIND :: k2
      END TYPE Child

      TYPE, EXTENDS(Child) :: NextGen(k3)
        INTEGER, KIND :: k3
      END TYPE NextGen

      CONTAINS
!*
      SUBROUTINE sub1(Obj)
      CLASS(Base(4,*)) :: Obj(:)

      Obj%tag = '1'

      END SUBROUTINE sub1

      SUBROUTINE sub2(Obj)
      CLASS(Base(4,*)) :: Obj(:,:,:,:)

      Obj%tag = '2'

      END SUBROUTINE sub2

      END MODULE Mod1
!*
      PROGRAM Generic_TypeBound05a
      USE MOD1
      IMPLICIT NONE

      CLASS(Base(4,:)), POINTER :: poly1(:), poly2(:,:,:,:)
      TYPE(Base(4,5))  :: base1(1024)
      TYPE(NextGen(4,10,4,4)), TARGET :: tgt2(1,1,1,1)

      ALLOCATE(Base(4,10):: poly1(10))
      poly2 => tgt2

      CALL base1%SUB(base1)
      IF ( ANY(base1%tag .NE. '1') ) STOP 11
      CALL poly1%SUB(base1)
      IF ( ANY(base1%tag .NE. '1') ) STOP 12
      CALL poly2%SUB(base1)
      IF ( ANY(base1%tag .NE. '1') ) STOP 13

      CALL poly1%SUB(poly1)
      IF ( ANY(poly1%tag .NE. '1') ) STOP 14
      CALL poly2%SUB(poly1)
      IF ( ANY(poly1%tag .NE. '1') ) STOP 15

      CALL poly1%SUB(poly2)
      IF ( ANY(poly2%tag .NE. '2') ) STOP 16
      CALL poly2%SUB(poly2)
      IF ( ANY(poly2%tag .NE. '2') ) STOP 17

      END PROGRAM Generic_TypeBound05a
