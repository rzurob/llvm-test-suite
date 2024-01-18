!*  ===================================================================
!*
!*                               DTP - Generic Assignment
!*
!*  DATE                       : October 02, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : Generic Resolution - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : Resolution by type incompatibility
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

        INTEGER :: value
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child1 (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1

        CONTAINS
         PROCEDURE, PASS(this) :: assgn1
         GENERIC :: assignment(=) => assgn1
      END TYPE Child1

      TYPE, EXTENDS(Base) :: Child2 (k2,l2)
        INTEGER, KIND :: k2
        INTEGER, LEN :: l2

        CONTAINS
         PROCEDURE, PASS(this) :: assgn2
         GENERIC :: assignment(=) => assgn2
      END TYPE Child2

      TYPE, EXTENDS(Child1) :: NextGen1 (k13,l13)
        INTEGER, KIND :: k13
        INTEGER, LEN :: l13
      END TYPE NextGen1

      TYPE, EXTENDS(Child1) :: NextGen2 (k23,l23)
        INTEGER, KIND :: k23
        INTEGER, LEN :: l23
      END TYPE NextGen2

      CONTAINS
!*
      SUBROUTINE assgn1(this,obj)
      CLASS(Child1(4,*,4,*)), INTENT(OUT) :: this
      CLASS(Base(4,*)), INTENT(IN)  :: obj

      this%value = 1

      END SUBROUTINE

      SUBROUTINE assgn2(this,obj)
      CLASS(Child2(4,*,4,*)), INTENT(OUT) :: this
      CLASS(Base(4,*)), INTENT(IN)  :: obj

      this%value = 2

      END SUBROUTINE

      END MODULE Mod1
!*
      PROGRAM Generic_Assign01
      USE MOD1
      IMPLICIT TYPE(Base(4,10))(B)

      IMPLICIT TYPE(Child1(4,1,4,1))(C)
      IMPLICIT TYPE(NextGen1(4,1,4,1,8,1))(N)

      IMPLICIT TYPE(Child2(4,1,4,1))(K)
      IMPLICIT TYPE(NextGen2(4,1,4,1,8,1))(M)

      C2 = B1
      IF (C2%value .NE. 1) STOP 10
      C2 = C1
      IF (C2%value .NE. 1) STOP 11
      C2 = N1
      IF (C2%value .NE. 1) STOP 12

      N2 = B1
      IF (N2%value .NE. 1) STOP 13
      N2 = C1
      IF (N2%value .NE. 1) STOP 14
      N2 = N1
      IF (N2%value .NE. 1) STOP 15

      K2 = B1
      IF (K2%value .NE. 2) STOP 16
      K2 = K1
      IF (K2%value .NE. 2) STOP 17
      K2 = M1
      IF (K2%value .NE. 2) STOP 18

      M2 = B1
      IF (M2%value .NE. 1) STOP 19
      M2 = K1
      IF (M2%value .NE. 1) STOP 20
      M2 = M1
      IF (M2%value .NE. 1) STOP 21

      END PROGRAM Generic_Assign01
