!*  ===================================================================
!*
!*                               DTP - Generic Operator
!*
!*  DATE                       : October 03, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : Generic Resolution - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : Resolution based on rank - assumed size array
!*                               rank decalred with DIMENSION vs. shape declared explicitly
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

        CONTAINS
         PROCEDURE, PASS :: mut1
         PROCEDURE, PASS :: mut2
         GENERIC :: operator(*) => mut1, mut2
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1
      END TYPE Child

      TYPE, EXTENDS(Child) :: NextGen (k13,l13)
        INTEGER, KIND :: k13
        INTEGER, LEN :: l13
      END TYPE NextGen

      CONTAINS
!*
      TYPE(Base(4,:)) FUNCTION mut1(arg1,arg2)
      CLASS(Base(4,*)), INTENT(IN) :: arg1
      CLASS(Base(4,*)), DIMENSION(:,:), INTENT(IN) :: arg2   ! rank 1
      POINTER :: mut1

      ALLOCATE(BASE(arg2%k,arg2%l) :: mut1)
      mut1%value = 14

      END FUNCTION mut1

      TYPE(Base(4,:)) FUNCTION mut2(arg1,arg2)
      CLASS(Base(4,*)), INTENT(IN) :: arg1
      CLASS(Base(4,*)), INTENT(IN) :: arg2(:,:)            ! rank 2
      POINTER :: mut2

      ALLOCATE(BASE(arg2%k,arg2%l) :: mut2)
      mut2%value = 24

      END FUNCTION mut2

      END MODULE Mod1
!*
      PROGRAM Generic_Operator_diag01

      END PROGRAM Generic_Operator_diag01
