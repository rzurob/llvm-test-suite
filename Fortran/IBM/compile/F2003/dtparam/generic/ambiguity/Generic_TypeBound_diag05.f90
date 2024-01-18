!*  ===================================================================
!*
!*                               DTP - Generic Type-Bound
!*
!*  DATE                       : October 02, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY SUBROUTINES TESTED   : Generic Resolution - Derived-type parameters
!*  SECONDARY SUBROUTINES TESTED : Resolution for polymorphic objects
!*                                 based on the number of arguments
!*                                 ambiguous PASS and NOPASS
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
         PROCEDURE, NOPASS :: sub0
         PROCEDURE, PASS :: sub1
         GENERIC :: SUB =>  sub0, sub1
      END TYPE Base

      CONTAINS
!*
      SUBROUTINE sub0
      CLASS(Base(4,:)), POINTER  :: pntr

      ALLOCATE (pntr, source = Base(4,10)() )
      IF ( .NOT. ASSOCIATED(pntr)) STOP 30

      END SUBROUTINE sub0

      SUBROUTINE sub1(Obj)
      CLASS(Base(4,*)) :: Obj
      CLASS(Base(4,:)), POINTER  :: pntr

      ALLOCATE (pntr, source = Obj)
      IF ( .NOT. ASSOCIATED(pntr)) STOP 30

      END SUBROUTINE sub1

      END MODULE Mod1
!*
      PROGRAM Generic_TypeBound_diag05

      END PROGRAM Generic_TypeBound_diag05
