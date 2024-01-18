!*  ===================================================================
!*
!*                               DTP - Generic Interface
!*
!*  DATE                       : October 02, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY SUBROUTINES TESTED   : Generic Resolution - Derived-type parameters
!*  SECONDARY SUBROUTINES TESTED : Resolution for polymorphic objects (type compatible)
!*                                 based on the number of arguments
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : GENERIC
!*
!*  DESCRIPTION                :
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      MODULE Mod1
      IMPLICIT NONE

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2)
        INTEGER, KIND :: k2
      END TYPE Child

      TYPE, EXTENDS(Child) :: NextGen
      END TYPE NextGen

      INTERFACE SUB
         SUBROUTINE sub1(Obj)
            IMPORT BASE
            CLASS(Base(4,*)) :: Obj
         END SUBROUTINE sub1

         SUBROUTINE sub2(Obj)
            IMPORT CHILD
            CLASS(Child(4,*,8)) :: Obj  !distinguish by kind parameter not present in sub1
         END SUBROUTINE sub2
      END INTERFACE

      END MODULE Mod1
!*
      PROGRAM Generic_Interface_diag02

      END PROGRAM Generic_Interface_diag02
