!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Generic_Interface_diag01
!*                               DTP - Generic Interface
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : October 02, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY SUBROUTINES TESTED   : Generic Resolution - Derived-type parameters
!*  SECONDARY SUBROUTINES TESTED : Resolution for polymorphic objects (type compatible)
!*                                 based on the number of arguments
!*                     
!*
!*  DRIVER STANZA              : xlf20_diag01
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : GENERIC
!*
!*  DESCRIPTION                :
!*
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

      TYPE, EXTENDS(Child) :: NextGen(k3)
        INTEGER, KIND :: k3 
      END TYPE NextGen

      INTERFACE SUB 
         SUBROUTINE sub_1arg(Obj)
            IMPORT BASE, CHILD, NEXTGEN
            CLASS(Base(4,*)) :: Obj
            CLASS(Base(4,:)), POINTER  :: pntr 
         END SUBROUTINE sub_1arg

         SUBROUTINE sub_2arg(Obj,Arg)
            IMPORT BASE, CHILD, NEXTGEN
            CLASS(Base(4,*)), OPTIONAL :: Obj, Arg 
            CLASS(Base(4,:)), POINTER  :: pntr 
         END SUBROUTINE sub_2arg
      END INTERFACE

      END MODULE Mod1
!*
      PROGRAM Generic_Interface_diag01

      END PROGRAM Generic_Interface_diag01
