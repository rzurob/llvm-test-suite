!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sept. 22, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : generalization of module procedure
!*                               stmts, by making the MODULE keyword
!*                               optional. These statements are called
!*                               procedure statements in F2003.
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This diagnostic test, makes sure that
!*                               if MODULE is not specified and the identifier
!*                               refers to a variable, it is flagged.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      integer :: int_var

      interface generic_name
         procedure int_var
      end interface

      call generic_name()

      end

