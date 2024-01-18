
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : GENERICS
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*                             :
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : C1112: generic-spec in use only statement shall not specify
!*                                      a generic binding
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type base
      integer :: i
      contains
         procedure, pass :: assign 
         procedure, pass :: assignint
         generic :: assignment =>assign, assignint
   end type

   contains

      subroutine assign(a,b)
         class(base), intent(out) :: a
         class(base), intent(in) :: b

         a%i = b%i

      end subroutine 

      subroutine assignint(a,b)
         class(base), intent(out) :: a
         integer, intent(in) :: b

         a%i = b

      end subroutine 
end module

program genericC1112Assignment001d
   use m, only: assignment(=)
end program
