!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : argAssocPtr001.f
!*
!*  PROGRAMMER                 : Vicram Uppal
!*  DATE                       : 04/03/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : 
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Passing a char target to an internal  
!*                               subroutine which has a char pointer
!*  				 which points to that target. Test  
!*				 associatoin between the two chars. 
!*
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

      character(4), target :: a

end module

program argAssocPtr001

   use m
   
   a = 'wxyz'

   call tsub(a)

   contains

      subroutine tsub(k)

         character(4), value, target :: k
         character(4), pointer :: pp

         pp => k

         if (associated(pp, a)) error stop 1_4

         if (.not. associated(pp, k)) error stop 2_4

      end subroutine

   end
