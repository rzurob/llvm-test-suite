!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 21/03/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Associate constructor and intent(in) variable
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program misc001d

   integer :: j = -9
   call sub(j)
   call sub1(j)

   contains
      subroutine sub(i)
         integer, intent(in) :: i
         associate( k => i)
            k = 11
         end associate
      end subroutine
      
      subroutine sub1(i)
         class(*), intent(in) :: i
         select type( k => i)
            type is ( integer )
               k = 11
         end select
      end subroutine
end

