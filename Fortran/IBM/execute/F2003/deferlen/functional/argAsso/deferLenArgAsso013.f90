!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 05/01/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : argument association with unlimited polymorphic
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

module n

   contains

      subroutine foo ( u )
         class(*), intent(inout) :: u

         select type ( u )
            type is ( character(*) )
               print *, u, len(u)
               u = "abcdefghijk"
         end select

      end subroutine

end module

program deferLenArgAsso013
   use n

   character(:), allocatable, target :: c1
   character(:), pointer :: c2

   allocate ( c1, source = "www.ibm.com" )

   c2 => c1

   call foo ( c1 )
   call foo ( c2 )

   print *, c1
   print *, c2

end program
