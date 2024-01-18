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
!*  DESCRIPTION                : array character with deferred length with
!*                               argument association
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
   character(:), allocatable :: c1(:)
end module

program deferLenArgAsso002
   use n

   allocate ( c1(3), source = (/ "one", "two", "333" /) )
   print *, c1, "|", len(c1)

   call foo ( c1 )

   contains

      subroutine foo ( c )
         character(:), allocatable :: c(:)

         if ( allocated ( c ) ) then
            print *, len(c), size(c), c
         end if
      end subroutine

end program