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
!*  DESCRIPTION                : scalar character with deferred length scalar
!*                               with finalization
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

   type base
      character(:), pointer :: c => null()
      contains
         final :: basefinal
   end type

   contains

      subroutine basefinal( dtv )
         type(base), intent(inout) :: dtv

         print *, "basefinal"
         if ( associated ( dtv%c ) ) then
            print *, 'length: ', len(dtv%c), dtv%c
            deallocate ( dtv%c )
         end if

      end subroutine

end module

program deferLenAllocate025
   use m

   class(base), allocatable :: b1
   
   character(:), pointer :: c
   
   allocate ( c, source = 'aaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbbcccccccccccccccccccccccccc' )

   allocate ( b1, source = base() )
   allocate ( b1%c, source = 'abcdefghijkl' )

   deallocate ( b1 )
   
   allocate ( b1 )

   allocate ( b1%c, source = '' )
   deallocate ( b1 )
   
   allocate ( b1 )
   b1%c => c
   
   deallocate ( b1 )

end program
