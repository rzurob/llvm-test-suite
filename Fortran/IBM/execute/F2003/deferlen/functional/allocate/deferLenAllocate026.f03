!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DESCRIPTION                : scalar character with deferred length scalar/array in polymorphic derived types
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
      character(:), pointer :: c1(:) => null()
      contains
         final :: basefinal
   end type

   type, extends(base) :: child
      character(:), pointer :: c2(:) => null()
      contains
         final :: childfinal
   end type

   contains

       subroutine basefinal( dtv )
         type(base), intent(inout) :: dtv

         print *, "basefinal"
         if ( associated ( dtv%c1 ) ) then
            print *, 'length: ', len(dtv%c1), dtv%c1
            deallocate ( dtv%c1 )
         end if

      end subroutine

      subroutine childfinal( dtv )
         type(child), intent(inout) :: dtv

         print *, "childfinal"
         if ( associated ( dtv%c2 ) ) then
            print *, 'length: ', len(dtv%c2), dtv%c2
            deallocate ( dtv%c2 )
         end if

      end subroutine

end module

program deferLenAllocate026
   use m

   class(base), allocatable :: b1, b2(:)

   allocate ( base :: b1 )

   allocate ( b1%c1(3), source = (/ 'abc', 'def', 'ghi' /) )
   deallocate ( b1 )

   allocate ( child :: b1 )
   allocate ( b1%c1(4), source = (/ 'abcd', 'defg', 'hijk', 'lmno' /) )
   select type ( b1 )
      type is ( child )
         allocate ( b1%c2(10), source = (/ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j' /) )
   end select

   deallocate ( b1 )

end program
