! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodeferredlp /tstdev/F2003/deferlen/functional/allocate/deferLenAllocate025.f
! opt variations: -qck -qnok -qnol -qdeferredlp

!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
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

   type base(k1,n1)    ! (4,20)
       integer, kind :: k1
       integer, len  :: n1
      character(:), pointer :: c => null()
      contains
         final :: basefinal
   end type

   contains

      subroutine basefinal( dtv )
         type(base(4,*)), intent(inout) :: dtv

         print *, "basefinal"
         if ( associated ( dtv%c ) ) then
            print *, 'length: ', len(dtv%c), dtv%c
            deallocate ( dtv%c )
         end if

      end subroutine

end module

program deferLenAllocate025
   use m

   class(base(4,20)), allocatable :: b1

   character(:), pointer :: c

   allocate ( c, source = 'aaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbbcccccccccccccccccccccccccc' )

   allocate ( b1, source = base(4,20)() )
   allocate ( b1%c, source = 'abcdefghijkl' )

   deallocate ( b1 )

   allocate ( b1 )

   allocate ( b1%c, source = '' )
   deallocate ( b1 )

   allocate ( b1 )
   b1%c => c

   deallocate ( b1 )

end program