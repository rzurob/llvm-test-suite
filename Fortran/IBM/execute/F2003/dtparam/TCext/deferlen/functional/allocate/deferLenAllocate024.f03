! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qdeferredlp /tstdev/F2003/deferlen/functional/allocate/deferLenAllocate024.f
! opt variations: -qnock -qnok -qnol -qnodeferredlp

!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DESCRIPTION                : scalar character with deferred length scalar
!*                               with MAX/MIN
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

   character(:), pointer :: c1
   character(:), allocatable :: c2

   type base(k1,n1)    ! (4,20)
       integer, kind :: k1
       integer, len  :: n1
      character(:), allocatable :: c
   end type

   type(base(4,:)), allocatable :: b1

end module

program deferLenAllocate024
   use m

   allocate ( c1, source = "abcdefghi" )
   allocate ( c2, source = "abcdefghi" )
   allocate ( b1, source = base(4,20)("abcdefghi") )

   ! same

   print *, max ( c1, c2, b1%c )
   print *, min ( c1, c2, b1%c )

   ! different

   deallocate ( c2 )
   allocate ( c2, source = "AB" )

   deallocate ( b1%c )
   allocate ( b1%c, source = "CDEF" )

   print *, max ( c1, c2, b1%c )
   print *, min ( c1, c2, b1%c )

   print *, max ( c1, c2, b1%c, 'aa', 'aaa', 'z' )
   print *, min ( c1, c2, b1%c, 'A', 'aaa', 'z' )

end program
