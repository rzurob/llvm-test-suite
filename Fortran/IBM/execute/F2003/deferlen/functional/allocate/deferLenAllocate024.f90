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

   type base
      character(:), allocatable :: c
   end type

   type(base), allocatable :: b1

end module

program deferLenAllocate024
   use m

   allocate ( c1, source = "abcdefghi" )
   allocate ( c2, source = "abcdefghi" )
   allocate ( b1, source = base("abcdefghi") )

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
