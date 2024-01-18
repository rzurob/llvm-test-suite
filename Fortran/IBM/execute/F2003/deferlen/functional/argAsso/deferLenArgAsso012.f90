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
!*  DESCRIPTION                : character with deferred length
!*                               in user-defined assignment
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

   type base
      character(:), allocatable :: c
   end type

   interface! assignment(=)
      subroutine mycharassignment(a,b)
         import base
         type(base), intent(out)  :: a
         character(:), allocatable, intent(in) :: b
      end subroutine
   end interface

end module

subroutine mycharassignment(a,b)
   use n, only: base
   type(base), intent(out)  :: a
   character(:), allocatable, intent(in) :: b

   if( allocated( a%c ) ) deallocate ( a%c )
   print *, "inside assignment: ", b, len(b)
   allocate ( a%c, source = b )

end subroutine

program deferLenArgAsso012
   use n

   type(base), pointer :: b1
   character(:), allocatable :: c1

   allocate ( b1, source = base(' ') )
   allocate ( c1, source = 'XLFortran' )

   print *, b1%c, len(b1%c)

 !  b1 = c1

   call mycharassignment(b1, c1)

   print *, b1%c, len(b1%c)

end program
