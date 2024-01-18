!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*                             :
!*  SECONDARY FUNCTIONS TESTED : with Assignment( )
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : C459: functional TC, private and public specific typebound
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
      integer :: i = -999
      contains
         generic :: assignment(=) => btob
         procedure, pass ( a ) :: btob
         generic, public :: assignment(=) => btoi
         procedure, private, pass( a ) :: btoi
   end type

   contains

   subroutine btob ( a, b )
      class(base) :: a, b
      intent( out ) :: a
      intent( in )  :: b

      a%i = b%i + 1

   end subroutine

   subroutine btoi ( a, b )
      class(base) :: a
      integer :: b
      intent( in ) :: b
      intent( out )  :: a

      a%i = b + 3

   end subroutine

end module

program genericC459Assignment003
   use m

   class(base) , allocatable :: b1, b2
   allocate ( b1, b2 )

   b2 = base( 200 )
   b1 = b2

   if ( b1%i /= 202 ) error stop 1_4

   b2 = 10
   b1 = b2

   if ( ( b1%i /= 14 ) .or. ( b2%i /= 13 ) )   error stop 2_4

end program
