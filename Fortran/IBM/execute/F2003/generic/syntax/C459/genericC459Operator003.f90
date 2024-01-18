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
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
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
      integer i
      contains
         generic :: operator(.myadd.) => add_i
         procedure :: add_i
         generic, public :: operator(.myadd.) => add_base
         procedure, pass, private :: add_base
   end type

   contains

      function add_i ( passobj , int )
         class(base), intent(in) :: passobj
         integer, intent(in) :: int
         type(base) :: add_i
         add_i%i = passobj%i + int
      end function

      function add_base ( passobj, base )
         class(base), intent(in) :: passobj, base
         type(base) :: add_base
         add_base%i = passobj%i + base%i
      end function

end module

program genericC459Operator003
   use m

   type(base), allocatable :: b1, b2

   allocate ( base :: b1, b2 )

   b1%i = 10
   b2%i = 20

   b1 = b1 .myadd. b2
   if ( b1%i /= 30 ) error stop 1_4

   b2 = b1 .myadd. 10
   if ( b2%i /= 40 ) error stop 2_4

end program
