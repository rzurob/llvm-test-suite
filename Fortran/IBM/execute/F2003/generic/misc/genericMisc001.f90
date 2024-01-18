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
!*  SECONDARY FUNCTIONS TESTED : misc.
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Structure Constructor for derived type
!*                               containing allocatable component and allocate it with
!*                               allocatable function result
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

  type inner
      integer :: j = -999
      contains
         procedure :: iadd
   end type

   type base
      integer :: x = -999
      type(inner), allocatable :: i
   end type

   contains

      type(inner) function iadd ( a, b ) result(abc)
         class(inner), intent(in) :: a, b
         allocatable :: abc
         allocate ( abc )
         abc%j = a%j + b%j
      end function

end module

program genericMisc001
   use m

   class(base), allocatable :: b1
   type(inner) :: i1, i2

   i1%j = 100
   i2%j = 200

   associate ( g => base ( x = 10, i = iadd( i1 , i2 ) ) )
      if ( .not. allocated(g%i) ) error stop 1_4
      if ( ( g%x /= 10 ) .or. ( g%i%j /= 300 ) ) error stop 2_4
   end associate

end
