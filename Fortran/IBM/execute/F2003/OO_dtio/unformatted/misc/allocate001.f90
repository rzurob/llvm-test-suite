!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 21/03/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: allocate statement with source= a function return
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
      character(3) :: c
      contains
         procedure, pass :: write
   end type

   type, extends(base) :: child
      integer(4) :: i
   end type

contains

   class(base) function write(dtv)
      class(base), intent(in) :: dtv
      allocatable :: write
      allocate ( write, source = dtv )
      print *, 'hi'
   end function

end module

program allocate001 
   use m

   class(base), allocatable :: b1
   type(base)               :: b2 = base('IBM')

   allocate ( b1, source = b2%write()  ) !<- writes b2

end program

