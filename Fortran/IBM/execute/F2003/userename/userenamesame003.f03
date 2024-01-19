!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 30, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Rename operator in  USE statement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : rename a public operator unary same name as a type bound
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m

   type baseobj
      integer :: x = -999
      integer :: y = -999
      contains
         procedure :: mybadd
         generic :: operator(.add.) => mybadd
   end type

   interface operator(.add.)
   		module procedure mybadd2
   end interface

   contains

   function mybadd ( a )
      class(baseobj), intent(in) :: a

      type(baseobj) :: mybadd

      mybadd%y = a%x + a%y
      mybadd%x = a%x + a%y + 1000

      print *, 'mybadd'

   end function

   function mybadd2 ( a )
      integer, intent(in) :: a
      integer :: mybadd2

      mybadd2 = a + 1000


      print *, 'mybadd2'

   end function



end module

program mainprog
   use m, operator (.plus.) => operator(.add.)

   type(baseobj), target      :: b1
   type(baseobj), pointer     :: b2
   type(baseobj), allocatable :: b3
   integer :: i1=5,i2=5,i3=0
   b1 = baseobj ( 100, 200 )

   allocate ( b3, source = baseobj () )

   b3 = .add. b1
   print *, b3%x

   i3 = .plus. i2
   print *, i3

end program
