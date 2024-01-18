!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: userenamenest002.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 30, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Rename operator in  USE statement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : rename a public operator binary nested module
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

   function mybadd ( a, b )
      class(baseobj), intent(in) :: a
      type(baseobj), intent(in)  :: b

      type(baseobj) :: mybadd

      mybadd%x = a%x + b%x
      mybadd%y = a%y + b%y

      print *, 'mybadd'

   end function

   function mybadd2 ( a, b )
      integer, intent(in) :: a
      integer, intent(in)  :: b

      integer :: mybadd2

      mybadd2 = a + b + 1000


      print *, 'mybadd2'

   end function



end module
module m2
use m, operator (.plus.) => operator(.add.)
end module

program mainprog
   use m2

   type(baseobj), target      :: b1
   type(baseobj), pointer     :: b2
   type(baseobj), allocatable :: b3
   integer :: i1=5,i2=5,i3=0
   b1 = baseobj ( 100, 200 )

   allocate ( b2, source = baseobj ( 101, 201 ) )
   allocate ( b3, source = baseobj () )

   b3 = b1 .add. b2
   print *, b3%x

   i3 = i1 .plus. i2
   print *, i3

end program
