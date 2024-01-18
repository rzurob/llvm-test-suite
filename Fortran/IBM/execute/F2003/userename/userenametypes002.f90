!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: userenamenest004.f
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
!*  DESCRIPTION                : rename a public operator binary nested module, multiple renames
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m

   type baseobj
      integer :: x = 0
      integer :: y = 0
      real :: r = 0.0

   end type

   interface operator(.multiply.)
     module procedure mult2
   end interface
   interface operator(.divide.)
     module procedure div2
   end interface
   interface operator(.add.)
     module procedure addit2
   end interface

   contains

   function addit ( a, b )
      class(baseobj), intent(in) :: a
      type(baseobj), intent(in)  :: b
      type(baseobj) :: addit

      addit%x = a%x + b%x
      addit%y = a%y + b%y
      addit%r = a%r + b%r
      print *, 'add'
   end function

   function mult ( a, b )
      class(baseobj), intent(in) :: a
      type(baseobj), intent(in)  :: b
      type(baseobj) :: mult

      mult%x = a%x * b%x
      mult%y = a%y * b%y
      mult%y = a%r * b%r
      print *, 'mult'
   end function

   function div ( a, b )
      class(baseobj), intent(in) :: a
      type(baseobj), intent(in)  :: b
      type(baseobj) :: div

      div%x = a%x / b%x
      div%y = a%y / b%y
      div%r = a%r / b%r
      print *, 'div'
   end function

   function addit2 ( a, b )
      class(baseobj), intent(in) :: a
      type(baseobj), intent(in)  :: b
      type(baseobj) :: addit2

      addit2%x = a%x + b%x
      addit2%y = a%y + b%y
      addit2%r = a%r + b%r
      print *, 'add2'
   end function

   function mult2 ( a, b )
      class(baseobj), intent(in) :: a
      type(baseobj), intent(in)  :: b
      type(baseobj) :: mult2

      mult2%x = a%x * b%x
      mult2%y = a%y * b%y
      mult2%r = a%r * b%r
      print *, 'mult2'
   end function

   function div2 ( a, b )
      class(baseobj), intent(in) :: a
      type(baseobj), intent(in)  :: b
      type(baseobj) :: div2

      div2%x = a%x / b%x
      div2%y = a%y / b%y
      div2%r = a%r / b%r
      print *, 'div2'
   end function



end module


program mainprog
   use m, operator (.plus.) => operator(.add.), operator (.divit.) => operator(.divide.),operator (.multit.) => operator(.multiply.)

   type(baseobj), target      :: b1
   type(baseobj), pointer     :: b2
   type(baseobj), allocatable :: b3
   b1 = baseobj ( 100, 200,2.0 )
   allocate ( b2, source = baseobj ( 101, 201 ,3.0) )
   allocate ( b3, source = baseobj () )

   b3 = b2 .plus. b1
   print *, b3%x
   print *, b3%y
   print *, b3%r

   b3 = b2 .divit. b1
   print *, b3%x
   print *, b3%y
   print *, b3%r

   b3 = b2 .multit. b1
   print *, b3%x
   print *, b3%y
   print *, b3%r



end program
