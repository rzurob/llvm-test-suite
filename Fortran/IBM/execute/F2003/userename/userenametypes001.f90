!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: userenametypes001.f
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
!*  DESCRIPTION                : rename a public operator unary with defined types
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m

   type baseobj
      integer :: x = 0
      integer :: y = 0
      real :: r = 0.0

   end type

   interface operator(.square.)
     module procedure sq
   end interface

   interface operator(.power.)
     module procedure raise
   end interface

   contains


   function raise ( a )
      class(baseobj), intent(in) :: a
       type(baseobj) :: raise

      raise%x = a%x ** 3
      raise%y = a%y ** 3
      raise%r = a%r ** 3
      print *, 'raised'
   end function

   function sq ( a )
      class(baseobj), intent(in) :: a
      type(baseobj) :: sq

      sq%x = a%x * a%x
      sq%y = a%y * a%y
      sq%r = a%r * a%r
      print *, 'sq'
   end function





end module


program mainprog
   use m, operator (.squareit.) => operator(.square.), operator (.raiseit.) => operator(.power.)

   type(baseobj), target      :: b1
   type(baseobj), pointer     :: b2
   type(baseobj), allocatable :: b3
   b1 = baseobj ( 3, 4,2.0 )
   allocate ( b2, source = baseobj ( 10, 20 ,3.0) )
   allocate ( b3, source = baseobj () )

   b3 = .squareit.b1
   print *, b3%x
   print *, b3%y
   print *, b3%r



   b3 = .raiseit. b1
   print *, b3%x
   print *, b3%y
   print *, b3%r



end program
