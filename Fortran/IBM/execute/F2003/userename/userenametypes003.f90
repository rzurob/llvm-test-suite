!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: userenametypes003.f
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
!*  DESCRIPTION                : rename a public operator unary with intrinsics
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m

   type baseobj
      integer :: x = 0
      integer :: y = 0
      real :: r = 0.0

   end type

   interface operator(.char.)
     module procedure givechar
   end interface

   interface operator(.count.)
     module procedure countarr
   end interface

   contains


   function countarr ( a )
      logical, intent(in) :: a(5)
      integer :: countarr

      countarr=count(a)
      print *, 'countarr'
   end function

   function givechar ( a )
      integer, intent(in) :: a
      character :: givechar

      givechar=char(a)
      print *, 'givechar'
   end function





end module


program mainprog
   use m, operator (.countlog.) => operator(.count.), operator (.givechar.) => operator(.char.)
   logical ::log1(5)=(/.true.,.true.,.false.,.true.,.false./)
   integer :: i2=68,result=0
   character :: c

   result=.countlog. log1
   c=.givechar. i2

   print *, result
   print *, c

end program
