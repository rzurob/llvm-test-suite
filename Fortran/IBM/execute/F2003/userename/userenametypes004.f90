!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: userenametypes004.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : userenametypes004.f
!*
!*  PROGRAMMER                 : Rob Wheeler
!*  DATE                       : Mar. 30, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Rename operator in  USE statement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                : rename a public operator binary with intrinsics
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m

   type baseobj
      integer :: x = 0
      integer :: y = 0
      real :: r = 0.0
      
   end type

   interface operator(.diff.)
     module procedure diff
   end interface
  
   interface operator(.counts.)
     module procedure countarrs
   end interface
   
   contains

   
   function countarrs ( a,b )
      logical, intent(in) :: a(5),b(5)
      integer :: countarrs

      countarrs=count(a)+count(b)
      print *, 'countarr'
   end function
   
   function diff ( a,b )
      real, intent(in) :: a,b
      real :: diff

      diff=dim(a,b)
      print *, 'diff'
   end function
   
  
   


end module


program mainprog
   use m, operator (.countlogs.) => operator(.counts.), operator (.diffgreater.) => operator(.diff.)
   logical ::log1(5)=(/.true.,.true.,.false.,.true.,.false./)
   logical ::log2(5)=(/.true.,.true.,.false.,.true.,.false./)
   integer :: result=0
   real  :: result2=0.0,r1=7.0,r2=3.0
   
   result=log2 .countlogs. log1
   result2=r1 .diffgreater. r2
   
   print *, result
   print *, result2

end program
