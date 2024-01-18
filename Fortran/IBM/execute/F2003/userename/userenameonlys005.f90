!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: userenameonlys005.f
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
!*  DESCRIPTION                : rename a public operator unary with multiple renames
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module opmod

  public :: operator(.realed.)

  interface operator(.realed.)
    module procedure sqreal
  end interface

  interface operator(.halved.)
    module procedure cut
  end interface

  contains
    function sqreal(a)
      real :: sqreal
      real, intent(in) :: a
      sqreal=a*a
    end function sqreal

    function cut(a)
      real :: cut
      real, intent(in) :: a
      cut=a/2
    end function cut
end module


program main
 use opmod , only: operator(.half.) => operator(.halved.)
 use opmod , only: operator(.square.) => operator(.realed.)
 use opmod , only: operator(.bisect.) => operator(.halved.)
 use opmod , only: operator(.multitself.) => operator(.realed.)
  real :: a=4.0,b=2.0,c

  c=.square.a
  print *,c
  c=.multitself.a
  print *,c

  c=.half.b
  print *,c
  c=.bisect.b
  print *,c

end program