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
!*  DESCRIPTION                : rename a public operator unary with access via one only
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
  real :: a=1.0,b=2.0,c

  c=.square.b
  print *,c

end program