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
!*  DESCRIPTION                : rename a public operator unary mixed case
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module opmod

  public :: operator(.realed.)

  interface operator(.realed.)
    module procedure sqreal
  end interface

  contains
    function sqreal(a)
      real :: sqreal
      real, intent(in) :: a
      sqreal=a*a
    end function sqreal

end module


program main

 use opmod , operator(.realed.) => operator(.realed.)
  real :: a=1.0,b=2.0,c

  c=.realed.b
  print *,c

end program