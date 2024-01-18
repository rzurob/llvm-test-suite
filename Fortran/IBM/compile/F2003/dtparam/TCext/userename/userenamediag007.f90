! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/userename/userenamediag007.f
! opt variations: -qnol

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
!*  DESCRIPTION                : ensure renaming to an ambiguous type bound yields error
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module opmod

  type modreal(n1,k1)    ! (20,4)
    integer, kind :: k1
    integer, len  :: n1
    real(k1)      :: x

    contains
      procedure :: plus
      procedure :: plus2
      generic :: operator(.add.) => plus

  end type

  interface operator(.adda.)
    module procedure plus2
  end interface

  contains
    function plus(a,b)
      type(modreal(20,4)) :: plus
      class(modreal(*,4)), intent(in) :: a,b
      plus%x = a%x+b%x
    end function plus

    function plus2(a,b)
      type(modreal(20,4)) :: plus2
      class(modreal(*,4)), intent(in) :: a,b
      plus2%x = a%x+b%x
    end function plus2


end module


program main
use opmod , operator(.add.) => operator(.adda.)
  type(modreal(20,4)) :: a,b,c
  c=a.add.b
end program
