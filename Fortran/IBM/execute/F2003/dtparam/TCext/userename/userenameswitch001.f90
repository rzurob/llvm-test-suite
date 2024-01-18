! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/userename/userenameswitch001.f
! opt variations: -qnol

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: userenameswitch001.f
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
!*  DESCRIPTION                : ensure that ops from different modules are parsed correctly

!*
!234567890123456789012345678901234567890123456789012345678901234567890
module opmod
  type modreal2(n1,k1)    ! (20,4)
    integer, kind :: k1
    integer, len  :: n1
    real(k1)      :: x=8.0

  end type
  type modreal(n2,k2)    ! (20,4)
    integer, kind :: k2
    integer, len  :: n2
    real(k2)      :: x=4.0

    contains
      procedure :: plus

      generic :: operator(.add.) => plus

  end type

  contains
    function plus(a,b)
      type(modreal(20,4)) :: plus
      class(modreal(*,4)), intent(in) :: a
      type(modreal2(*,4)), intent(in) ::b
      plus%x = b%x+a%x
      print *, "modereal2 class second"
    end function plus

end module

module opmod2
 use opmod

  interface operator(.plus.)
    module procedure plus2
  end interface
  contains
   function plus2(a,b)
      type(modreal2(20,4)) :: plus2
      class(modreal2(*,4)), intent(in) :: a
      class(modreal(*,4)), intent(in) :: b
      plus2%x = a%x+b%x
      print *, "modreal2 first"
    end function plus2
end module

program main
use opmod
use opmod2, operator(.add.) => operator(.plus.)
  type(modreal(20,4)) :: a,z
  type(modreal2(20,4)) :: b,y

  z=a.add.b
  print *, z%x
  y=b.add.a
  print *, y%x
  z=a.add.b.add.y
  print *, z%x





end program
