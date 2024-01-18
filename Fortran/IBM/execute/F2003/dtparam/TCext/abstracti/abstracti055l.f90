! GM DTP extension using:
! ftcx_dtp -qnok -ql /tstdev/F2003/abstracti/functional/abstracti055.f

!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : abstracti055l
!*
!*                               by Alberto Alvarez-Mesquida)
!*  DATE                       : 2007-11-01 (original: 02/20/2006)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Abstract Interface
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  DESCRIPTION                : Testing:
!*  - Procedure statements with abstract interface
!*  - Procedure pointers statements with abstract interface
!*  - Dummy args for external procedures (subroutines and functions)
!*  - OPTIONAL and INTENT attributes for dummy args.
!*  - DT procedure pointer component with pass attr.
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*
!* =====================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*
!* =====================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

  type, abstract :: dt(l1)    ! (20)
      integer, len :: l1
  contains
    procedure (suba), deferred, nopass :: tbps1
    procedure (subb), deferred, nopass :: tbps2
    generic :: gen => tbps1, tbps2
  end type dt

  abstract interface
    subroutine suba(n,x,y,z,ret)
      integer, intent(in) :: n
      integer(4), intent(in) :: x(*)
      real(4), intent(inout), optional ::  y(n)
      integer(4), intent(out) :: z(n)
      integer(4) ret
    end subroutine suba
    subroutine subb(n,x,y,z,ret)
      integer, intent(in) :: n
      integer(8), intent(in) :: x(*)
      real(8), intent(inout), optional ::  y(n)
      integer(8), intent(out) :: z(n)
      integer(8) ret
    end subroutine subb
  end interface

  procedure (suba) :: one
  procedure (subb) :: two

end module m

module m1

  use m

  type, extends(dt) :: dtt    ! (20)
    procedure (suba), pointer, nopass :: dts1
    procedure (subb), pointer, nopass :: dts2
  contains
    procedure, nopass :: tbps1 => my_tbps1
    procedure, nopass :: tbps2 => my_tbps2
  end type dtt

contains

subroutine my_tbps1(n,x,y,z,ret)

  integer, intent(in) :: n
  integer(4), intent(in) :: x(n)
  real(4), intent(inout), optional ::  y(n)
  integer(4), intent(out) :: z(n)
  integer(4) ret

  print *, "Subroutine my_tbps1"

  if ( .not. present(y) ) then
    z = 2*x
    ret = sum(z)
  else
    z = 2*x + int(y)
    ret = sum(x) + sum(y)
  end if

end subroutine my_tbps1

subroutine my_tbps2(n,x,y,z,ret)

  integer, intent(in) :: n
  integer(8), intent(in) :: x(n)
  real(8), intent(inout), optional ::  y(n)
  integer(8), intent(out) :: z(n)
  integer(8) ret

  print *, "Subroutine my_tbps2"

  if ( .not. present(y) ) then
    z = 2*x
    ret = sum(z)
  else
    z = 2*x + int(y)
    ret = sum(x) + sum(y)
  end if

end subroutine my_tbps2

end module m1

program abstracti055l

  use m1

  procedure (suba), pointer :: pp
  procedure (subb), pointer :: qq

  type(dtt(20)) :: dta

  dta%dts1 => one
  dta%dts2 => two

  pp => dta%dts1
  qq => dta%dts2

  call sub(qq)
  call sub(dta%dts2,dta%dts1)

contains

subroutine sub(f,g)

  procedure (subb), pointer :: f
  procedure (suba), optional, pointer :: g
  procedure (suba), save, pointer :: pf => null()

  integer, parameter :: n = 3
  integer :: a4(n) = (/1,1,1/)
  real :: b4(n) = (/1.0,2.0,3.0/)
  integer c4(n), ii, kk

  integer(8) :: a8(n) = (/2,2,2/)
  real(8) :: b8(n) = (/4.0d0,5.0d0,6.0d0/)
  integer(8) c8(n), jj

  type(dtt(20)) :: dtx

  print *, "Subroutine sub"
  print *, "pf pointer assoc. =", associated(pf)

  if ( associated(pf) ) then
    call f(n,x=a8,z=c8,ret=jj)
    call g(n,a4,b4,c4,ii)
    call pf(n,x=a4,z=c4,y=b4,ret=kk)
    i = ii + int(jj,4) + kk
    print *, "c4 =", c4
    print *, "pf associated, i =", i
    call dtx%gen(n,x=a4,z=c4,ret=kk)
    print *, "kk =", kk
    print *, "c4 =", c4
  else
    pf => one
  end if

  call f(n,x=a8,z=c8,ret=jj)
  print *, "jj =", jj
  print *, "c8 =", c8

  call dtx%gen(n,x=a8,y=b8,z=c8,ret=jj)
  print *, "jj =", jj
  print *, "c8 =", c8

  if ( present(g) ) then
    call f(n,x=a8,z=c8,ret=jj)
    call g(n,a4,b4,c4,ii)
    print *, "ii =", ii
    print *, "c4 =", c4
    i = ii + int(jj,4)
    print *, "optional arg. is present, i =", i
    call dtx%gen(n,x=a4,y=b4,z=c4,ret=kk)
    print *, "kk =", kk
    print *, "c4 =", c4
  end if

end subroutine sub

end

subroutine one(n,x,y,z,ret)

  integer, intent(in) :: n
  integer(4), intent(in) :: x(n)
  real(4), optional ::  y(n)
  integer(4), intent(out) :: z(n)
  integer(4) ret

  print *, "Subroutine one"

  if ( .not. present(y) ) then
    z = 2*x
    ret = sum(z)
  else
    z = 2*x + int(y)
    ret = sum(x) + sum(y)
  end if

end subroutine one

subroutine two(n,x,y,z,ret)

  integer, intent(in) :: n
  integer(8), intent(in) :: x(n)
  real(8), optional ::  y(n)
  integer(8), intent(out) :: z(n)
  integer(8) ret

  print *, "Subroutine two"

  if ( .not. present(y) ) then
    z = 2*x
    ret = sum(z)
  else
    z = 2*x + int(y)
    ret = sum(x) + sum(y)
  end if

end subroutine two
