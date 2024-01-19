! GM DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/F2003/abstracti/functional/abstracti048.f

!***********************************************************************
!* =====================================================================
!*
!*                               by Alberto Alvarez-Mesquida)
!*  DATE                       : 2007-10-31 (original: 02/20/2006)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Abstract Interface
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  DESCRIPTION                : Testing:
!*  - Procedure statements with abstract interface
!*  - Procedure pointers statements with abstract interface
!*  - Dummy args for internal procedures (subroutines and functions)
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

  type dt(k1)    ! (4)
      integer, kind :: k1
    procedure (funf), pointer, pass(w) :: dtf => null()
    procedure (subs), pointer, pass(w) :: dts => null()
  end type dt

  abstract interface
    integer function funf(n,x,y,z,w)
      import dt
      class(dt(4)) :: w
      integer, intent(in) :: n
      integer, intent(in) :: x(n)
      real, intent(inout), optional ::  y(n)
      integer, intent(out) :: z(n)
    end function funf
    subroutine subs(n,x,y,z,w,ret)
      import dt
      class(dt(4)) :: w
      integer, intent(in) :: n
      integer, intent(in) :: x(*)
      real, intent(inout), optional ::  y(n)
      integer, intent(out) :: z(n)
      integer ret
    end subroutine subs
  end interface

  procedure (funf) :: one
  procedure (subs) :: two

end module m

program abstracti048k

  use m

  procedure (funf), pointer :: pp
  procedure (subs), pointer :: qq

  type(dt(4)) :: dta

  dta%dtf => one
  dta%dts => two

  pp => dta%dtf
  qq => dta%dts

  call sub(qq)
  call sub(dta%dts,dta%dtf)

  j = fun(dta%dtf)
  j = fun(pp,dta%dts)

contains

subroutine sub(f,g)

  procedure (subs), pointer :: f
  procedure (funf), optional, pointer :: g
  procedure (funf), save, pointer :: pf => null()

  type(dt(4)) :: dtt

  integer, parameter :: n = 3
  integer :: a(n) = (/1,1,1/)
  real :: b(n) = (/1.0,2.0,3.0/)
  integer c(n), ii

  print *, "Subroutine sub"
  print *, "pf pointer assoc. =", associated(pf)

  if ( associated(pf) ) then
    call f(n,x=a,z=c,w=dtt,ret=ii)
    dtt%dtf => one
    dtt%dts => two
    i = ii + g(n,a,b,c,dtt) + pf(n,x=a,z=c,y=b,w=dtt)
    print *, "c =", c
    print *, "pf associated, i =", i
  else
    pf => one
  end if

  call f(n,x=a,z=c,w=dtt,ret=ii)

  print *, "i =", ii
  print *, "c =", c

  dtt%dtf => one
  dtt%dts => two

  call f(n,x=a,z=c,w=dtt,ret=ii)

  print *, "i =", ii
  print *, "c =", c

  dtt%dtf => null()
  dtt%dts => null()

  if ( present(g) ) then
    call f(n,x=a,z=c,w=dtt,ret=ii)
    dtt%dtf => one
    dtt%dts => two
    i = ii + g(n,a,b,c,dtt)
    print *, "optional arg. is present, i =", i
  end if

end subroutine sub

integer function fun(f,g)

  procedure (funf), pointer :: f
  procedure (subs), optional, pointer :: g
  procedure (subs), save, pointer :: pf => null()

  type(dt(4)) :: dtt

  integer, parameter :: n = 3
  integer :: a(n) = (/1,1,1/)
  real :: b(n) = (/1.0,2.0,3.0/)
  integer c(n), ii, jj

  print *, "Function fun"
  print *, "pf pointer assoc. =", associated(pf)

  if ( associated(pf) ) then
    call g(n,a,b,c,dtt,ii)
    dtt%dtf => one
    dtt%dts => two
    call pf(n,x=a,z=c,y=b,w=dtt,ret=jj)
    print *, "c =", c
    i = f(n,x=a,z=c,w=dtt) + ii + jj
    print *, "pf associated, i =", i
  else
    pf => two
    dtt%dtf => one
    dtt%dts => two
  end if

  i = f(n,x=a,z=c,w=dtt)

  print *, "i =", i
  print *, "c =", c

  dtt%dtf => one
  dtt%dts => two

  i = f(n,x=a,z=c,w=dtt)

  print *, "i =", i
  print *, "c =", c

  dtt%dtf => null()
  dtt%dts => null()

  if ( present(g) ) then
    call g(n,a,b,c,dtt,ii)
    dtt%dtf => one
    dtt%dts => two
    i = f(n,x=a,z=c,w=dtt) + ii
    print *, "optional arg. is present, i =", i
  end if

  fun = 0

end function fun

end

integer function one(n,x,y,z,w)
  use m, only : dt

  class(dt(4)) :: w
  integer, intent(in) :: n
  integer, intent(in) :: x(n)
  real, optional ::  y(n)
  integer, intent(out) :: z(n)

  print *, "Function one"
  print *, "w%dtf pointer assoc. =", associated(w%dtf)
  print *, "w%dts pointer assoc. =", associated(w%dts)

  if ( .not. present(y) ) then
    z = 2*x
    one = sum(z)
  else
    z = 2*x + int(y)
    one = sum(x) + sum(y)
  end if

end function one

subroutine two(n,x,y,z,w,ret)
  use m, only : dt

  class(dt(4)) :: w
  integer, intent(in) :: n
  integer, intent(in) :: x(n)
  real, optional ::  y(n)
  integer, intent(out) :: z(n)
  integer ret

  print *, "Subroutine two"
  print *, "w%dtf pointer assoc. =", associated(w%dtf)
  print *, "w%dts pointer assoc. =", associated(w%dts)

  if ( .not. present(y) ) then
    z = 2*x
    ret = sum(z)
  else
    z = 2*x + int(y)
    ret = sum(x) + sum(y)
  end if

end subroutine two
