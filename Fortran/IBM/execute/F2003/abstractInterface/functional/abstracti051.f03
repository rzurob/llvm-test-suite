!*  ===================================================================
!*
!*  DATE                       : 02/20/2006
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing:
!*          - Procedure statements with abstract interface
!*          - Procedure pointers statements with abstract interface
!*          - Dummy args for external procedures (subroutines and functions)
!*          - OPTIONAL and INTENT attributes for dummy args.
!*          - DT procedure pointer component with pass attr.
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

  type, abstract :: dt
  contains
    procedure (funf), deferred, pass(w) :: tbpf
    procedure (subs), deferred, pass(w) :: tbps
  end type dt

  abstract interface
    integer function funf(n,x,y,z,w)
      import dt
      class(dt) :: w
      integer, intent(in) :: n
      integer, intent(in) :: x(n)
      real, intent(inout), optional ::  y(n)
      integer, intent(out) :: z(n)
    end function funf

    subroutine subs(n,x,y,z,w,ret)
      import dt
      class(dt) :: w
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

module m1

  use m

  type, extends(dt) :: dtt
    procedure (funf), pointer, nopass :: dtf => null()
    procedure (subs), pointer, nopass :: dts => null()
  contains
    procedure, pass(w) :: tbpf => my_tbpf
    procedure, pass(w) :: tbps => my_tbps
  end type dtt

  interface
    integer function my_tbpf(n,x,y,z,w)
      import dtt
      class(dtt) :: w
      integer, intent(in) :: n
      integer, intent(in) :: x(n)
      real, intent(inout), optional ::  y(n)
      integer, intent(out) :: z(n)
    end function my_tbpf

    subroutine my_tbps(n,x,y,z,w,ret)
      import dtt
      class(dtt) :: w
      integer, intent(in) :: n
      integer, intent(in) :: x(*)
      real, intent(inout), optional ::  y(n)
      integer, intent(out) :: z(n)
      integer ret
    end subroutine my_tbps
  end interface

end module m1

program abstracti051

  use m1

  procedure (funf), pointer :: pp
  procedure (subs), pointer :: qq

  type(dtt) :: dta

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

  type(dtt) :: dtx

  integer, parameter :: n = 3
  integer :: a(n) = (/1,1,1/)
  real :: b(n) = (/1.0,2.0,3.0/)
  integer c(n), ii

  print *, "Subroutine sub"
  print *, "pf pointer assoc. =", associated(pf)

  if ( associated(pf) ) then
    call f(n,x=a,z=c,w=dtx,ret=ii)
    dtx%dtf => one
    dtx%dts => two
    i = ii + g(n,a,b,c,dtx) + pf(n,x=a,z=c,y=b,w=dtx)
    print *, "c =", c
    print *, "pf associated, i =", i
  else
    pf => one
  end if

  call f(n,x=a,z=c,w=dtx,ret=ii)

  print *, "i =", ii
  print *, "c =", c

  dtx%dtf => one
  dtx%dts => two

  call f(n,x=a,z=c,w=dtx,ret=ii)

  print *, "i =", ii
  print *, "c =", c

  dtx%dtf => null()
  dtx%dts => null()

  if ( present(g) ) then
    call f(n,x=a,z=c,w=dtx,ret=ii)
    dtx%dtf => one
    dtx%dts => two
    i = ii + g(n,a,b,c,dtx)
    print *, "optional arg. is present, i =", i
  end if

end subroutine sub

integer function fun(f,g)

  procedure (funf), pointer :: f
  procedure (subs), optional, pointer :: g
  procedure (subs), save, pointer :: pf => null()

  type(dtt) :: dtx

  integer, parameter :: n = 3
  integer :: a(n) = (/1,1,1/)
  real :: b(n) = (/1.0,2.0,3.0/)
  integer c(n), ii, jj

  print *, "Function fun"
  print *, "pf pointer assoc. =", associated(pf)

  if ( associated(pf) ) then
    call g(n,a,b,c,dtx,ii)
    dtx%dtf => one
    dtx%dts => two
    call pf(n,x=a,z=c,y=b,w=dtx,ret=jj)
    print *, "c =", c
    i = f(n,x=a,z=c,w=dtx) + ii + jj
    print *, "pf associated, i =", i
  else
    dtx%dtf => one
    dtx%dts => two
    pf => two
  end if

  i = f(n,x=a,z=c,w=dtx)

  print *, "i =", i
  print *, "c =", c

  dtx%dtf => one
  dtx%dts => two

  i = f(n,x=a,z=c,w=dtx)

  print *, "i =", i
  print *, "c =", c

  dtx%dtf => null()
  dtx%dts => null()

  if ( present(g) ) then
    call g(n,a,b,c,dtx,ii)
    dtx%dtf => one
    dtx%dts => two
    i = f(n,x=a,z=c,w=dtx) + ii
    print *, "optional arg. is present, i =", i
  end if

  fun = 0

end function fun

end

integer function my_tbpf(n,x,y,z,w)
  use m1, only : dtt

  class(dtt) :: w
  integer, intent(in) :: n
  integer, intent(in) :: x(n)
  real, optional ::  y(n)
  integer, intent(out) :: z(n)

  print *, "Function my_tbpf"
  print *, "w%dtf pointer assoc. =", associated(w%dtf)
  print *, "w%dts pointer assoc. =", associated(w%dts)

  if ( .not. present(y) ) then
    z = 2*x
    my_tbpf = sum(z)
  else
    z = 2*x + int(y)
    my_tbpf = sum(x) + sum(y)
  end if

end function my_tbpf

subroutine my_tbps(n,x,y,z,w,ret)
  use m1, only : dtt

  class(dtt) :: w
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

end subroutine my_tbps

integer function one(n,x,y,z,w)
  use m1, only : dtt

  class(dtt) :: w
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
  use m1, only : dtt

  class(dtt) :: w
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