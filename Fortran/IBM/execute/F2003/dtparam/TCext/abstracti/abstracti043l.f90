! GM DTP extension using:
! ftcx_dtp -qnok -ql /tstdev/F2003/abstracti/functional/abstracti043.f

!************************************************************************
!* ======================================================================
!*
!*  TEST CASE NAME             : abstracti043l
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
!*  - Dummy args for external procedures (subroutines and functions)
!*  - OPTIONAL and INTENT attributes for dummy args.
!*  - DT procedure pointer component
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

program abstracti043l

  abstract interface
    integer function funf(n,x,y,z)
      integer, intent(in) :: n
      integer, intent(in) :: x(n)
      real, intent(inout), optional ::  y(n)
      integer, intent(out) :: z(n)
    end function funf
    subroutine subs(n,x,y,z,ret)
      integer, intent(in) :: n
      integer, intent(in) :: x(*)
      real, intent(inout), optional ::  y(n)
      integer, intent(out) :: z(n)
      integer ret
    end subroutine subs
  end interface

  interface
    subroutine sub(f,g)
      import funf, subs
      procedure (subs), pointer :: f
      procedure (funf), optional, pointer :: g
    end subroutine sub
  end interface

  interface
    integer function fun(f,g)
      import funf, subs
      procedure (funf), pointer :: f
      procedure (subs), optional, pointer :: g
    end function fun
  end interface

  procedure (funf) :: one
  procedure (subs) :: two
  procedure (funf), pointer :: pp
  procedure (subs), pointer :: qq

  type dt(l1)    ! (20)
      integer, len :: l1
    procedure (funf), pointer, nopass :: dtf
    procedure (subs), pointer, nopass :: dts
  end type dt

  type(dt(20)) :: dta

  dta%dtf => one
  dta%dts => two

  pp => dta%dtf
  qq => dta%dts

  call sub(qq)
  call sub(dta%dts,dta%dtf)

  j = fun(dta%dtf)
  j = fun(pp,dta%dts)

end

subroutine sub(f,g)

  abstract interface
    integer function funf(n,x,y,z)
      integer, intent(in) :: n
      integer, intent(in) :: x(n)
      real, intent(inout), optional ::  y(n)
      integer, intent(out) :: z(n)
    end function funf
    subroutine subs(n,x,y,z,ret)
      integer, intent(in) :: n
      integer, intent(in) :: x(*)
      real, intent(inout), optional ::  y(n)
      integer, intent(out) :: z(n)
      integer ret
    end subroutine subs
  end interface

  procedure (funf) :: one
  procedure (subs), pointer :: f
  procedure (funf), optional, pointer :: g
  procedure (funf), save, pointer :: pf => null()

  integer, parameter :: n = 3
  integer :: a(n) = (/1,1,1/)
  real :: b(n) = (/1.0,2.0,3.0/)
  integer c(n), ii

  print *, "Subroutine sub"
  print *, "pf pointer assoc. =", associated(pf)

  if ( associated(pf) ) then
    call f(n,x=a,z=c,ret=ii)
    i = ii + g(n,a,b,c) + pf(n,x=a,z=c,y=b)
    print *, "c =", c
    print *, "pf associated, i =", i
  else
    pf => one
  end if

  call f(n,x=a,z=c,ret=ii)

  print *, "i =", ii
  print *, "c =", c

  if ( present(g) ) then
    call f(n,x=a,z=c,ret=ii)
    i = ii + g(n,a,b,c)
    print *, "optional arg. is present, i =", i
  end if

end subroutine sub

integer function fun(f,g)

  abstract interface
    integer function funf(n,x,y,z)
      integer, intent(in) :: n
      integer, intent(in) :: x(n)
      real, intent(inout), optional ::  y(n)
      integer, intent(out) :: z(n)
    end function funf
    subroutine subs(n,x,y,z,ret)
      integer, intent(in) :: n
      integer, intent(in) :: x(*)
      real, intent(inout), optional ::  y(n)
      integer, intent(out) :: z(n)
      integer ret
    end subroutine subs
  end interface

  procedure (subs) :: two
  procedure (funf), pointer :: f
  procedure (subs), optional, pointer :: g
  procedure (subs), save, pointer :: pf => null()

  integer, parameter :: n = 3
  integer :: a(n) = (/1,1,1/)
  real :: b(n) = (/1.0,2.0,3.0/)
  integer c(n), ii, jj

  print *, "Function fun"
  print *, "pf pointer assoc. =", associated(pf)

  if ( associated(pf) ) then
    call g(n,a,b,c,ii)
    call pf(n,x=a,z=c,y=b,ret=jj)
    print *, "c =", c
    i = f(n,x=a,z=c) + ii + jj
    print *, "pf associated, i =", i
  else
    pf => two
  end if

  i = f(n,x=a,z=c)

  print *, "i =", i
  print *, "c =", c

  if ( present(g) ) then
    call g(n,a,b,c,ii)
    i = f(n,x=a,z=c) + ii
    print *, "optional arg. is present, i =", i
  end if

  fun = 0

end function fun

integer function one(n,x,y,z)
  integer, intent(in) :: n
  integer, intent(in) :: x(n)
  real, optional ::  y(n)
  integer, intent(out) :: z(n)

  if ( .not. present(y) ) then
    z = 2*x
    one = sum(z)
  else
    z = 2*x + int(y)
    one = sum(x) + sum(y)
  end if

end function one

subroutine two(n,x,y,z,ret)
  integer, intent(in) :: n
  integer, intent(in) :: x(n)
  real, optional ::  y(n)
  integer, intent(out) :: z(n)
  integer ret

  if ( .not. present(y) ) then
    z = 2*x
    ret = sum(z)
  else
    z = 2*x + int(y)
    ret = sum(x) + sum(y)
  end if

end subroutine two
