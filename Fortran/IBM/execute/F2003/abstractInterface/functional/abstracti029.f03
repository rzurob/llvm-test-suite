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
!*          - Dummy args for external subroutines
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

program abstracti029

  abstract interface
    integer function funf(n,x,y,z)
      integer, intent(in) :: n
      integer, intent(in) :: x(n)
      real, intent(inout), optional ::  y(n)
      integer, intent(out) :: z(n)
    end function funf
  end interface

  interface
    subroutine sub(f,g)
      import funf
      procedure (funf), pointer :: f
      procedure (funf), optional, pointer :: g
    end subroutine sub
  end interface

  interface
    subroutine sub1(f,g)
      import funf
      procedure (funf) :: f
      procedure (funf), optional :: g
    end subroutine sub1
  end interface

  procedure (funf) :: one
  procedure (funf), pointer :: pp

  pp => one

  call sub(pp)
  call sub(pp,pp)

  call sub1(pp,one)
  call sub1(one,pp)

end

subroutine sub(f,g)

  abstract interface
    integer function funf(n,x,y,z)
      integer, intent(in) :: n
      integer, intent(in) :: x(n)
      real, intent(inout), optional ::  y(n)
      integer, intent(out) :: z(n)
    end function funf
  end interface

  procedure (funf) :: one
  procedure (funf), pointer :: f
  procedure (funf), optional, pointer :: g
  procedure (funf), save, pointer :: pf => null()

  integer, parameter :: n = 3
  integer :: a(n) = (/1,1,1/)
  real :: b(n) = (/1.0,2.0,3.0/)
  integer c(n)

  print *, "Subroutine sub"
  print *, "pf pointer assoc. =", associated(pf)

  if ( associated(pf) ) then
    i = f(n,x=a,z=c) + g(n,a,b,c) + pf(n,x=a,z=c,y=b)
    print *, "c =", c
    print *, "pf associated, i =", i
  else
    pf => one
  end if

  i = f(n,x=a,z=c)

  print *, "i =", i
  print *, "c =", c

  if ( present(g) ) then
    i = f(n,x=a,z=c) + g(n,a,b,c)
    print *, "optional arg. is present, i =", i
  end if

end subroutine sub

subroutine sub1(f,g)

  abstract interface
    integer function funf(n,x,y,z)
      integer, intent(in) :: n
      integer, intent(in) :: x(n)
      real, intent(inout), optional ::  y(n)
      integer, intent(out) :: z(n)
    end function funf
  end interface

  procedure (funf) :: one
  procedure (funf) :: f
  procedure (funf), optional :: g
  procedure (funf), save, pointer :: pf => null()

  integer, parameter :: n = 3
  integer :: a(n) = (/1,1,1/)
  real :: b(n) = (/1.0,2.0,3.0/)
  integer c(n)

  print *, "Subroutine sub"
  print *, "pf pointer assoc. =", associated(pf)

  if ( associated(pf) ) then
    i = f(n,x=a,z=c) + g(n,a,b,c) + pf(n,x=a,z=c,y=b)
    print *, "c =", c
    print *, "pf associated, i =", i
  else
    pf => one
  end if

  i = f(n,x=a,z=c)

  print *, "i =", i
  print *, "c =", c

  if ( present(g) ) then
    i = f(n,x=a,z=c) + g(n,a,b,c)
    print *, "optional arg. is present, i =", i
  end if

end subroutine sub1

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

