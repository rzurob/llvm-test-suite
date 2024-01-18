!***********************************************************************
!* =====================================================================
!*
!*                               by Alberto Alvarez-Mesquida)
!*  DATE                       : 2007-10-30 (original: 02/20/2006)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Abstract Interface
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  DESCRIPTION                : Testing:
!*  - Procedure statements with abstract interface
!*  - Procedure pointers statements with abstract interface
!*  - Dummy args for external subroutines
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

module m

  abstract interface
    function fun(x)
      integer fun, x
    end function fun
  end interface

  type dt (kdt_1,ldt_1) ! kdt_1,ldt_1=4,13
    integer, kind :: kdt_1
    integer, len  :: ldt_1
    procedure (fun), pointer, nopass :: dtp
  end type dt

end module m

program abstracti037kl

  use m

  interface
    subroutine sub(f,g)
      import fun
      procedure (fun), pointer :: f
      procedure (fun), optional, pointer :: g
    end subroutine sub
  end interface

  interface
    subroutine sub1(f,g)
      import fun
      procedure (fun) :: f
      procedure (fun), optional :: g
    end subroutine sub1
  end interface

  procedure (fun) :: one
  procedure (fun), pointer :: pp

  type(dt(4,13)) :: dta ! tcx: (4,13)

  dta%dtp => one
  pp => dta%dtp

  call sub(dta%dtp)
  call sub(pp,dta%dtp)

  call sub1(dta%dtp,one)
  call sub1(one,pp)

end

subroutine sub(f,g)

  abstract interface
    function fun(x)
      integer fun, x
    end function fun
  end interface

  procedure (fun) :: one
  procedure (fun), pointer :: f
  procedure (fun), optional, pointer :: g
  procedure (fun), save, pointer :: pf => null()

  print *, "Subroutine sub"
  print *, "pf pointer assoc. =", associated(pf)

  if ( associated(pf) ) then
    i = f(2) + g(2) + pf(2)
    print *, "pf associated, i =", i
  else
    pf => one
  end if

  i = f(2)
  print *, "i =", i
  if ( present(g) ) then
    i = f(2) + g(2)
    print *, "optional arg. is present, i =", i
  end if

end subroutine sub

subroutine sub1(f,g)

  abstract interface
    function fun(x)
      integer fun, x
    end function fun
  end interface

  procedure (fun) :: one
  procedure (fun) :: f
  procedure (fun), optional :: g
  procedure (fun), save, pointer :: pf => null()

  print *, "Subroutine sub1"
  print *, "pf pointer assoc. =", associated(pf)

  if ( associated(pf) ) then
    i = f(2) + g(2) + pf(2)
    print *, "pf associated, i =", i
  else
    pf => one
  end if

  i = f(2)
  print *, "i =", i
  if ( present(g) ) then
    i = f(2) + g(2)
    print *, "optional arg. is present, i =", i
  end if

end subroutine sub1

function one(x)
  integer one, x
  one = 1 + x
end function one

! Extensions to introduce derived type parameters:
! type: dt - added parameters (kdt_1,ldt_1) to invoke with (4,13) / declare with (4,*) - 1 changes
