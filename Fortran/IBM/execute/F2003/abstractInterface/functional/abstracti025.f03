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

program abstracti025

  abstract interface
    function fun(x)
      integer fun, x
    end function fun
  end interface

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

  pp => one

  call sub(pp)
  call sub(pp,pp)

  call sub1(pp,one)
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
