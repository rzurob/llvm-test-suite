!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Alberto Alvarez-Mesquida
!*  DATE                       : 02/20/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Testing:
!*          - Procedure statements with abstract interface
!*          - Procedure pointers statements with abstract interface
!*          - Modules and dummy args. for module procedures.
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

  abstract interface
    function fun(x)
      integer fun, x
    end function fun
  end interface

  procedure (fun) :: one
  procedure (fun), pointer :: pp

contains 

subroutine sub(f,g)

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

end module m

program abstracti028

  use m

  pp => one

  call sub(pp)
  call sub(pp,pp)

  call sub1(pp,one)
  call sub1(one,pp)

end

function one(x)
  integer one, x
  one = 1 + x
end function one
