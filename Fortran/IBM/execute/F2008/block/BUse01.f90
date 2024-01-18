!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : BUse01
!*
!*  PROGRAMMER                 : dforster
!*  DATE                       : 2010-11-05
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 BLOCK
!*  SECONDARY FUNCTIONS TESTED : USE: definitions available to block
!*  ADAPTED FROM               : -
!*
!*  DESCRIPTION
!*
!*  Define types, variables and procedures in a module, and verify that they
!*  are accessible in the module.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module mod
  implicit none
  type city (l)
    integer, len :: l
    character(l) :: name
  end type city

  integer, save :: ivar = 123
  character(2) :: cvar
  real(4), save :: rvar = 1.23
  complex(8), save :: zvar = (1.23d0,1.23d0)
  logical(1) :: lvar
  type(city(5)) :: paris

contains
  subroutine modsub(a)
    type(city(*)) :: a
    print *, a%name
  end subroutine modsub

  integer function modfun(a,c)
    type(city(*)) :: a
    character(1) :: c
    modfun = index(a%name,c)
  end function modfun

end module mod

program BUse01
  implicit none
  block
    use :: mod
    ivar = 99
    cvar = 'xx'
    rvar = 4.0
    zvar = (rvar, 1/rvar)
    lvar = .true.
    paris = city(5)('paris')
    print *, ivar, cvar, rvar, zvar, lvar, paris
    call modsub(paris)
    print *, modfun(paris,'r')
  end block
  call outer
end program BUse01

subroutine outer
  use :: mod
  implicit none
  print *, ivar, cvar, rvar, zvar, lvar, paris
  call modsub(paris)
  print *, modfun(paris,'r')
end subroutine outer
