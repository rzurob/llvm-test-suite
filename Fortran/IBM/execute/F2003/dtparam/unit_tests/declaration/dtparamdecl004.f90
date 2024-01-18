!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: dtparamdecl004.f
! %VERIFY: dtparamdecl004.out:dtparamdecl004.vf
! %STDIN:
! %STDOUT: dtparamdecl004.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/20/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : TYPE parameters,
!*                                1) kind and len attribute
!*                                2) type parameters used in:
!*                                     i) array high bound
!*                                     ii) integer length
!*                                3) no default values for type param
!*                                4) no inheritance
!*                                5) type parameter values are constants
!*                                6) type parameter values have type parameter keywords
!*                                7) type definition in module
!*                                9) components in derived template have
!*                                     i) allocatable
!*                                     ii) pointer
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

module dtparamdecl004m
  type, extends(old) :: new
  end type

  type old
     integer :: b
  end type

  type base(basekind, baselen)
     integer, kind :: basekind
     integer, len :: baselen
     integer(basekind) :: basearray(baselen)
     real, allocatable :: real1
     logical, pointer :: logptr1

  end type
end module

program dtparamdecl004
  use dtparamdecl004m

  type(base(4,4)) :: base1

  type(base(basekind=4,baselen=6)) :: base2

  base1%basearray = 3
  base1%basearray(2) = 6

  base2%basearray = 5
  base2%basearray(2) = 9

  print *, ' base1%basearray=', base1%basearray
  print *, ' base2%basearray=', base2%basearray

  if (associated(base1%logptr1)) then
    print *, '** error base1%logptr1) associated'
  else
    print *, 'base1%logptr1 not associated'
  end if

  if (allocated(base1%real1)) then
    print *, '** error base1%real1) allocated'
  else
    print *, 'base1%real1 not allocated'
  end if

  allocate(base1%real1)
  base1%real1 = 20.34
  print *, 'base1%real1=', base1%real1


end

