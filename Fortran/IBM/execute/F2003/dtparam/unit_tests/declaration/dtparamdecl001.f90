!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: dtparamdecl001.f
! %VERIFY: dtparamdecl001.out:dtparamdecl001.vf
! %STDIN:
! %STDOUT: dtparamdecl001.out
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
!*                                7) no modules
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

program dtparamdecl001
  type, extends(old) :: new
  end type

  type old
     integer :: b
  end type

  type base(basekind, baselen)
     integer, kind :: basekind
     integer, len :: baselen
     integer(basekind) :: basearray(baselen)

  end type

  type(base(4,4)) :: base1

   type(base(basekind=4,baselen=6)) :: base2

   base1%basearray = 3
   base1%basearray(2) = 6

   base2%basearray = 5
   base2%basearray(2) = 9

   print *, ' base1=', base1
   print *, ' base2=', base2

end

