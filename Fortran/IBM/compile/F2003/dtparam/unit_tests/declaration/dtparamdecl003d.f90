!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp dtparamdecl003d.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/12/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : type parameters, invalid type parameter
!*                               keyword.  Type parameter values for type
!*                               without type parameters. Missing type
!*                               parameter keywords.
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
program dtparamdecl003d

  type test
     integer :: id1
     integer :: id2
  end type

  type baseproc(basekind, baselength)
     integer, kind :: basekind
     integer, len :: baselength

  end type

  ! The parameter keyword %1$s does not match a type parameter of the
  !    derived type %2$s.
  type (baseproc(basekind=4, baselengt=8)) :: base1
  ! The parameter keyword %1$s does not match a type parameter of the
  !    derived type %2$s.
  type (baseproc(4, aselength=8)) :: base2

  type (test) :: test1 = test(4,4)
  ! component keyword required in structure constructor
  type (test) :: test2 = test(id1=4,4)
  ! component must be explicitly specifed in structure constructor
  type (test) :: test4 = test(id2=4)


  ! Type parameter values must not be specified for a type, %1$s, with no
  !   type parameters.
  type (test(3)) :: test3

  ! Once a type parameter keyword has been specified in a derived-type
  !   specifier, all subsequent values must have a keyword specified.
  type (baseproc(basekind=4, 8)) :: base3

  ! The type parameter %1$s has already been explicitly specified in the
  !  derived-type specifier.
  type (baseproc(basekind=4, baselength=8, basekind=2)) :: base4

  ! Type parameter %1$s must be explicitly specified in this derived-type
  !   specifier.
  type (baseproc(baselength=8)) :: base5


end