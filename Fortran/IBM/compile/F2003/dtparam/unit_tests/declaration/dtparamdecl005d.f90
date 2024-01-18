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
! %POSTCMD: dcomp dtparamdecl005d.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/12/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : type parameters, make sure type parameter values
!*                                 dont have deferred and assumed.
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
program dtparamdecl005d

  type test
     integer :: id1
     integer :: id2
  end type

  type baseproc(basekind, baselength, baselength2)
     integer, kind :: basekind
     integer, len :: baselength
     integer, len :: baselength2

  end type

  ! can't have both deferred and assumed type parameters
  type (baseproc(4,*,:)) :: base1

  ! can't have both deferred and assumed type parameters
  type (baseproc(4,:,*)) :: base2

  contains
  subroutine new1 (char3, base4)
    character(*) :: char3
    ! invalid array-spec for array char3
    dimension char3(:,*)
    type (baseproc(2, *, *)) :: base4


  end subroutine
end