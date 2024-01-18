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
! %POSTCMD: dcomp dtparamdecl002d.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/12/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : type parameters, kind/len specified for
!*                               a component, duplicated, not integer type
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
program dtparamdecl002d

  type baseproc(basekind)
     integer, kind :: basekind
     ! Type parameter attribute %1$s can not be specified for component %2$s.
     integer, kind :: base2
     ! Type parameter attribute %1$s can not be specified for component %2$s.
     integer, len ::  base3

  end type


  type baseproc2(basekind2, basetype2)
     ! Type parameter %1$s must be of type integer.
     real, kind :: basekind2
     ! Type parameter %1$s must be of type integer.
     logical, kind :: basetype2
  end type

  type baseproc3(basekind3, basetype3)
     ! A %1$s attribute has already been specified for this type parameter.
     !    The %2$s attribute has been ignored.
     integer, kind, len :: basekind3
     ! A %1$s attribute has already been specified for this type parameter.
     !    The %2$s attribute has been ignored.
     integer, len, kind :: basetype3
  end type


  type baseproc4(basekind4, basetype4)
     ! A %1$s attribute has already been specified for this type parameter.
     !   The %2$s attribute has been ignored.
     integer, kind, kind :: basekind4
     ! A %1$s attribute has already been specified for this type parameter.
     !   The %2$s attribute has been ignored.
     integer, len, len :: basetype4
  end type
end