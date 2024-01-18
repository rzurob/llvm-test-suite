!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
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
! %POSTCMD: dcomp dtparamdecl004d.f
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*                                                                     
!*  TEST CASE TITLE            :
!*                                                                     
!*  PROGRAMMER                 : Chris Tandy
!*  DATE                       : 09/12/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : type parameters, make sure type parameter values
!*                                 are scalar integer expressions. Deferred type
!*                                 parameters must have POINTER/ALLOCATABLE.
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
program dtparamdecl004d

  type test
     integer :: id1
     integer :: id2
  end type

  type baseproc(basekind, baselength)
     integer, kind :: basekind
     integer, len :: baselength
     
  end type  
  
  ! not scalar integer expression
  integer(2+2.0):: int1
  ! not scalar integer expression
  character(2.0) :: char1
  
  ! not scalar integer expression
  type (baseproc(2+2.0, 2.0)) :: base1
  
  ! deferred-shape array does not have POINTER/ALLOCATABLE
  integer :: int4(:)
  
  ! deferred type parameter missing POINTER/ALLOCATABLE
  type (baseproc(2, :)) :: base2
  ! deferred type parameter missing POINTER/ALLOCATABLE
  type (baseproc(2, baselength=:)) :: base2a
  
  ! char(*) must be a dummy argument or parameter
  character(*) :: char2

  ! an assumed type parameter must be for a dummy arg
  type (baseproc(2, *)) :: base3
  ! an assumed type parameter must be for a dummy arg
  type (baseproc(2, baselength=*)) :: base3a
  
  type baseproc2(basekind2, baselength2)
     integer, kind :: basekind2
     integer, len :: baselength2
     ! not scalar integer expression
     integer(2+2.0):: int2
     ! not scalar integer expression
     integer(basekind2+2.0) :: int3
     
  end type  
  
  contains
  subroutine new1 (char3, base4)
    character(*) :: char3
    type (baseproc(2, *)) :: base4
    
    
  end subroutine
end  