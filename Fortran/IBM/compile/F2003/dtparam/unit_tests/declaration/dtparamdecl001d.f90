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
! %POSTCMD: dcomp dtparamdecl001d.f
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
!*  DESCRIPTION                : type parameters, type parameter name can      
!*                               not conflict with component/binding
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
program dtparamdecl001d

  type baseproc
     integer  :: basekind
     contains
       !  expect a message indicating identifier basekind is already a binding/component/type parameter
       !   of the same derived type
       procedure basekind
  end type
  
  type baseproc2
     integer  :: basekind
     !  expect a message indicating identifier basekind is already a component/type parameter
     !   of the same derived type
     integer  :: basekind
  end type
  
  type base(basekind, baselen)
     integer, kind :: basekind
     integer, len :: baselen
     contains
       !  expect a message indicating identifier basekind is already a binding/component/type parameter
       !   of the same derived type
       procedure basekind
  end type
  
  type base2(basekind, baselen)
     integer, kind :: basekind
     integer, len :: baselen
     !  expect a message indicating identifier basekind is already a component/type parameter
     !   of the same derived type
     integer  :: basekind
  end type
  
  type base3(basekind, basekind)
     integer, kind :: basekind
  end type   
  
end  
  
  