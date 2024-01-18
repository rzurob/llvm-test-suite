!************************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qlanglvl=2003pure
! %GROUP: SelRKindDiag1.f 
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!************************************************************************
      program SelRKindDiag1
        print *, selected_real_kind(3, 3, 3) 
      end                 

!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Ren Jian Gang
!*  DATE                       : Nov 06, 2010
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM China Development Shanghai Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : 
!     A message is emitted for langlvl error with 3 arguments specified
!     of selected_real_kind intrinsic
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :  -qlanglvl=2003pure
!*
!*  DESCRIPTION                : 
!*
!*
!*
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890
