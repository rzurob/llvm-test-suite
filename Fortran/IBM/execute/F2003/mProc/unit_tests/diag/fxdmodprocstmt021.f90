!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: fxdmodprocstmt021.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : fxdmodprocstmt021
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : April. 11, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : generalization of module procedure
!*                               stmts, by making the MODULE keyword
!*                               optional. These statements are called
!*                               procedure statements in F2003.
!*                                                   
!*  SECONDARY FUNCTIONS TESTED : None 
!*
!*  DRIVER STANZA              : xlf90
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                : procedure pointers with implicit interfaces
!*                               must be flagged. Make sure this constraint
!*                               is met when proc ptr is the same as the generic
!*                               interface name.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      
      procedure(integer), pointer :: xx 

      interface xx
        procedure xx !< should be flagged bc no explicit iface
      end interface

      procedure(integer) :: yy

      interface yy
        procedure yy !< should be flagged bc no explicit iface
      end interface

      procedure(), pointer :: zz 

      interface zz
        procedure zz !< should be flagged bc no explicit iface
      end interface

      end
