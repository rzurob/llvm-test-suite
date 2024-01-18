! ************************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/intrimod42.presh intrimod42
! %COMPOPTS: -qhalt=w
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD:
! %END
!************************************************************************
!************************************************************************
!*
!*  FORTRAN TEST CASE            IBM INTERNAL USE ONLY
!*  Test Case Title  : INTRINSIC/NON_INTRINSIC module nature
!*  Test Case Name   : intrimod42a.f
!*  Created By       : Bahram Chehrazy
!*  DATE             : January, 2004
!*  Description      : Testing backward compatibility with XLF8.1.
!*                     Calling module procedures where the modules are
!*                     pre-compiled with 8.1 and the mainline is compiled
!*                     with Post 8.1.  The Option "-qmodule=mangle81" is
!*                     NOT specified.  No errors should be reported.
!*
!*************************************************************************
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  01/23/04   BC     Initial Version
!*  04/04/08   GM     Defect 299017 - Adjusted Description
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
!

