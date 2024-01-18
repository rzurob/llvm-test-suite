!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qalign=bindc=packed
! %GROUP: bdfunc02af.F
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!**********************************************************************
!*  =================================================================== 
!*  XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*                                                                     
!*  TEST CASE NAME             : bdfunc02af.F
!*  TEST CASE TITLE            : Derived types with the BIND(C) attr.
!*                                                                     
!*  PROGRAMMER                 : Rob James
!*  DATE                       : July 23, 2003
!*  ORIGIN                     : XL Fortran Compiler Development
!*                                                                      
!*  DESCRIPTION                : Functional testing of derived types
!*                               with the BIND(C) attribute.
!*                                                             
!* =================================================================== 
!*
!*  REVISION HISTORY            
!*  
!*  MM/DD/YYYY:  Init:  Comments:
!*  07/23/2003   RJ     -Initial Version
!*                                                                    
!* =================================================================== 
!234567890123456789012345678901234567890123456789012345678901234567890
#define PROGNAME bdfunc02af
#define MODNAME mod_bdfunc02af
#define VALUE_ATTR 
#define VALUE_SPECIFIED 0
#include "bdfunc02.ft"
