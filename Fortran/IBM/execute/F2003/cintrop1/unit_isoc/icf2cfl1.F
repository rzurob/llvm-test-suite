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
! %COMPOPTS:
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: ${TR_SRC}/icf2c.sh l1
! %END
!**********************************************************************
!**********************************************************************
!*  =================================================================== 
!*  XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*                                                                     
!*  TEST CASE NAME             : icf2cfl1.F
!*  TEST CASE TITLE            : Testing Interlanguage Calls
!*                                                                     
!*  PROGRAMMER                 : Rob James
!*  DATE                       : May 29, 2003
!*  ORIGIN                     : XL Fortran Compiler Development
!*                                                                      
!*  DESCRIPTION                : Passing an argument from Fortran to
!*                               a C function.
!*                                                             
!* =================================================================== 
!*
!*  REVISION HISTORY            
!*  
!*  MM/DD/YYYY:  Init:  Comments:
!*  05/29/2003   RJ     -Initial Version
!*                                                                    
!* =================================================================== 
!234567890123456789012345678901234567890123456789012345678901234567890
#define PROGRAM_NAME icf2cfl1
#define ARGTYPE integer(c_int_least8_t)
#define ARGVALUE 42
#define VALUE_ATTR , VALUE
#include "icf2cf.ft"
