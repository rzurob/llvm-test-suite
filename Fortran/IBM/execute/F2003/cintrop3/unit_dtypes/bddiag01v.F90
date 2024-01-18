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
! %POSTCMD: dcomp bddiag01v.F
! %END
!**********************************************************************
!**********************************************************************
!*  =================================================================== 
!*  XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*                                                                     
!*  TEST CASE NAME             : bddiag01v.F
!*  TEST CASE TITLE            : Derived types with the BIND(C) attr.
!*                                                                     
!*  PROGRAMMER                 : Rob James
!*  DATE                       : July 11, 2003
!*  ORIGIN                     : XL Fortran Compiler Development
!*                                                                      
!*  DESCRIPTION                : Testing the types of components of 
!*                               BIND(C) derived types.
!*                                                             
!* =================================================================== 
!*
!*  REVISION HISTORY            
!*  
!*  MM/DD/YYYY:  Init:  Comments:
!*  07/11/2003   RJ     -Initial Version
!*                                                                    
!* =================================================================== 
!234567890123456789012345678901234567890123456789012345678901234567890
#define PROGNAME bddiag01v
#define COMPTYPE double precision
#define USESTATEMENT 
#define DT_DECLARATION type, bind(c) :: bcdt; integer(4) a; end type
#include "bddiag01.ft"
