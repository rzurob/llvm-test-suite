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
! %POSTCMD: dcomp bddiag14.f
! %END
!**********************************************************************
!**********************************************************************
!*  =================================================================== 
!*  XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*                                                                     
!*  TEST CASE NAME             : bddiag14.f
!*  TEST CASE TITLE            : Derived types with the BIND(C) attr.
!*                                                                     
!*  PROGRAMMER                 : Rob James
!*  DATE                       : July 23, 2003
!*  ORIGIN                     : XL Fortran Compiler Development
!*                                                                      
!*  DESCRIPTION                : Testing sequence statement inside of
!*                               BIND(C) derived types.
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
      program bddiag14
        type, bind(c) :: dt
          sequence
          character :: a
        end type
      end
