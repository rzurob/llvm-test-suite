!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgb038.f
! %VERIFY: fort.18:fxiomsgb038.vf
! %STDIN: 
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: 
! %END
!***************************************************************************
 

!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*                                                                     
!*  TEST CASE TITLE            : Format with invalid char. right of period
!*                                                                     
!*  PROGRAMMER                 : Rayson Liu
!*  DATE                       : Feburary 18, 2004
!*  ORIGIN                     : AIX Compiler Development, 
!*                             : IBM Software Solutions Toronto Lab     
!*                                                                      
!*  PRIMARY FUNCTIONS TESTED   : WRITE  FORMAT
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 2
!*
!*  DESCRIPTION                : The D, E, F, G, I, Z, O, and B codes can't have
!*                               characters on the right of the period. This 
!*                               test case demonstrates D and B code with this
!*                               kind of illegal using.
!*
!*
!*  TEST CONDITIONS            : 1) Invalid specifier right of period for D fmt
!*                               2) Invalid specifier right of period for B fmt
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgb038
 
      implicit none                     ! All variables must be Declared
 
 
      integer*4 case_id, ios            ! Test Case id under test.
 
      real*4 varreal
 
      character*20 form

      character*300 errmsg
 
!
! Initialize Return Code routine to SUCCESS...
!
 
      case_id = 0
      call zzrc ( case_id )
 
 
!
! TestCase 1...
!
 
      case_id = case_id + 1
 
      form = '( D5.. )'
 
      write(0, fmt = form, iostat = ios, iomsg = errmsg ) varreal

      write(18,*) errmsg
 
 
!
! TestCase 2...
!
 
      case_id = case_id + 1
 
      form = '( B5.[ )'
 
      write(0, fmt = form, iostat = ios, iomsg = errmsg ) varreal

      write(18,*) errmsg
 
      end               ! End of TestCase.
