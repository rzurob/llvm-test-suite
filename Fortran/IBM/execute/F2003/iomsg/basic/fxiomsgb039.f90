!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgb039.f
! %VERIFY: fort.18:fxiomsgb039.vf
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
!*  TEST CASE TITLE            : Extra commas in the format specifier
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
!*  DESCRIPTION                : Different combinations of invalid commas were
!*                               put in format statements. 
!*
!*
!*  TEST CONDITIONS            : 1) Invalid comma between parenthesis.
!*                               2) Invalid commas after I format code.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgb039
 
      implicit none                     ! All variables must be Declared
 
 
      integer*4 case_id, ios            ! Test Case id under test.
 
      integer*4 varint
 
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
 
      form = '( 50 (,I5,) )'
 
      write(0, fmt = form, iostat = ios, iomsg = errmsg ) varint

      write(18,*) errmsg
 
 
 
!
! TestCase 2...
!
 
      case_id = case_id + 1
 
      form = '( 50 ( I5,, ) )'
 
      write(0, fmt = form, iostat = ios, iomsg = errmsg ) varint

      write(18,*) errmsg

      end                            ! End of TestCase.
