!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgl022.f
! %VERIFY: fort.18:fxiomsgl022.vf
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
!*  TEST CASE TITLE            : Invalid unit numbers & file types -w/ BACKSPACE
!*                                                                     
!*  PROGRAMMER                 : Rayson Liu
!*  DATE                       : Feburary 18, 2004
!*  ORIGIN                     : AIX Compiler Development, 
!*                             : IBM Software Solutions Toronto Lab     
!*                                                                      
!*  PRIMARY FUNCTIONS TESTED   : BACKSPACE
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 3
!*
!*  DESCRIPTION                : BACKSPACE is only for external file connected
!*                               for sequential access, and the UNIT number has
!*                               to be in the range 1 through 2147483647. The
!*                               BACKSPACE statement was tested in a subroutine
!*                               which passes in a string as iomsg specifier.
!*
!*  TEST CONDITIONS            : 1) Backspace  with unit number -9.
!*                               2) Backspace  with direct unformatted file
!*                               3) Backspace  with direct formatted file
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgl022
 
      implicit none       
 
      integer*4 case_id    
 
      integer*4 ios, unit_number

      character*300 errmsg

!
!  Initialize Return Code routine to 0
!
      case_id = 0
      call zzrc (case_id )
 
!
! TestCase 1...
!
 
      case_id = case_id + 1

      unit_number = -9
 
      call print_msg( unit_number, errmsg )
 
 
!
! TestCase 2...
!
 
      case_id = case_id + 1

      open ( 8, access = 'DIRECT', recl = 80 )
 
      call print_msg( 8, errmsg )
 
 
!
! TestCase 3...
!

      case_id = case_id + 1
 
      open ( 9, form = 'FORMATTED', access = 'DIRECT', recl = 80 )
 

      call print_msg( 9, errmsg )
 
 
! Clean up...
 
      close ( 8, status = 'DELETE' )
 
      close ( 9, status = 'DELETE' )


   contains

      subroutine print_msg( unit_id,  msg_var )
         integer, intent(IN)::  unit_id
         character(LEN=*), intent(OUT)::  msg_var

         backspace ( unit = unit_id, iostat = ios, err = 30, iomsg=msg_var )

         call zzrc ( case_id )
30       write(18,*) msg_var
         if ( ios <=0 ) call zzrc ( case_id )

      end subroutine

 
      end            ! End of TestCase.

