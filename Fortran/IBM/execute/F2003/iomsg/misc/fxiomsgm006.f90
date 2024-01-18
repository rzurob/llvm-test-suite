!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgm006.f
! %VERIFY: 
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
!*  TEST CASE TITLE            : Sequence derived type with IOMSG
!*                                                                     
!*  PROGRAMMER                 : Rayson Liu
!*  DATE                       : Feburary 18, 2004
!*  ORIGIN                     : AIX Compiler Development, 
!*                             : IBM Software Solutions Toronto Lab     
!*                                                                      
!*  PRIMARY FUNCTIONS TESTED   : OPEN
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                : Using one component of a sequence derived type
!*                               as IOSMG specifier to check if the following
!*                               component was overwritten.
!*
!*  TEST CONDITIONS            : 1) Open new seq. formatted file as old.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************


      program fxiomsgm006

      implicit none                     ! All variables must be Declared
 
 
      integer*4 case_id, ios            ! Test Case id under test.
 
      type msg
          sequence
          character*1     cz
          integer*4       iz
          character*50    destroy
          character*10    keep
      end type msg

      type(msg)      errmsg

 
!
! Initialize Return Code routine to SUCCESS...
!
 
      case_id = 0
      call zzrc ( case_id )
 
!
! TestCase 1...
!
 
      case_id = case_id + 1

      errmsg%keep = 'abc'

      open ( 9, status = 'OLD', access = 'SEQUENTIAL', form =  &
  &    'FORMATTED', err = 100, iostat = ios, iomsg = errmsg%destroy )
 
      call zzrc ( case_id )

 
100   if ( errmsg%keep <> 'abc' )  call zzrc ( 100 )

      if ( ios <> 6 ) call zzrc ( case_id )
 
 
! Clean up....
 
      close ( 9, status = 'DELETE' )
 
      end                            ! End of TestCase.
