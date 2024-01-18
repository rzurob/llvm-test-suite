!***************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f fort.*
! %COMPOPTS:  -qfree=f90
! %GROUP: fxiomsgl028.f
! %VERIFY: fort.18:fxiomsgl028.vf
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
!*  TEST CASE TITLE            : OPEN with DIRECT access while RECL is missing
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
!*  DESCRIPTION                : Open a file with 'DIRECT' access code, but the
!*                               RECL code is missing.The OPEN statement was 
!*                               tested in a function which returns a string.
!*
!*  TEST CONDITIONS            : 1) Open a file with 'DIRECT' access code but
!*                                  RECL is absent.
!*                               
!*                              
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************


      program fxiomsgl028

      implicit none

      integer*4       ios
      integer*4       case_id
      character*300   errmsg
      character*11     access_code

      case_id = 0
      call zzrc( case_id )

!
! TestCase 1..
!
      case_id = case_id + 1

      access_code = 'DIRECT'

      errmsg = print_msg( 'DIRECT', 'FORMATTED' )

      write ( 18, * ) errmsg

100   close (8, status = 'DELETE')



      contains

      character*300 function print_msg ( access_var, form_var )
        character(LEN=*), intent(IN):: access_var
        character(LEN=*), intent(IN):: form_var
        character*300 errmsg_var

        open ( 8, FILE = 'file1', access = access_var, form = form_var, &
      & err = 10, iostat = ios, iomsg = errmsg_var )


        call zzrc( case_id )
      
10      print_msg = errmsg_var

        if (ios .ne.25) error stop 1000

      end function print_msg


      end

