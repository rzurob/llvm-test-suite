!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: fxiosendeor020.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : fxiosendeor020
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : Aug. 19, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_end and is_iostat_eor intrinsics
!*  SECONDARY FUNCTIONS TESTED : None 
!*
!*  DRIVER STANZA              : xlf90
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                : This tests the functionality of is_iostat_end when used
!*                               to read an internal file. The value of the iostat= spec
!*                               is dependent on iostat_end runtime option, but that
!*                               should not affect the result of the testcases. In this
!*                               case, we set it to 2003std.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      integer :: ios = 0
      character(10) :: internal_file = ""
      character :: tmp

      call setrteopts("iostat_end=2003std")

      do while( .not. is_iostat_end(ios) )
         
         read( internal_file,*,iostat=ios ) tmp
         write(6,*) "ios = ", ios
         
      enddo
      
      end
