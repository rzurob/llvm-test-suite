!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: fxiosendeor025.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug. 26, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_end and is_iostat_eor intrinsics
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This tests the use of dummy argument name in the
!*                               intrinsic.
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      integer :: arg

   ! TESTING IS_IOSTAT_END
      write(*,*) is_iostat_end(i=-1)
      write(*,*) is_iostat_end(i=-2)
      write(*,*) is_iostat_end(i=-3)
      write(*,*) is_iostat_end(i=-4)
      write(*,*) is_iostat_end(i=1)
      write(*,*) is_iostat_end(i=0)
      write(*,*) is_iostat_end(i=-1*5+8)
      write(*,*) is_iostat_end(i=-1*5+3)

      write(*,*)

      arg = -1
      write(*,*) is_iostat_end(i=arg)
      arg = -2
      write(*,*) is_iostat_end(i=arg)
      arg = -3
      write(*,*) is_iostat_end(i=arg)
      arg = -4
      write(*,*) is_iostat_end(i=arg)
      arg = 1
      write(*,*) is_iostat_end(i=arg)
      arg = 0
      write(*,*) is_iostat_end(i=arg)
      arg = 1
      write(*,*) is_iostat_end(i=arg*arg+2)
      write(*,*) is_iostat_end(i=-arg)

      write(*,*) "\n-------------\n"

   ! TESTING IS_IOSTAT_EOR
      write(*,*) is_iostat_eor(i=-1)
      write(*,*) is_iostat_eor(i=-2)
      write(*,*) is_iostat_eor(i=-3)
      write(*,*) is_iostat_eor(i=-4)
      write(*,*) is_iostat_eor(i=1)
      write(*,*) is_iostat_eor(i=0)
      write(*,*) is_iostat_eor(i=-1*5+3)
      write(*,*) is_iostat_eor(i=-1*5+1)

      write(*,*)

      arg = -1
      write(*,*) is_iostat_eor(i=arg)
      arg = -2
      write(*,*) is_iostat_eor(i=arg)
      arg = -3
      write(*,*) is_iostat_eor(i=arg)
      arg = -4
      write(*,*) is_iostat_eor(i=arg)
      arg = 1
      write(*,*) is_iostat_eor(i=arg)
      arg = 0
      write(*,*) is_iostat_eor(i=arg)
      arg = 4
      write(*,*) is_iostat_eor(i=arg*arg+2)
      write(*,*) is_iostat_eor(i=-arg)

      end
