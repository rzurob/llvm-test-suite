!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: fxiosendeor026.f
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
!*  TEST CASE TITLE            : fxiosendeor026
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : Aug. 26, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_end and is_iostat_eor intrinsics
!*  SECONDARY FUNCTIONS TESTED : None 
!*
!*  DRIVER STANZA              : xlf90
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                : This tests the use of various kinds of integer arguments.
!*                               ie. integer*1
!*                                   integer*2
!*                                   integer*4
!*                                   integer*8
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none
      
      integer*1 :: int1
      integer*2 :: int2
      integer*4 :: int4
      integer*8 :: int8
      
  ! TESTING IS_IOSTAT_END

      int1 = -1
      int2 = -1
      int4 = -1
      int8 = -1
      write(*,*) is_iostat_end(int1)
      write(*,*) is_iostat_end(int2)
      write(*,*) is_iostat_end(int4)
      write(*,*) is_iostat_end(int8)

      write(*,*)

      int1 = -2
      int2 = -2
      int4 = -2
      int8 = -2
      write(*,*) is_iostat_end(int1)
      write(*,*) is_iostat_end(int2)
      write(*,*) is_iostat_end(int4)
      write(*,*) is_iostat_end(int8)

      write(*,*)

      int1 = -3
      int2 = -3
      int4 = -3
      int8 = -3
      write(*,*) is_iostat_end(int1)
      write(*,*) is_iostat_end(int2)
      write(*,*) is_iostat_end(int4)
      write(*,*) is_iostat_end(int8)

      write(*,*)

      int1 = -4
      int2 = -4
      int4 = -4
      int8 = -4
      write(*,*) is_iostat_end(int1)
      write(*,*) is_iostat_end(int2)
      write(*,*) is_iostat_end(int4)
      write(*,*) is_iostat_end(int8)

      write(*,*)

      int1 = 0
      int2 = 0
      int4 = 0
      int8 = 0
      write(*,*) is_iostat_end(int1)
      write(*,*) is_iostat_end(int2)
      write(*,*) is_iostat_end(int4)
      write(*,*) is_iostat_end(int8)

      write(*,*)

      int1 = 1
      int2 = 1
      int4 = 1
      int8 = 1
      write(*,*) is_iostat_end(int1)
      write(*,*) is_iostat_end(int2)
      write(*,*) is_iostat_end(int4)
      write(*,*) is_iostat_end(int8)

      write(*,*) "\n--\n"
  ! TESTING IS_IOSTAT_EOR

      int1 = -1
      int2 = -1
      int4 = -1
      int8 = -1
      write(*,*) is_iostat_eor(int1)
      write(*,*) is_iostat_eor(int2)
      write(*,*) is_iostat_eor(int4)
      write(*,*) is_iostat_eor(int8)

      write(*,*)

      int1 = -2
      int2 = -2
      int4 = -2
      int8 = -2
      write(*,*) is_iostat_eor(int1)
      write(*,*) is_iostat_eor(int2)
      write(*,*) is_iostat_eor(int4)
      write(*,*) is_iostat_eor(int8)

      write(*,*)

      int1 = -3
      int2 = -3
      int4 = -3
      int8 = -3
      write(*,*) is_iostat_eor(int1)
      write(*,*) is_iostat_eor(int2)
      write(*,*) is_iostat_eor(int4)
      write(*,*) is_iostat_eor(int8)

      write(*,*)

      int1 = -4
      int2 = -4
      int4 = -4
      int8 = -4
      write(*,*) is_iostat_eor(int1)
      write(*,*) is_iostat_eor(int2)
      write(*,*) is_iostat_eor(int4)
      write(*,*) is_iostat_eor(int8)

      write(*,*)

      int1 = 0
      int2 = 0
      int4 = 0
      int8 = 0
      write(*,*) is_iostat_eor(int1)
      write(*,*) is_iostat_eor(int2)
      write(*,*) is_iostat_eor(int4)
      write(*,*) is_iostat_eor(int8)

      write(*,*)

      int1 = 1
      int2 = 1
      int4 = 1
      int8 = 1
      write(*,*) is_iostat_eor(int1)
      write(*,*) is_iostat_eor(int2)
      write(*,*) is_iostat_eor(int4)
      write(*,*) is_iostat_eor(int8)

      end
