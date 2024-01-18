!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: fxiosendeor002.f
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
!*  TEST CASE TITLE            : fxiosendeor002 
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : Aug. 17, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_end and is_iostat_eor intrinsics
!*  SECONDARY FUNCTIONS TESTED : None 
!*
!*  DRIVER STANZA              : xlf90
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                : This diagnostic test, makes sure that only 1 argument
!*                               is allowed to be passed to these intrinsics.
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none
      integer :: a = 1, b = 2, c = 3
      integer, dimension(3) :: aa, bb, cc
      logical :: ignore
      logical, dimension(3) :: igArr

      ignore = is_iostat_end()
      ignore = is_iostat_end(a)  ! should be fine

      igArr  = is_iostat_end()
      igArr  = is_iostat_end(aa) ! should be fine

      ignore = is_iostat_end(a, b)
      igArr = is_iostat_end(aa, bb)

      ignore = is_iostat_end(a, b, c)
      igArr = is_iostat_end(aa, bb, cc)

      ignore = is_iostat_end(b, c)
      igArr = is_iostat_end(cc, bb)

      ignore = is_iostat_eor()
      ignore = is_iostat_eor(a)  ! should be fine

      igArr  = is_iostat_eor()
      igArr  = is_iostat_eor(aa) ! should be fine

      ignore = is_iostat_eor(a, b)
      igArr = is_iostat_eor(aa, bb)

      ignore = is_iostat_eor(a, b, c)
      igArr = is_iostat_eor(aa, bb, cc)

      ignore = is_iostat_eor(b, c)
      igArr = is_iostat_eor(cc, bb)
      end
