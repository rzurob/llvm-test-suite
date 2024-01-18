!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qstrict
! %GROUP:  fxmisc01.f
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
!*  TEST CASE TITLE            : F2K IEEE Modules
!*
!*  PROGRAMMER                 : Rafik Zurob
!*  DATE                       : June 12, 2002
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ieee_rem, ieee_invalid
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : test that ieee_rem sets ieee_invalid
!*                               when x/y can't fit into an integer.
!*                               In this test case, we don't care
!*                               about the return value, just the
!*                               flags
!234567890123456789012345678901234567890123456789012345678901234567890
      use ieee_arithmetic
      implicit none
      integer*4 i
      real*8 dx, dy, result
      logical val(5), exp(4)

      ! dx/dy fits in an integer*8, so no flag will be set.
      dx = huge(i) * 4.4d0
      dy = 2.0d0
      result = ieee_rem(dx,dy)
      call ieee_get_flag(ieee_all,val)
      print *, result
      do i = 1, 4
        if (val(i) .neqv. .false.) call zzrc(i)
      enddo
      
      ! dx/dy doesn't fit in an integer*8, so ieee_invalid will be set.
      dx = huge(dx)
      dy = 2.0d0
      result = ieee_rem(dx,dy)
      call ieee_get_flag(ieee_all,val)
      print *, result  ! result may be inaccurate. don't care about it.
      exp = (/ .false., .false., .true., .false. /)
      do i = 1, 4
        if (val(i) .neqv. exp(i)) call zzrc(4+i)
      enddo

      end
