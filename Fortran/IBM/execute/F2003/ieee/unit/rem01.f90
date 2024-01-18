!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qstrict
! %GROUP:  rem01.f
! %VERIFY:
! %STDIN:
! %STDOUT: rem01.out
! %EXECARGS:
! %POSTCMD: $TR_SRC/rem01.posh rem01
! %END
!**********************************************************************

!*  ===================================================================
!*
!*  DATE                       : August, 2002
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : ieee_rem
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : test that ieee_rem sets ieee_invalid
!*                               and not ieee_divide_by_zero
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      use ieee_arithmetic
      real*4 xr, yr
      real*8 xd, yd
      real*16 xq, yq
      logical val(5)

      real*4,  parameter :: RINF = z"7f800000"
      real*8,  parameter :: DINF = z"7ff0000000000000"
      real*16, parameter :: QINF = z"fff00000000000000000000000000000"

      real*4,  parameter :: RNANQ = z"7ff00000"
      real*8,  parameter :: DNANQ = z"fff8000000000000"
      real*16, parameter :: QNANQ = z"fff80000000000000000000000000000"


      call ieee_set_flag(ieee_all, .false.)

      xr = RINF
      yr = 2.0
      yr = ieee_rem(xr,yr)
      call ieee_get_flag(ieee_all, val)
      print *, val
      print *, yr

      call ieee_set_flag(ieee_all, .false.)

      xr = 2.0
      yr = RINF
      yr = ieee_rem(xr,yr)
      call ieee_get_flag(ieee_all, val)
      print *, val
      print *, yr
      print *, ""

      call ieee_set_flag(ieee_all, .false.)

      xr = 0.0
      yr = 2.0
      yr = ieee_rem(xr,yr)
      call ieee_get_flag(ieee_all, val)
      print *, val
      print *, yr

      call ieee_set_flag(ieee_all, .false.)

      xr = 2.0
      yr = 0.0
      yr = ieee_rem(xr,yr)
      call ieee_get_flag(ieee_all, val)
      print *, val
      print *, yr
      print *, ""

      call ieee_set_flag(ieee_all, .false.)

      xr = RNANQ
      yr = RNANQ
      yr = ieee_rem(xr,yr)
      call ieee_get_flag(ieee_all, val)
      print *, val
      print *, yr

      call ieee_set_flag(ieee_all, .false.)

      xr = RNANQ
      yr = 2.0
      yr = ieee_rem(xr,yr)
      call ieee_get_flag(ieee_all, val)
      print *, val
      print *, yr

      call ieee_set_flag(ieee_all, .false.)

      xr = 2.0
      yr = RNANQ
      yr = ieee_rem(xr,yr)
      call ieee_get_flag(ieee_all, val)
      print *, val
      print *, yr
      print *, ""

      call ieee_set_flag(ieee_all, .false.)

      xr = RINF
      yr = 0.0
      yr = ieee_rem(xr,yr)
      call ieee_get_flag(ieee_all, val)
      print *, val
      print *, yr

      call ieee_set_flag(ieee_all, .false.)

      xr = 0.0
      yr = RINF
      yr = ieee_rem(xr,yr)
      call ieee_get_flag(ieee_all, val)
      print *, val
      print *, yr
      print *, ""

      call ieee_set_flag(ieee_all, .false.)

      xr = RNANQ
      yr = RINF
      yr = ieee_rem(xr,yr)
      call ieee_get_flag(ieee_all, val)
      print *, val
      print *, yr

      call ieee_set_flag(ieee_all, .false.)

      xr = RNANQ
      yr = 0.0
      yr = ieee_rem(xr,yr)
      call ieee_get_flag(ieee_all, val)
      print *, val
      print *, yr

      call ieee_set_flag(ieee_all, .false.)

      xr = RINF
      yr = RNANQ
      yr = ieee_rem(xr,yr)
      call ieee_get_flag(ieee_all, val)
      print *, val
      print *, yr

      call ieee_set_flag(ieee_all, .false.)

      xr = 0.0
      yr = RNANQ
      yr = ieee_rem(xr,yr)
      call ieee_get_flag(ieee_all, val)
      print *, val
      print *, yr
      print *, ""

      ! Test real*8 x, real*8 y
      print *, "(real*8 x, real*8 y)"

      call ieee_set_flag(ieee_all, .false.)

      xd = DINF
      yd = 2.0
      yd = ieee_rem(xd,yd)
      call ieee_get_flag(ieee_all, val)
      print *, val
      print *, yd

      call ieee_set_flag(ieee_all, .false.)

      xd = 2.0
      yd = DINF
      yd = ieee_rem(xd,yd)
      call ieee_get_flag(ieee_all, val)
      print *, val
      print *, yd
      print *, ""

      call ieee_set_flag(ieee_all, .false.)

      xd = 0.0
      yd = 2.0
      yd = ieee_rem(xd,yd)
      call ieee_get_flag(ieee_all, val)
      print *, val
      print *, yd

      call ieee_set_flag(ieee_all, .false.)

      xd = 2.0
      yd = 0.0
      yd = ieee_rem(xd,yd)
      call ieee_get_flag(ieee_all, val)
      print *, val
      print *, yd
      print *, ""

      call ieee_set_flag(ieee_all, .false.)

      xd = DNANQ
      yd = DNANQ
      yd = ieee_rem(xd,yd)
      call ieee_get_flag(ieee_all, val)
      print *, val
      print *, yd

      call ieee_set_flag(ieee_all, .false.)

      xd = DNANQ
      yd = 2.0
      yd = ieee_rem(xd,yd)
      call ieee_get_flag(ieee_all, val)
      print *, val
      print *, yd

      call ieee_set_flag(ieee_all, .false.)

      xd = 2.0
      yd = DNANQ
      yd = ieee_rem(xd,yd)
      call ieee_get_flag(ieee_all, val)
      print *, val
      print *, yd
      print *, ""

      call ieee_set_flag(ieee_all, .false.)

      xd = DINF
      yd = 0.0
      yd = ieee_rem(xd,yd)
      call ieee_get_flag(ieee_all, val)
      print *, val
      print *, yd

      call ieee_set_flag(ieee_all, .false.)

      xd = 0
      yd = DINF
      yd = ieee_rem(xd,yd)
      call ieee_get_flag(ieee_all, val)
      print *, val
      print *, yd
      print *, ""

      call ieee_set_flag(ieee_all, .false.)

      xd = DNANQ
      yd = DINF
      yd = ieee_rem(xd,yd)
      call ieee_get_flag(ieee_all, val)
      print *, val
      print *, yd

      call ieee_set_flag(ieee_all, .false.)

      xd = DNANQ
      yd = 0.0
      yd = ieee_rem(xd,yd)
      call ieee_get_flag(ieee_all, val)
      print *, val
      print *, yd

      call ieee_set_flag(ieee_all, .false.)

      xd = DINF
      yd = DNANQ
      yd = ieee_rem(xd,yd)
      call ieee_get_flag(ieee_all, val)
      print *, val
      print *, yd

      call ieee_set_flag(ieee_all, .false.)

      xd = 0.0
      yd = DNANQ
      yd = ieee_rem(xd,yd)
      call ieee_get_flag(ieee_all, val)
      print *, val
      print *, yd
      print *, ""

      ! Test real*16 x, real*16 y
      print *, "(real*16 x, real*16 y)"

      call ieee_set_flag(ieee_all, .false.)

      xq = QINF
      yq = 2.0
      yq = ieee_rem(xq,yq)
      call ieee_get_flag(ieee_all, val)
      print *, val
      print *, yq

      call ieee_set_flag(ieee_all, .false.)

      xq = 2.0
      yq = QINF
      yq = ieee_rem(xq,yq)
      call ieee_get_flag(ieee_all, val)
      print *, val
      print *, yq
      print *, ""

      call ieee_set_flag(ieee_all, .false.)

      xq = 0.0
      yq = 2.0
      yq = ieee_rem(xq,yq)
      call ieee_get_flag(ieee_all, val)
      print *, val
      print *, yq

      call ieee_set_flag(ieee_all, .false.)

      xq = 2.0
      yq = 0.0
      yq = ieee_rem(xq,yq)
      call ieee_get_flag(ieee_all, val)
      print *, val
      print *, yq
      print *, ""

      call ieee_set_flag(ieee_all, .false.)

      xq = QNANQ
      yq = QNANQ
      yq = ieee_rem(xq,yq)
      call ieee_get_flag(ieee_all, val)
      print *, val
      print *, yq

      call ieee_set_flag(ieee_all, .false.)

      xq = QNANQ
      yq = 2.0
      yq = ieee_rem(xq,yq)
      call ieee_get_flag(ieee_all, val)
      print *, val
      print *, yq

      call ieee_set_flag(ieee_all, .false.)

      xq = 2.0
      yq = QNANQ
      yq = ieee_rem(xq,yq)
      call ieee_get_flag(ieee_all, val)
      print *, val
      print *, yq
      print *, ""

      call ieee_set_flag(ieee_all, .false.)

      xq = QINF
      yq = 0.0
      yq = ieee_rem(xq,yq)
      call ieee_get_flag(ieee_all, val)
      print *, val
      print *, yq

      call ieee_set_flag(ieee_all, .false.)

      xq = 0
      yq = QINF
      yq = ieee_rem(xq,yq)
      call ieee_get_flag(ieee_all, val)
      print *, val
      print *, yq
      print *, ""

      call ieee_set_flag(ieee_all, .false.)

      xq = QNANQ
      yq = QINF
      yq = ieee_rem(xq,yq)
      call ieee_get_flag(ieee_all, val)
      print *, val
      print *, yq

      call ieee_set_flag(ieee_all, .false.)

      xq = QNANQ
      yq = 0.0
      yq = ieee_rem(xq,yq)
      call ieee_get_flag(ieee_all, val)
      print *, val
      print *, yq

      call ieee_set_flag(ieee_all, .false.)

      xq = QINF
      yq = QNANQ
      yq = ieee_rem(xq,yq)
      call ieee_get_flag(ieee_all, val)
      print *, val
      print *, yq

      call ieee_set_flag(ieee_all, .false.)

      xq = 0.0
      yq = QNANQ
      yq = ieee_rem(xq,yq)
      call ieee_get_flag(ieee_all, val)
      print *, val
      print *, yq
      print *, ""


end
