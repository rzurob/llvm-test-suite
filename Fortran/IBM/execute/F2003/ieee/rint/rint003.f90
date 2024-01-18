! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:  $TR_SRC/fxieee.presh rint003
! %COMPOPTS: -qstrict -qfloat=rrm:nofold -qfree=f90 -qxlf90=signedzero
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: rm -f *.mod
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 13, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_RINT  with array sections.
!*  SECONDARY FUNCTIONS TESTED : IEEE_GET_ROUNDING_MODE
!*                               IEEE_SET_ROUNDING_MODE
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : This testcase contains :
!* 1.Test rounding to nereast
!* 2.Test rounding to zero
!* 3.Test rounding to +INF
!* 4.Test rounding to -INF
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      program fxieee05

         use ieee_arithmetic
         use ieee_exceptions
         use constants_for_ieee

   implicit none
   integer :: i
         real*4 :: xr(20), res(20), yr(20)
   real*8 :: xr_8(20), res_8(20), yr_8(20)
   real*16 :: xr_16(20), res_16(20), yr_16(20)
   type(ieee_round_type) :: rtype
   type(ieee_round_type), parameter :: rt_nearest = IEEE_NEAREST
   type(ieee_round_type), parameter :: rt_20 = IEEE_TO_ZERO
   type(ieee_round_type), parameter :: rt_up = IEEE_UP
   type(ieee_round_type), parameter :: rt_down = IEEE_DOWN
   type(ieee_round_type), parameter :: rt_other = IEEE_OTHER
   type(ieee_status_type) :: status_value, status_value2
         logical :: flag_values(5)


!  Test nereast

             call ieee_get_status(status_value)

             call ieee_set_rounding_mode(rt_nearest)
             call ieee_get_rounding_mode(rtype)
             if (rtype /= rt_nearest) error stop 1

             !  test real*4

             call ieee_get_status(status_value2)
             xr(1:20:2) = (/ (i+0.4,i=1,10,1) /)
             xr(2:20:2) = (/ (i-0.4,i=-1,-10,-1) /)
             yr(1:20:2) = (/ (i,i=1,10,1) /)
             yr(2:20:2) = (/ (i,i=-1,-10,-1) /)
             call ieee_set_status(status_value2)

             res = ieee_rint(xr)

             call ieee_get_status(status_value2)
             if (.not. comp_4(res, yr)) error stop 2
             call ieee_set_status(status_value2)


             !  test real*8

             call ieee_get_status(status_value2)
             xr_8(1:20:2) = (/ (i+0.4,i=1,10,1) /)
             xr_8(2:20:2) = (/ (i-0.4,i=-1,-10,-1) /)
             yr_8(1:20:2) = (/ (i,i=1,10,1) /)
             yr_8(2:20:2) = (/ (i,i=-1,-10,-1) /)
             call ieee_set_status(status_value2)

             res_8 = ieee_rint(xr_8)

             call ieee_get_status(status_value2)
             if (.not. comp_8(res_8, yr_8)) error stop 4
             call ieee_set_status(status_value2)


             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,5
                 if (flag_values(i) .neqv. .false.)error stop 50
             enddo

             !  test real*16

             call ieee_get_status(status_value2)
             xr_16(1:20:2) = (/ (i+0.4,i=1,10,1) /)
             xr_16(2:20:2) = (/ (i-0.4,i=-1,-10,-1) /)
             yr_16(1:20:2) = (/ (i,i=1,10,1) /)
             yr_16(2:20:2) = (/ (i,i=-1,-10,-1) /)
             call ieee_set_status(status_value2)

             res_16 = ieee_rint(xr_16)

             call ieee_get_status(status_value2)
             if (.not. comp_16(res_16, yr_16)) error stop 5
             call ieee_set_status(status_value2)


             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,4
                 if (flag_values(i) .neqv. .false.)error stop 50
             enddo


! Test  to zero

            call ieee_set_status(status_value)
            call ieee_set_rounding_mode(rt_20)
            call ieee_get_rounding_mode(rtype)
            if (rtype /= rt_20) error stop 6

             ! test real*4

             call ieee_get_status(status_value2)
             xr(1:20:2) = (/ (i+0.4,i=1,10,1) /)
             xr(2:20:2) = (/ (i-0.4,i=-1,-10,-1) /)
             yr(1:20:2) = (/ (i,i=1,10,1) /)
             yr(2:20:2) = (/ (i,i=-1,-10,-1) /)
             call ieee_set_status(status_value2)

             res = ieee_rint(xr)

             call ieee_get_status(status_value2)
             if (.not. comp_4(res, yr)) error stop 7
             call ieee_set_status(status_value2)

             !  test real*8

             call ieee_get_status(status_value2)
             xr_8(1:20:2) = (/ (i+0.4,i=1,10,1) /)
             xr_8(2:20:2) = (/ (i-0.4,i=-1,-10,-1) /)
             yr_8(1:20:2) = (/ (i,i=1,10,1) /)
             yr_8(2:20:2) = (/ (i,i=-1,-10,-1) /)
             call ieee_set_status(status_value2)

             res_8 = ieee_rint(xr_8)

             call ieee_get_status(status_value2)
             if (.not. comp_8(res_8, yr_8)) error stop 8
             call ieee_set_status(status_value2)

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,5
                 if (flag_values(i) .neqv. .false.)error stop 60
             enddo

             !  test real*16

             call ieee_get_status(status_value2)
             xr_16(1:20:2) = (/ (i+0.4,i=1,10,1) /)
             xr_16(2:20:2) = (/ (i-0.4,i=-1,-10,-1) /)
             yr_16(1:20:2) = (/ (i,i=1,10,1) /)
             yr_16(2:20:2) = (/ (i,i=-1,-10,-1) /)
             call ieee_set_status(status_value2)

             res_16 = ieee_rint(xr_16)

             call ieee_get_status(status_value2)
             if (.not. comp_16(res_16, yr_16)) error stop 9
             call ieee_set_status(status_value2)

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,4
                 if (flag_values(i) .neqv. .false.)error stop 60
             enddo


! Test +INF

            call ieee_set_status(status_value)

            call ieee_set_rounding_mode(rt_up)
            call ieee_get_rounding_mode(rtype)
            if (rtype /= rt_up) error stop 10

            ! test real*4

             call ieee_get_status(status_value2)
             xr(1:20:2) = (/ (i+0.4,i=1,10,1) /)
             xr(2:20:2) = (/ (i-0.4,i=-1,-10,-1) /)
             yr(1:20:2) = (/ (i,i=2,11,1) /)
             yr(2:20:2) = (/ (i,i=-1,-10,-1) /)
             call ieee_set_status(status_value2)

             res = ieee_rint(xr)

             call ieee_get_status(status_value2)
             if (.not. comp_4(res, yr)) error stop 11
             call ieee_set_status(status_value2)

            !  test real*8


             call ieee_get_status(status_value2)
             xr_8(1:20:2) = (/ (i+0.4,i=1,10,1) /)
             xr_8(2:20:2) = (/ (i-0.4,i=-1,-10,-1) /)
             yr_8(1:20:2) = (/ (i,i=2,11,1) /)
             yr_8(2:20:2) = (/ (i,i=-1,-10,-1) /)
             call ieee_set_status(status_value2)

             res_8 = ieee_rint(xr_8)

             call ieee_get_status(status_value2)
             if (.not. comp_8(res_8, yr_8)) error stop 12
             call ieee_set_status(status_value2)

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,5
                 if (flag_values(i) .neqv. .false.)error stop 70
             enddo

            !  test real*16

             call ieee_get_status(status_value2)
             xr_16(1:20:2) = (/ (i+0.4,i=1,10,1) /)
             xr_16(2:20:2) = (/ (i-0.4,i=-1,-10,-1) /)
             yr_16(1:20:2) = (/ (i,i=2,11,1) /)
             yr_16(2:20:2) = (/ (i,i=-1,-10,-1) /)
             call ieee_set_status(status_value2)

             res_16 = ieee_rint(xr_16)

             call ieee_get_status(status_value2)
             if (.not. comp_16(res_16, yr_16)) error stop 13
             call ieee_set_status(status_value2)

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,4
                 if (flag_values(i) .neqv. .false.)error stop 70
             enddo



! test to -INF

             call ieee_set_status(status_value)

             call ieee_set_rounding_mode(rt_down)
             call ieee_get_rounding_mode(rtype)
             if (rtype /= rt_down) error stop 14

             ! test real*4

             call ieee_get_status(status_value2)
             xr(1:20:2) = (/ (i+0.4,i=1,10,1) /)
             xr(2:20:2) = (/ (i-0.4,i=-1,-10,-1) /)
             yr(1:20:2) = (/ (i,i=1,10,1) /)
             yr(2:20:2) = (/ (i,i=-2,-11,-1) /)
             call ieee_set_status(status_value2)

             res = ieee_rint(xr)

             call ieee_get_status(status_value2)
             if (.not. comp_4(res, yr)) error stop 15
             call ieee_set_status(status_value2)

             !  test real*8

             call ieee_get_status(status_value2)
             xr_8(1:20:2) = (/ (i+0.4,i=1,10,1) /)
             xr_8(2:20:2) = (/ (i-0.4,i=-1,-10,-1) /)
             yr_8(1:20:2) = (/ (i,i=1,10,1) /)
             yr_8(2:20:2) = (/ (i,i=-2,-11,-1) /)
             call ieee_set_status(status_value2)

             res_8 = ieee_rint(xr_8)

             call ieee_get_status(status_value2)
             if (.not. comp_8(res_8, yr_8)) error stop 16
             call ieee_set_status(status_value2)

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,5
                 if (flag_values(i) .neqv. .false.)error stop 80
             enddo

             !  test real*16

             call ieee_get_status(status_value2)
             xr_16(1:20:2) = (/ (i+0.4,i=1,10,1) /)
             xr_16(2:20:2) = (/ (i-0.4,i=-1,-10,-1) /)
             yr_16(1:20:2) = (/ (i,i=1,10,1) /)
             yr_16(2:20:2) = (/ (i,i=-2,-11,-1) /)
             call ieee_set_status(status_value2)

             res_16 = ieee_rint(xr_16)

             call ieee_get_status(status_value2)
             if (.not. comp_16(res_16, yr_16)) error stop 17
             call ieee_set_status(status_value2)

             ! Now check that no flags were turned on.
             call ieee_get_flag(ieee_all,flag_values)
             do i = 1,4
                 if (flag_values(i) .neqv. .false.)error stop 80
             enddo



       contains

        logical function comp_4(arr1, arr2)
          real*4 :: arr1(20), arr2(20)
          logical precision_r4
          do i = 1, size(arr1)
          if  (.not.precision_r4(arr1(i),arr2(i))) then
          comp_4 = .false.
          return
          endif
          end do
          comp_4 = .true.
        end function comp_4

        logical function comp_8(arr1, arr2)
          real*8 :: arr1(20), arr2(20)
          logical precision_r8
          do i = 1, size(arr1)
          if  (.not.precision_r8(arr1(i),arr2(i))) then
          comp_8 = .false.
          return
          endif
          end do
          comp_8 = .true.
        end function comp_8

        logical function comp_16(arr1, arr2)
          real*16 :: arr1(20), arr2(20)
          logical precision_r6
          do i = 1, size(arr1)
          if  (.not.precision_r6(arr1(i),arr2(i))) then
          comp_16 = .false.
          return
          endif
          end do
          comp_16 = .true.
        end function comp_16

      end program
