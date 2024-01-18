! ************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f ieee_*.mod xlf_fp_util.mod constants_for_ieee.mod
! %COMPOPTS: 
! %GROUP: ../ieeeconsts.f intrimod05.f
! %VERIFY: intrimod05.out:../emptyout.vf
! %STDIN:
! %STDOUT: intrimod05.out
! %EXECARGS:
! %POSTCMD: rm -f ieee_*.mod xlf_fp_util.mod constants_for_ieee.mod
! %END
!************************************************************************
!************************************************************************
!*
!*  FORTRAN TEST CASE            IBM INTERNAL USE ONLY
!*  Test Case Title  : INTRINSIC/NON_INTRINSIC module nature
!*  Test Case Name   : intrimod05.f
!*  Created By       : Bahram Chehrazy
!*  DATE             : January, 2004
!*  Description      : Use INTRINSIC and NON_INTRINSIC modules in a
!*                     module subroutine/function. 
!*                     The output should be empty.
!*
!*************************************************************************
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  01/23/04   BC     Initial Version
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
!

!... These are fake ieee modules

        module ieee_arithmetic

            character*40 :: c1='This is a fake ieee_arithmetic module.'
            contains
                subroutine ieee_set_rounding_mode()
                   print *, c1
                end subroutine

                function ieee_rint(r1)
                    real r1, ieee_rint
                    ieee_rint = r1 * 1234.56789E-12
                    print *, "You've called a wrong ieee routine."
                end function ieee_rint

                function ieee_value(r1, tp)
                    real r1, ieee_value
                    integer tp
                    ieee_value = r1 * tp
                    print *, "You've called a wrong ieee routine."
                end function ieee_value

                function ieee_class(r1)
                    real r1, ieee_class
                    ieee_class = r1 / .43342
                    print *, "You've called a wrong ieee routine."
                end function ieee_class

                function ieee_next_after(r1,r2)
                    real r1, r2, ieee_next_after
                    ieee_next_after = r1 * r2
                    print *, "You've called a wrong ieee routine."
                end function ieee_next_after

                function ieee_support_datatype(r4)    
                    real*4 r4
                    logical ieee_support_datatype
                    print *, "You've called a wrong ieee routine."
                    ieee_support_datatype=.false.
                end function ieee_support_datatype

                function ieee_is_finite(r4)
                    real*4 r4
                    logical ieee_is_finite
                    print *, "You've called a wrong ieee routine."
                    ieee_is_finite=.false.
                end function ieee_is_finite

        end module ieee_arithmetic


        module ieee_exceptions

            character*40 :: c2='This is a fake ieee_exceptions module.'
            logical :: ieee_flag=.false.

            contains
                subroutine ieee_get_rounding_mode()
                   print *, c2
                end subroutine

                subroutine ieee_get_status( v1 )
                     integer v1
                     print *, "You've called a wrong status routine."
                     v1 = 1313
                end subroutine ieee_get_status

                subroutine ieee_set_status( v1 )
                     integer v1
                     print *, "You've called a wrong status routine : ", v1
                end subroutine ieee_set_status

                subroutine ieee_set_flag(flg)
                    logical flg

                    print *, "You've called a wrong flag routine."
                    ieee_flag = flg
                end subroutine ieee_set_flag

                subroutine ieee_get_flag(flg, hlt)
                    integer flg
                    logical hlt

                    print *, "You've called a wrong flag routine."
                    hlt = .true.
                end subroutine ieee_get_flag

        end module ieee_exceptions


        module xlf_fp_util
            Character*40 :: c2="This is a fake xlf_fp_util module."

            contains

                function get_fpscr_flags(i2)
                    integer i2, get_fpscr_flags
                    print *, c2
                    get_fpscr_flags=1313
                end function get_fpscr_flags

                subroutine set_fpscr_flags(i2)
                    integer i2
                    print *, c2
                end subroutine set_fpscr_flags

                subroutine clr_fpscr_flags(i2)
                    integer i2
                    print *, c2
                end subroutine clr_fpscr_flags

        end module xlf_fp_util



!... A non_intrinsic module that uses the intrinsic modules
       module mod1

         contains

!... Calling some ieee procedures and evaluating the results. The real
!... intrinsic ieee module must have been called.
            subroutine sub1()
               use, intrinsic :: ieee_arithmetic
	       use, intrinsic :: ieee_exceptions
               use, non_intrinsic :: constants_for_ieee
               use, intrinsic :: xlf_fp_util

               real*4 yr
               type(ieee_round_type) :: rtype
               type(ieee_round_type), parameter :: rt_nearest = IEEE_NEAREST
               type(ieee_status_type) :: status_value
               logical :: flag_values(5)
               integer(fpscr_kind), dimension(5) :: flags


               flags = (/ fp_overflow, fp_div_by_zero, fp_invalid, &
     &               fp_underflow, fp_inexact /)

               call ieee_get_flag(ieee_all, flag_values)
               do k = 1, 5
                  if (flag_values(k) .neqv. .false. ) stop 10
               enddo
           
               if (ieee_support_datatype(PINF_4) .AND. &
 	           ieee_support_datatype(NINF_4)) then
                  if (ieee_is_finite(PINF_4) .OR. ieee_is_finite(NINF_4)) stop 12
               endif

               if (ieee_support_datatype(PHD_4) .AND. ieee_support_datatype(PTD_4)) then
                   if (ieee_is_finite(PHD_4) .neqv. .true.) stop 13
                   if (ieee_is_finite(PTD_4) .neqv. .true.) stop 14
               endif

               call ieee_get_status(status_value)
               call ieee_set_rounding_mode(rt_nearest)
               call ieee_get_rounding_mode(rtype)
               if (rtype /= rt_nearest) stop 15
               yr = ieee_rint(1.1)
               if (yr /= 1.0) stop 16
               call ieee_set_status(status_value)

!... Testing xlf_fp_util module
               call set_fpscr_flags(flags(1))
               call clr_fpscr_flags(flags(5)) 
               if ( get_fpscr_flags(flags(1)) .eq. 0 ) stop 17
               if ( get_fpscr_flags(flags(5)) .ne. 0 ) stop 18

            end subroutine sub1


            logical function fun1() 
               use, intrinsic :: ieee_arithmetic
	       use, intrinsic :: ieee_exceptions
               use, non_intrinsic :: constants_for_ieee
               use, intrinsic :: xlf_fp_util

               real*4 yr
               type(ieee_round_type) :: rtype
               type(ieee_round_type), parameter :: rt_nearest = IEEE_NEAREST
               type(ieee_status_type) :: status_value
               logical :: flag_values(5)
               integer(fpscr_kind), dimension(5) :: flags


               flags = (/ fp_overflow, fp_div_by_zero, fp_invalid, &
     &               fp_underflow, fp_inexact /)

               call ieee_get_flag(ieee_all, flag_values)
               do k = 1, 5
                  if (flag_values(k) .neqv. .false. ) stop 30
               enddo
           
               if (ieee_support_datatype(PINF_4) .AND. &
 	           ieee_support_datatype(NINF_4)) then
                  if (ieee_is_finite(PINF_4) .OR. ieee_is_finite(NINF_4)) stop 32
               endif

               if (ieee_support_datatype(PHD_4) .AND. ieee_support_datatype(PTD_4)) then
                   if (ieee_is_finite(PHD_4) .neqv. .true.) stop 33
                   if (ieee_is_finite(PTD_4) .neqv. .true.) stop 34
               endif

               call ieee_get_status(status_value)
               call ieee_set_rounding_mode(rt_nearest)
               call ieee_get_rounding_mode(rtype)
               if (rtype /= rt_nearest) stop 35
               yr = ieee_rint(1.1)
               if (yr /= 1.0) stop 36
               call ieee_set_status(status_value)

               call set_fpscr_flags(flags(1))
               call clr_fpscr_flags(flags(5)) 
               if ( get_fpscr_flags(flags(1)) .eq. 0 ) stop 37
               if ( get_fpscr_flags(flags(5)) .ne. 0 ) stop 38

               fun1=.true.
            end function fun1

      end module mod1


      program intrimod05
         use mod1
         call sub1()
         if ( .not. fun1() ) stop 20
      end program
