! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fxieee.presh scalb005
! %COMPOPTS: -qfree=f90 -qstrict
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: rm -f *.mod
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : F2K IEEE Modules
!*
!*  PROGRAMMER                 : Alexandru Mihaileanu
!*  DATE                       : March 14, 2002
!*  ORIGIN                     : XL Fortran Development
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_SCALB with internal procedures.
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : IEEE_SCALB(x,i) = 2**i*x
!*  This testcase tests that values are passed correctly to the dummy 
!*  arguments of an internal procedure.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      program scalb_intpro

        use ieee_arithmetic
	implicit none
        
        real*4 :: xar4(2),ar4
        integer :: iar(2)
	real*8 ::  xar8(2),ar8
        real*16 :: xar16(2), ar16
        logical :: flag_values(5)
        type(ieee_status_type) :: status_value

		
	xar4 = (/16, 18/)
	iar = (/5, 10/)
	xar8 = (/16_8, 18_8/)
        xar16 = (/126.0_16, 168.0_16/)
     
!Test real*4

        call ieee_get_status(status_value)

        call bar1(xar4, iar)

!Test real*8

        call ieee_set_status(status_value)

        call bar2(xar8, iar)

!Test real*16

        call ieee_set_status(status_value)

        call bar3(xar16, iar)

      contains

        subroutine bar1(x, a)

          real*4 :: x(2), resx(2), result4(2)
          integer :: a(2), i
          logical :: flag_value

          resx = ieee_scalb(x,a)

          ! Now check that no flags were turned on.
          call ieee_get_flag(ieee_all,flag_values)
          do i = 1,5
              if (flag_values(i) .neqv. .false.)error stop 101
          enddo

          do i = 1, 2
             result4(i) = 2.0_4**a(i)*x(i)
             if ( resx(i) /= result4(i) ) call zzrc(i+10)
          enddo
        end subroutine bar1

        subroutine bar2(y, b)
          real*8 :: y(2), resy(2), result8(2)
          integer :: b(2), i
          logical :: flag_value

          resy = ieee_scalb(y,b)

          ! Now check that no flags were turned on.
          call ieee_get_flag(ieee_all,flag_values)
          do i = 1,5
              if (flag_values(i) .neqv. .false.)error stop 201
          enddo

          do i = 1, 2
             result8(i) = 2.0_8**b(i)*y(i)
             if ( resy(i) /= result8(i) ) call zzrc(i+20)
          enddo
        end subroutine bar2


        subroutine bar3(z, c)
          real*16 :: z(2), resz(2), result16(2)
          integer :: c(2), i
          logical :: flag_value

          resz = ieee_scalb(z,c)

          ! Now check that no flags were turned on.
          call ieee_get_flag(ieee_all,flag_values)
          do i = 1,5
              if (flag_values(i) .neqv. .false.)error stop 301
          enddo

          do i = 1, 2
             result16(i) = 2.0_16**c(i)*z(i)
             if ( resz(i) /= result16(i) ) call zzrc(i+30)
          enddo
        end subroutine bar3

      end program

