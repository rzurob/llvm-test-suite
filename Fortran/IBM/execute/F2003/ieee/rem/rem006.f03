! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 14, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_REM with modules.
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      module modus

         contains

         subroutine bar1(x4, y4, z4)

            use ieee_arithmetic
	    implicit none
            real*4 :: x4, y4, z4
            logical :: flag_values(5)
            integer :: i

            !  test real*4

            call ieee_set_flag(ieee_all,.false.)

            z4 = ieee_rem(x4, y4)
            i = int(z4)
            if (z4 .ne. 1.0) call zzrc(i)
            call ieee_get_flag(ieee_all,flag_values)
               do i = 1,4
                   if (flag_values(i) .neqv. .false.)call zzrc(20+i)
               enddo


         end subroutine bar1


         subroutine bar2(x8, y8, z8)

            use ieee_arithmetic
	    implicit none
            real*8 :: x8, y8, z8
            logical :: flag_values (5)
            integer :: i, j

            !  test real*8

           call ieee_set_flag(ieee_all,.false.)

            z8 = ieee_rem(x8, y8)
            j = int(z8)
            if (z8.ne. 2.0) call zzrc(j)
            call ieee_get_flag(ieee_all,flag_values)
               do i = 1,4
                   if (flag_values(i) .neqv. .false.)call zzrc(30+i)
               enddo
         end subroutine bar2

      end module modus


      program rem_module

      use modus
      use ieee_arithmetic
      implicit none

      real*4 :: a1, a2, resa
      real*8 :: b1, b2, resb

      a1 = 10.0
      a2 = 3.0
      b1 = 10.0_8
      b2 = 4.0_8

      call bar1(a1, a2, resa)

      call bar2(b1, b2, resb)

      end
