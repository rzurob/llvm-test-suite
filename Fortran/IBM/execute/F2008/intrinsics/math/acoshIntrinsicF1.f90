!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : acoshIntrinsicF1.f
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Shahid Alam
!*  DATE                       : 2010-11-16
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Math Intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : ACOSH
!*
!*  REFERENCE                  : Feature Number 376003
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  This program tests the ACOSH(x) intrinsic procedure
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module trig

   contains

   subroutine acoshRS(r, res)
      real(4), intent(in) :: r
      real(4) res
      res = acosh(r)
      return
   end subroutine acoshRS

   real function acoshRF(r)
      real(4), intent(in) :: r
      acoshRF = acosh(r)
      return
   end function acoshRF

   subroutine acoshCS(c, res)
      complex(4), intent(in) :: c
      complex(4) res
      res = acosh(c)
      return
   end subroutine acoshCS

   complex function acoshCF(c)
      complex(4), intent(in) :: c
      acoshCF = acosh(c)
      return
   end function acoshCF

end module trig

program main

   use trig

   implicit none

   integer i
   complex(4), parameter :: init_value = acosh((0.75,0.5))
   real(4) RES_R
   complex(4) RES_C
   real(4), dimension(7) :: R, R_R
   complex(4), dimension(7) :: C, R_C

   interface
      logical(4) function precision_r4(a, b)
         real(4) a, b
      end function
      logical(4) function precision_x8(a, b)
        complex(4) a, b
      end function
   end interface

   C = (/ (1.0,0.5),   (0.75,0.3),   (0.5,0.2),    (0.0,0.1), (-0.5,0.6),    (-0.75,0.7),  (-1.0,0.9) /)
   R_C = (/ (0.7328577042,0.6748888493), (0.4047130644,0.8058367968), (0.2270882726,1.061710715), (0.9983407706E-01,1.570796371), (0.6197743416,2.001226902), (0.7731347680,2.178217888), (1.002976656,2.273896456) /)
   R = (/ 10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0 /)
   R_R = (/ 2.993222952, 3.688253880, 4.094066620, 4.381870270, 4.605070114, 4.787422180, 4.941591263 /)

   do i = 1, 7
      RES_C = acosh(C(i))
      !
      ! Check if the value computed is right for complex number
      !
      if (.NOT. precision_x8(RES_C, R_C(i))) then
         call zzrc(i+1)
      else
         !
         ! Check if the value computed is right for real number
         !
         RES_R = acosh(R(i))
         if (.NOT. precision_r4(RES_R, R_R(i))) then
            call zzrc(i+2)
         !
         ! Compute the cosh and check if its equal to the real number
         !
         else if (.NOT. precision_r4(cosh(RES_R), R(i))) then
            call zzrc(i+3)
         end if
      end if
   end do

   call acoshRS(R(4), RES_R)
   if (.NOT. precision_r4(RES_R, R_R(4))) then
      call zzrc(i+3)  ! 11
   end if
   call acoshCS(C(4), RES_C)
   if (.NOT. precision_x8(RES_C, R_C(4))) then
      call zzrc(i+4)  ! 12
   end if
   if (.NOT. precision_r4(acoshRF(R(4)), R_R(4))) then
      call zzrc(i+5)  ! 13
   else if (.NOT. precision_x8(acoshCF(C(4)), R_C(4))) then
      call zzrc(i+6)  ! 14
   end if

   if (acosh((0.75,0.5)) .NE. init_value) then
      call zzrc(i+7)
   end if

end program main
