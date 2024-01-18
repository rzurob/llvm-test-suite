! *********************************************************************
!*
!*  ===================================================================
!*
!*  DIAGNOSES TESTED           : Cannot access an intrinsic module and
!*                               a nonintrinsic module with the same
!*                               name in the same scope unit.
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

        module ieee_arithmetic
          character*40 :: c1='This is a fake ieee_arithmetic module.'
          contains
            subroutine ieee_set_rounding_mode()
              print *, c1
            end subroutine
        end module ieee_arithmetic


        program intrimod02a
          use, non_intrinsic :: ieee_arithmetic
          call sub1()
          contains
            subroutine sub1()
              use, intrinsic :: ieee_arithmetic
              real*4 yr
            end subroutine sub1
            subroutine sub2()
              use, non_intrinsic :: ieee_arithmetic
              integer i
            end subroutine
        end program
