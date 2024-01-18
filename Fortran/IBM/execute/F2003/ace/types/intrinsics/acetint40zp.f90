!******************************************************************************
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetint40zp
!*
!*  DATE                       : 2006-10-16
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : print complex A.C. (value test)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : intrinsic type, assignment, array constructor
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Verify that the values in a complex array constructor are correctly treated
!*  in a print statement.  All the values within the constructor should be
!*  converted to match the type specifier before the constructor is used to
!*  initialise the variable, whether widening or narrowing, so:
!*    print *, (/ complex*8:: <z*4value>, <z*8value>, <z*16value> /)
!*  (e.g., print *, (/ complex*8:: (1.1_4, 5.5_4), (3.3_8, -4.1_8), (7.7_16, 9.9_16)/))
!*  should print only complex*8 values
!*  (so the output should be " (1.100000024,5.500000000) (3.299999952,-4.099999905) (7.699999809,9.899999619)"
!*
!*  Overall, this test case is like acetint04, but printing instead of assigning
!*  and complex instead of integer.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint40zp

  implicit none
  integer :: i, errorCount
  complex(4) :: zarr4(12)
  complex(8) :: zarr8(12)
  complex(16):: zarr16(12)
  complex    :: zarr (12)
  double complex :: dzarr(12)

  complex(4),  parameter :: VAL1_4  = (1.1_4,1.1_4), VAL2_4 = (tiny(0.0_4),huge(0.0_4)), VAL3_4 = (huge(0.0_4),tiny(0.0_4))
  complex(8),  parameter :: VAL1_8  = (1.1_8,1.1_8), VAL2_8 = (tiny(0.0_8),huge(0.0_8)), VAL3_8 = (huge(0.0_8),tiny(0.0_8))
  complex(16), parameter :: VAL1_16  = (1.1_16,1.1_16), VAL2_16 = (tiny(0.0_16),huge(0.0_16)), VAL3_16 = (huge(0.0_16),tiny(0.0_16))
  integer, parameter  :: DP = kind(dzarr)
  double complex, parameter :: VAL1_DP  = (1.1_DP,1.1_DP), VAL2_DP = (tiny(0.0_DP),huge(0.0_DP)), VAL3_DP = (huge(0.0_DP),tiny(0.0_DP))

  character (600) :: base, out1, out2, out3

  ! Make sure the values in each array are correct
  !   - assigning via an array constructor makes for a circular test.
  zarr4(1) = VAL1_4; zarr4(2) = VAL2_4; zarr4(3) = VAL3_4;
  zarr4(4) = VAL1_8; zarr4(5) = VAL2_8; zarr4(6) = VAL3_8;
  zarr4(7) = VAL1_16; zarr4(8) = VAL2_16; zarr4(9) = VAL3_16;
  zarr4(10) = VAL1_DP; zarr4(11) = VAL2_DP; zarr4(12) = VAL3_DP

  zarr8(1) = VAL1_4; zarr8(2) = VAL2_4; zarr8(3) = VAL3_4;
  zarr8(4) = VAL1_8; zarr8(5) = VAL2_8; zarr8(6) = VAL3_8;
  zarr8(7) = VAL1_16; zarr8(8) = VAL2_16; zarr8(9) = VAL3_16;
  zarr8(10) = VAL1_DP; zarr8(11) = VAL2_DP; zarr8(12) = VAL3_DP

  zarr16(1) = VAL1_4; zarr16(2) = VAL2_4; zarr16(3) = VAL3_4;
  zarr16(4) = VAL1_8; zarr16(5) = VAL2_8; zarr16(6) = VAL3_8;
  zarr16(7) = VAL1_16; zarr16(8) = VAL2_16; zarr16(9) = VAL3_16;
  zarr16(10) = VAL1_DP; zarr16(11) = VAL2_DP; zarr16(12) = VAL3_DP

  zarr(1) = VAL1_4; zarr(2) = VAL2_4; zarr(3) = VAL3_4;
  zarr(4) = VAL1_8; zarr(5) = VAL2_8; zarr(6) = VAL3_8;
  zarr(7) = VAL1_16; zarr(8) = VAL2_16; zarr(9) = VAL3_16;
  zarr(10) = VAL1_DP; zarr(11) = VAL2_DP; zarr(12) = VAL3_DP

  dzarr(1) = VAL1_4; dzarr(2) = VAL2_4; dzarr(3) = VAL3_4;
  dzarr(4) = VAL1_8; dzarr(5) = VAL2_8; dzarr(6) = VAL3_8;
  dzarr(7) = VAL1_16; dzarr(8) = VAL2_16; dzarr(9) = VAL3_16;
  dzarr(10) = VAL1_DP; dzarr(11) = VAL2_DP; dzarr(12) = VAL3_DP


  errorCount = 0

  write(base,'(12("(",g22.14,",",g22.14,")":" "))') zarr4
  write(out1,'(12("(",g22.14,",",g22.14,")":" "))') &
       (/complex(4):: VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP/)
  write(out2,'(12("(",g22.14,",",g22.14,")":" "))') &
        (/complex(4):: (VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP, i=1,1)/)
  write(out3,'(12("(",g22.14,",",g22.14,")":" "))') &
        (/complex(4):: (VAL1_4,i=1,1), (VAL2_4,i=1,1), (VAL3_4,i=1,1), (VAL1_8,i=1,1), (VAL2_8,i=1,1), (VAL3_8,i=1,1), &
                       (VAL1_16,i=1,1), (VAL2_16,i=1,1), (VAL3_16,i=1,1), (VAL1_DP,i=1,1), (VAL2_DP,i=1,1), (VAL3_DP,i=1,1)/)
  call check(4)

  write(base,'(12("(",g22.14,",",g22.14,")":" "))') zarr8
  write(out1,'(12("(",g22.14,",",g22.14,")":" "))') &
        (/complex(8):: VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP/)
  write(out2,'(12("(",g22.14,",",g22.14,")":" "))') &
        (/complex(8):: (VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP, i=1,1)/)
  write(out3,'(12("(",g22.14,",",g22.14,")":" "))') &
        (/complex(8):: (VAL1_4,i=1,1), (VAL2_4,i=1,1), (VAL3_4,i=1,1), (VAL1_8,i=1,1), (VAL2_8,i=1,1), (VAL3_8,i=1,1), &
                       (VAL1_16,i=1,1), (VAL2_16,i=1,1), (VAL3_16,i=1,1), (VAL1_DP,i=1,1), (VAL2_DP,i=1,1), (VAL3_DP,i=1,1)/)
  call check(8)

  write(base,'(12("(",g22.14,",",g22.14,")":" "))') zarr16
  write(out1,'(12("(",g22.14,",",g22.14,")":" "))') &
        (/complex(16):: VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP/)
  write(out2,'(12("(",g22.14,",",g22.14,")":" "))') &
        (/complex(16):: (VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP, i=1,1)/)
  write(out3,'(12("(",g22.14,",",g22.14,")":" "))') &
        (/complex(16):: (VAL1_4,i=1,1), (VAL2_4,i=1,1), (VAL3_4,i=1,1), (VAL1_8,i=1,1), (VAL2_8,i=1,1), (VAL3_8,i=1,1), &
                        (VAL1_16,i=1,1), (VAL2_16,i=1,1), (VAL3_16,i=1,1), (VAL1_DP,i=1,1), (VAL2_DP,i=1,1), (VAL3_DP,i=1,1)/)
  call check(16)

  write(base,'(12("(",g22.14,",",g22.14,")":" "))') zarr
  write(out1,'(12("(",g22.14,",",g22.14,")":" "))') &
        (/complex:: VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP/)
  write(out2,'(12("(",g22.14,",",g22.14,")":" "))') &
        (/complex:: (VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP, i=1,1)/)
  write(out3,'(12("(",g22.14,",",g22.14,")":" "))') &
        (/complex:: (VAL1_4,i=1,1), (VAL2_4,i=1,1), (VAL3_4,i=1,1), (VAL1_8,i=1,1), (VAL2_8,i=1,1), (VAL3_8,i=1,1), &
                    (VAL1_16,i=1,1), (VAL2_16,i=1,1), (VAL3_16,i=1,1), (VAL1_DP,i=1,1), (VAL2_DP,i=1,1), (VAL3_DP,i=1,1)/)
  call check(0) ! i.e., the default

  write(base,'(12("(",g22.14,",",g22.14,")":" "))') dzarr
  write(out1,'(12("(",g22.14,",",g22.14,")":" "))') &
        (/double complex:: VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP/)
  write(out2,'(12("(",g22.14,",",g22.14,")":" "))') &
        (/double complex:: (VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP, i=1,1)/)
  write(out3,'(12("(",g22.14,",",g22.14,")":" "))') &
        (/double complex:: (VAL1_4,i=1,1), (VAL2_4,i=1,1), (VAL3_4,i=1,1), (VAL1_8,i=1,1), (VAL2_8,i=1,1), (VAL3_8,i=1,1), &
                            (VAL1_16,i=1,1), (VAL2_16,i=1,1), (VAL3_16,i=1,1), (VAL1_DP,i=1,1), (VAL2_DP,i=1,1), (VAL3_DP,i=1,1)/)
  call check(2) ! i.e., double complex

  if (errorCount > 0) then
     print *, "Error count:", errorCount
     stop 2
  end if

contains

  subroutine check(k)
    integer :: k
    if (base /= out1 .or. base /= out2 .or. base /= out3) then
       print *, "Problem with output for kind =", k, "; base /= out[123]:"
       print *, "base: [", trim(base), "]"
       print *, "out1: [", trim(out1), "]"
       print *, "out2: [", trim(out2), "]"
       print *, "out3: [", trim(out3), "]"
       errorCount = errorCount + 1
    end if
  end subroutine check

end program acetint40zp
