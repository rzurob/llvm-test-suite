!******************************************************************************
!*  ===========================================================================
!*  XL Fortran Test Case                                  IBM INTERNAL USE ONLY
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetint40zc
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-10-16
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : call subroutine with complex A.C. (value test)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : intrinsic type, assignment, array constructor
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  Verify that the values in an complex array constructor are correctly treated
!*  in a subroutine call.  All the values within the constructor should be
!*  converted to match the type specifier before the constructor is used to
!*  initialise the variable, whether widening or narrowing, so:
!*    call sub((/ complex*2:: <z*4value>, <z*8value>, <z*16value> /)
!*  (e.g., call sub((/ complex*8:: (1.1_4, 5.5_4), (3.3_8, -4.1_8), (7.7_16, 9.9_16)/))
!*  should pass in only complex*8 values
!*  (i.e., (/(1.1_4, 5.5_4), (3.3_8, -4.1_8), (7.7_8, 9.9_8)/)
!*  --- the first is actually (1.1_4, 5.5_4) stretched to complex*8, but without
!*  the precision given by *8).
!*
!*  Overall, this test case is like acetint04, but calling a subroutine instead of
!*  assigning, and complex instead of integer.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint40zc

  implicit none
  integer    :: i, errorCount
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


  call check4(1, (/complex(4):: VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, &
                                VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP/))
  call check4(2, (/complex(4):: (VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, &
                                 VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP, i=1,1)/))
  call check4(3, (/complex(4):: (VAL1_4,i=1,1), (VAL2_4,i=1,1), (VAL3_4,i=1,1), &
                                (VAL1_8,i=1,1), (VAL2_8,i=1,1), (VAL3_8,i=1,1), &
                                (VAL1_16,i=1,1), (VAL2_16,i=1,1), (VAL3_16,i=1,1), &
                                (VAL1_DP,i=1,1), (VAL2_DP,i=1,1), (VAL3_DP,i=1,1)/))

  call check8(4, (/complex(8):: VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, &
                                VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP/))
  call check8(5, (/complex(8):: (VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, &
                                 VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP, i=1,1)/))
  call check8(6, (/complex(8):: (VAL1_4,i=1,1), (VAL2_4,i=1,1), (VAL3_4,i=1,1), &
                                (VAL1_8,i=1,1), (VAL2_8,i=1,1), (VAL3_8,i=1,1), &
                                (VAL1_16,i=1,1), (VAL2_16,i=1,1), (VAL3_16,i=1,1), &
                                (VAL1_DP,i=1,1), (VAL2_DP,i=1,1), (VAL3_DP,i=1,1)/))

  call check16(7, (/complex(16):: VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, &
                                  VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP/))
  call check16(8, (/complex(16):: (VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, &
                                   VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP, i=1,1)/))
  call check16(9, (/complex(16):: (VAL1_4,i=1,1), (VAL2_4,i=1,1), (VAL3_4,i=1,1), &
                                  (VAL1_8,i=1,1), (VAL2_8,i=1,1), (VAL3_8,i=1,1), &
                                  (VAL1_16,i=1,1), (VAL2_16,i=1,1), (VAL3_16,i=1,1), &
                                  (VAL1_DP,i=1,1), (VAL2_DP,i=1,1), (VAL3_DP,i=1,1)/))

  call check(10, (/complex:: VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, &
                             VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP/))
  call check(11, (/complex:: (VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, &
                              VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP, i=1,1)/))
  call check(12, (/complex:: (VAL1_4,i=1,1), (VAL2_4,i=1,1), (VAL3_4,i=1,1), &
                             (VAL1_8,i=1,1), (VAL2_8,i=1,1), (VAL3_8,i=1,1), &
                             (VAL1_16,i=1,1), (VAL2_16,i=1,1), (VAL3_16,i=1,1), &
                             (VAL1_DP,i=1,1), (VAL2_DP,i=1,1), (VAL3_DP,i=1,1)/))

  call checkdz(13, (/double complex:: VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, &
                                      VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP/))
  call checkdz(14, (/double complex:: (VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, &
                                       VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP, i=1,1)/))
  call checkdz(15, (/double complex:: (VAL1_4,i=1,1), (VAL2_4,i=1,1), (VAL3_4,i=1,1), &
                                      (VAL1_8,i=1,1), (VAL2_8,i=1,1), (VAL3_8,i=1,1), &
                                      (VAL1_16,i=1,1), (VAL2_16,i=1,1), (VAL3_16,i=1,1),&
                                      (VAL1_DP,i=1,1), (VAL2_DP,i=1,1), (VAL3_DP,i=1,1)/))

  if (errorCount > 0) then
     print *, "Error count:", errorCount
     stop 2
  end if

contains

  subroutine check4(test, arr)
    complex (4) :: arr(:)
    integer :: test
    if (any(zarr4 /= arr)) then
       print *, "Problem with test", test
       print *, "zarr4: [", zarr4, "]"
       print *, "arr: [", arr, "]"
       errorCount = errorCount + 1
    end if
  end subroutine check4

  subroutine check8(test, arr)
    complex (8) :: arr(:)
    integer :: test
    if (any(zarr8 /= arr)) then
       print *, "Problem with test", test
       print *, "zarr8: [", zarr8, "]"
       print *, "arr: [", arr, "]"
       errorCount = errorCount + 1
    end if
  end subroutine check8

  subroutine check16(test, arr)
    complex (16) :: arr(:)
    integer :: test
    if (any(zarr16 /= arr)) then
       print *, "Problem with test", test
       print *, "zarr16: [", zarr16, "]"
       print *, "arr: [", arr, "]"
       errorCount = errorCount + 1
    end if
  end subroutine check16

  subroutine check(test, arr)
    complex :: arr(:)
    integer :: test
    if (any(zarr /= arr)) then
       print *, "Problem with test", test
       print *, "zarr: [", zarr, "]"
       print *, "arr: [", arr, "]"
       errorCount = errorCount + 1
    end if
  end subroutine check

  subroutine checkdz(test, arr)
    double complex :: arr(:)
    integer :: test
    if (any(dzarr /= arr)) then
       print *, "Problem with test", test
       print *, "dzarr: [", dzarr, "]"
       print *, "arr: [", arr, "]"
       errorCount = errorCount + 1
    end if
  end subroutine checkdz

end program acetint40zc
