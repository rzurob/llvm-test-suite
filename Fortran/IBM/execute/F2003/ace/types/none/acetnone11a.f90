!******************************************************************************
!*  ===========================================================================
!*  XL Fortran Test Case                                  IBM INTERNAL USE ONLY
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetnone11a
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-08-10
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : automatic arrays and sections as content (real)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : automatic, array section
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  Create an automatic array and manipulate it: print out the contents, assign
!*  to it, assign from it, and test it, using array constructors and array
!*  sections.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetnone11a

  use ieee_arithmetic
  implicit none

  call autotest(0)
  call autotest(1)
  call autotest(2)
  call autotest(10)
  call autotest(10000000)

contains

  subroutine autotest(n)
    integer :: n, i, ecount
    real(4) :: array(2*n+2), q, inf

    print *, "Testing ", n
    array(2:n+1) = (/ (1.0/i,i=1,n) /)
    array(n+2:2*n+1) = [ (array(n-i+1),i=0,n-1) ]
    array(1:2*n+2:2*n+1) = (/ ieee_value(0.0, ieee_quiet_nan), ieee_value(0.0, ieee_positive_inf) /)
    ecount = 0
    q = ieee_value(0.0, ieee_quiet_nan)
    inf = ieee_value(0.0, ieee_positive_inf)

    ! Test the contents -- these can be exact:
    if (array(1) == array(1) &
         .or. array(2*n+2) /= ieee_value(0.0, ieee_positive_inf)) then
       ecount = ecount + 1
       print *, 'Should be NaN(Q)/Inf: ', array(1), array(2*n+2)
    end if

    ! These may need to be approximate:
    do i=1,n
       if (.not.isSameReal4(array(i+1),1.0/i) .or. .not.isSameReal4(array(2*n+2-i),1.0/i)) then
          ecount = ecount + 1
          if (ecount < 10) print *, "at", i, array(i+1), array(2*n+2-i)
       end if
    end do

    if ( n > 1 .and. .not.all(isSameReal4(array((/ 2,n,n+1,n+2,2*n,2*n+1,2*n+2 /)),[1.0,1.0/(n-1),1.0/n,1.0/n,0.5,1.0,inf])) ) then
       ecount = ecount + 1
       print *, 'indexing: ', array([ 1,2,n,n+1,n+2,2*n,2*n+1,2*n+2 ])
    end if

    if (ecount > 0) then
       print *, "Total of", ecount, "errors for n=", n
       stop 2
    end if

    print *, array((/1,2,n+1,2*n+2/))
    print *, array([2*n+2,n+1,2,1])

    if (n > 1) then
       array = -2
       array((/1,2/)) = -3
       array([n+1,2*n+2]) = -3
       print *, array((/ (i,i+1,i=1,2*n+2,n) /)) ! -3, -3, -3, -2, -2, -3
    end if

  end subroutine autotest

  ! Adapted from our standard precision_R4 to be elemental, and to handle NaN and Inf:
  elemental logical function isSameReal4(value,expected)
    real(4), intent(in) :: value, expected
    real(4) :: high, low, delta

    ! If they're both extreme - both NaN or Infinite with the same sign - return true
    if (ieee_is_nan(value) .and. ieee_is_nan(expected) .or. (value == expected)) then
       isSameReal4 = .true.
    else
       delta = expected * 0.00001
       high = delta + expected
       low = expected - delta
       ! This is still not perfect: we don't handle the range near Inf well:
       if (expected < 0.0E0) then
          isSameReal4 = ((value >= high) .and. (value <= low))
       else
          isSameReal4 = ((value <= high) .and. (value >= low))
       end if
    end if

  end function isSameReal4

end program acetnone11a
