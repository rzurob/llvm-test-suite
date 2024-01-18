!******************************************************************************
!*  ===========================================================================
!*  XL Fortran Test Case                                  IBM INTERNAL USE ONLY
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetnone11
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-08-10
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : automatic arrays and sections as content
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

program acetnone11

  implicit none

  call autotest(0)
  call autotest(1)
  call autotest(2)
  call autotest(10)
  call autotest(10000000)
  
contains

  subroutine autotest(n)
    integer :: n, i, ecount
    integer :: array(2*n+2)

    print *, "Testing ", n
    array(2:n+1) = (/ (i,i=1,n) /)
    array(n+2:2*n+1) = [ (array(n-i+1),i=0,n-1) ]
    array(1:2*n+2:2*n+1) = (/ -1, -1 /)
    ecount = 0

    if (array(1) /= -1 .or. array(2*n+2) /= -1) then
       ecount = ecount + 1
       print *, 'Should be -1: ', array(1), array(2*n+2)
    end if
    do i=1,n
       if (array(i+1) /= i .or. array(2*n+2-i) /= i) then
          ecount = ecount + 1
          if (ecount < 10) print *, "at", i, array(i+1), array(2*n+2-i)
       end if
    end do

    if ( n > 1 .and. any(array((/ 1,2,n,n+1,n+2,2*n,2*n+1,2*n+2 /)) /= [-1,1,n-1,n,n,2,1,-1]) ) then
       ecount = ecount + 1
       print *, 'indexing: ', array([ 1,2,n,n+1,n+2,2*n,2*n+1,2*n+2 ])
    end if
    if (ecount > 0) then
       print *, "Total of", ecount, "errors for n=", n
       stop 2
    end if
    print *, array((/1,2,n+1,2*n+2/))
    print *, array((/2*n+2,n+1,2,1/))

    if (n > 1) then
       array = -2
       array((/1,2,n+1,2*n+2/)) = -3
       print *, array((/ (i,i=1,2*n+2,n) /)) ! -3, -3, -2
    end if

  end subroutine autotest

end program acetnone11
