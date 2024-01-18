!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2006-11-17
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : ac-do-variable name choice
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ac-do-variable, scope
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Create AC-implied do's with some variables named the same as other features
!*  of FORTRAN.  One caveat: we cannot use as variable names the names of
!*  intrinsics which we want to use, so
!*    print *, [integer:: (int(int),int=1,2)]
!*  and
!*    intvar = int(1)
!*    print *, [integer:: (int,int=1,2)]
!*  are not legal FORTRAN.  Leaving out the intrinsic makes it legal, though, so
!*    print *, [integer:: (int,int=1,2)]
!*  and even
!*    print *, [integer:: (integer,integer=1,2)]
!*  are legal.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module acetint53mod

  implicit integer (a-z) ! Normally, we want "none", but this test requires implicit types

contains

  function testmore()
    integer :: testmore(6)
    print *, [real(4)::    (real*2.0, real=0,3)]
    print *, [character::  (char(cmplx), cmplx=65,90)]

    logical = 0
    integer = 0
    print *, logical, integer
    do integer=9,10
       do int=11,11
          do logical=13,12,-1
             do cmplx=14,14
                print *, logical, integer, cmplx
                ! This use of cmplx is okay, since it's not used as an intrinsic in this module:
                print *, [character::  (char(cmplx), cmplx=65,90)]
             end do
          end do
       end do
    end do
    testmore = [integer:: ((logical+integer, logical=1,3),integer=1,2)]
    logical = -3
    integer = -4
  end function testmore

end module acetint53mod

program acetint53

  use acetint53mod
  implicit integer (a-z) ! Normally, we want "none", but this test requires implicit types
  integer :: array(6)

  logical = -1
  integer = -2

  print *, [integer::    (integer,integer=1,2)]
  print *, [logical(4):: (logical==2,logical=0,3)]
  print *, [real(4)::    (real(integer*2,4),integer=0,3)]
  print *, [integer::    ((logical,integer,logical=1,3),integer=1,2)]
  print *, [complex::    (cmplx(logical*2.0,logical+1.0), logical=1,2)]

  print *, logical, integer
  do integer=6,8
     do logical=4,5
        print *, logical, integer
        print *, [integer::    ((logical, integer, logical=1,3),integer=1,2)]
        print *, [logical::    ((logical == integer, integer=1,3),logical=1,2)]
        array = testmore()
     end do
  end do

  if (any([integer:: logical, integer] < 0)) stop 2

end program acetint53
