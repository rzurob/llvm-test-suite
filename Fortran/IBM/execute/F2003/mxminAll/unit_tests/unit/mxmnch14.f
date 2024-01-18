!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 20, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Support character argument for MAX/
!*                               MIN/MAXVAL/MINVAL/MAXLOC/MINLOC
!*  SECONDARY FUNCTIONS TESTED : Functional test
!*
!*  REQUIRED COMPILER OPTIONS  : -qfixed
!*
!*  DESCRIPTION                : MAX/MIN - Maximum or minimum value
!*                               according to their collating sequence
!*                               of ASCII characters.
!*                               MAXVAL/MINVAL - Maximum or minimum value
!*                               of elements in a character array.
!*                               MAXLOC/MINLOC - The location of maximum
!*                               or minimum value of elements in a character
!*                               array.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none
      integer i
      character(5), parameter :: aa(2,2) =
     + reshape((/'a','j','c','l'/), (/2,2/))
      character(5), parameter :: bb(2,4) =
     + reshape((/'e','f','g','h','i','b','k','d'/), (/2,4/))
      character(5), parameter :: cc(2,2) = max(aa, bb(:,3:4))
      character(5), parameter :: dd(2,2) = min(aa, bb(:,1:2))
      if (cc(1,1) /= 'i') error stop 1
      if (cc(2,1) /= 'j') error stop 2
      if (cc(1,2) /= 'k') error stop 3
      if (cc(2,2) /= 'l') error stop 4
      if (dd(1,1) /= 'a') error stop 5
      if (dd(2,1) /= 'f') error stop 6
      if (dd(1,2) /= 'c') error stop 7
      if (dd(2,2) /= 'h') error stop 8
      end
