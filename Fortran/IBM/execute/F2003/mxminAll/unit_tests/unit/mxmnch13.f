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
      character(5), allocatable :: aa(:,:), xx(:,:)
      character(5) :: bb(2,2)
      allocate(aa(2,2),xx(2,2))
      aa = '!'
      bb = '!'
      aa(2,1) = 'abc'
      bb(1,2) = 'xyz'
      xx = max(aa, bb, reshape((/'a','b','c','d'/), (/2,2/)))
      if (xx(1,1) /= 'a') error stop 1
      if (xx(2,1) /= 'b') error stop 2
      if (xx(1,2) /= 'xyz') error stop 3
      if (xx(2,2) /= 'd') error stop 4
      deallocate(aa,xx)
      end
