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
      type dt
        character(10) aa(2,3,4)
      end type
      type(dt) adt
      integer, allocatable :: yy(:)
      integer, allocatable :: xx(:,:)
      integer l(3), h(3), s(3)

      l(3) = 2
      h(3) = 3
      s(3) = 1

      adt%aa = '!'
      adt%aa(2,1,3) = 'ibm'
      adt%aa(1,2,4) = 'usa'
      adt%aa(1,2,2) = 'can'
      adt%aa(1,3,1) = 'bel'
      adt%aa(2,2,1) = 'jan'
      adt%aa(2,3,4) = 'gb'

      allocate(yy(3))
      yy = maxloc(adt%aa(:,:,l(3):h(3):s(3)))
      if (yy(1) /= 2) error stop 1
      if (yy(2) /= 1) error stop 2
      if (yy(3) /= 2) error stop 3
      deallocate(yy)

      allocate(xx(3,2))
      xx=maxloc(adt%aa(:,:,l(3):h(3):s(3)),
     + dim=1,mask=adt%aa(:,:,l(3):h(3):s(3)) .ne. 'x')
      if (xx(1,1) /= 1) error stop 4
      if (xx(2,1) /= 1) error stop 5
      if (xx(3,1) /= 1) error stop 6
      if (xx(1,2) /= 2) error stop 7
      if (xx(2,2) /= 1) error stop 8
      if (xx(3,2) /= 1) error stop 9
      deallocate(xx)

      l(2) = 1
      h(2) = 2
      s(2) = 1

      allocate(xx(2,4))
      xx=maxloc(adt%aa(:,l(2):h(2):s(2),:),
     + dim=2,mask=adt%aa(:,l(2):h(2):s(2),:) .ne. 'a')
      if (xx(1,1) /= 1) error stop 10
      if (xx(2,1) /= 2) error stop 11
      if (xx(1,2) /= 2) error stop 12
      if (xx(2,2) /= 1) error stop 13
      if (xx(1,3) /= 1) error stop 14
      if (xx(2,3) /= 1) error stop 15
      if (xx(1,4) /= 2) error stop 16
      if (xx(2,4) /= 1) error stop 17
      deallocate(xx)

      l(1) = 2
      h(1) = 2
      s(1) = 1

      allocate(xx(1,3))
      xx = maxloc(adt%aa(l(1):h(1):s(1),:,:),dim=3)
      if (xx(1,1) /= 3) error stop 18
      if (xx(1,2) /= 1) error stop 19
      if (xx(1,3) /= 4) error stop 20
      deallocate(xx)

      end