!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfixed
! %GROUP: mxmnch23.f 
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : mxmnch23
!*
!*  PROGRAMMER                 : John Zang
!*  DATE                       : Oct. 20, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Support character argument for MAX/
!*                               MIN/MAXVAL/MINVAL/MAXLOC/MINLOC
!*  SECONDARY FUNCTIONS TESTED : Functional test
!*
!*  DRIVER STANZA              : xlf90
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
      character(10) aa(2,3,4)
      character(10), allocatable :: xx(:,:)

      aa = '!'
      aa(2,1,3) = 'ibm'
      aa(1,2,4) = 'usa'
      aa(1,2,2) = 'can'
      aa(1,3,1) = 'bel'
      aa(2,2,1) = 'jan'
      aa(2,3,4) = 'gb'
      
      if (maxval(aa(:,:,2:3)) /= 'ibm') error stop 1
      if (maxval(aa(:,1:2,:)) /= 'usa') error stop 2
      if (maxval(aa(2:2,:,:)) /= 'jan') error stop 3

      allocate(xx(3,2))
      xx = maxval(aa(:,:,2:3), dim=1,mask=aa(:,:,2:3) .ne. 'x')
      if (xx(1,1) /= '!') error stop 4
      if (xx(2,1) /= 'can') error stop 5
      if (xx(3,1) /= '!') error stop 6
      if (xx(1,2) /= 'ibm') error stop 7
      if (xx(2,2) /= '!') error stop 8
      if (xx(3,2) /= '!') error stop 9
      deallocate(xx)

      allocate(xx(2,4))
      xx = maxval(aa(:,1:2,:), dim=2,mask=aa(:,1:2,:) .ne. 'a')
      if (xx(1,1) /= '!') error stop 10
      if (xx(2,1) /= 'jan') error stop 11
      if (xx(1,2) /= 'can') error stop 12
      if (xx(2,2) /= '!') error stop 13
      if (xx(1,3) /= '!') error stop 14
      if (xx(2,3) /= 'ibm') error stop 15
      if (xx(1,4) /= 'usa') error stop 152
      if (xx(2,4) /= '!') error stop 153
      deallocate(xx)

      allocate(xx(1,3))
      xx = maxval(aa(2:2,:,:),dim=3)
      if (xx(1,1) /= 'ibm') error stop 16
      if (xx(1,2) /= 'jan') error stop 17
      if (xx(1,3) /= 'gb') error stop 18
      deallocate(xx)

      aa = z'7f'
      aa(2,1,3) = 'ibm'
      aa(1,2,4) = 'usa'
      aa(1,2,2) = 'can'
      aa(1,3,1) = 'bel'
      aa(2,2,1) = 'jan'
      aa(2,3,4) = 'gb'

      if (minval(aa(:,2:3,1:4:2)) /= 'bel') error stop 19
      if (minval(aa(:,1:3:2,:)) /= 'bel') error stop 20
      if (minval(aa(1:1,:,:)) /= 'bel') error stop 21

      allocate(xx(3,2))
      xx = minval(aa(:,:,2:3), dim=1,mask=aa(:,:,2:3) .ne. 'x')
      if (xx(1,1) /= z'7f') error stop 22
      if (xx(2,1) /= 'can') error stop 23
      if (xx(3,1) /= z'7f') error stop 24
      if (xx(1,2) /= 'ibm') error stop 25
      if (xx(2,2) /= z'7f') error stop 26
      if (xx(3,2) /= z'7f') error stop 27
      deallocate(xx)

      allocate(xx(2,4))
      xx = minval(aa(:,1:2,:), dim=2,mask=aa(:,1:2,:) .ne. 'a')
      if (xx(1,1) /= z'7f') error stop 28
      if (xx(2,1) /= 'jan') error stop 29
      if (xx(1,2) /= 'can') error stop 30
      if (xx(2,2) /= z'7f') error stop 31
      if (xx(1,3) /= z'7f') error stop 32
      if (xx(2,3) /= 'ibm') error stop 33
      if (xx(1,4) /= 'usa') error stop 332
      if (xx(2,4) /= z'7f') error stop 333
      deallocate(xx)

      allocate(xx(1,3))
      xx = minval(aa(2:2,:,:),dim=3)
      if (xx(1,1) /= 'ibm') error stop 34
      if (xx(1,2) /= 'jan') error stop 35
      if (xx(1,3) /= 'gb') error stop 36
      deallocate(xx)
      end
