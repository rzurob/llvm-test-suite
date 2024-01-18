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
      character(10) aa(500000), bb(100,100)

      aa = '!'
      aa(283737) = 'maxval'
      if (maxval(aa) /= 'maxval') error stop 1

      aa = z'7f'
      aa(284363) = 'minval'
      if (minval(aa) /= 'minval') error stop 2

      bb = '!'
      bb(87, 34) = '8734'
      bb(3, 92) = '0392'
      if (maxval(bb) /= '8734') error stop 3

      bb = z'7f'
      bb(87, 34) = '8734'
      bb(3, 92) = '0392'
      if (minval(bb) /= '0392') error stop 4

      if (len(maxval(aa(0:-1))) /= 10) error stop 5
      if (len(minval(aa(0:-1))) /= 10) error stop 6

      do i = 1, 10
        if (maxval(aa(0:-1))(i:i) /= '\0') call zzrc(7+i)
        if (minval(aa(0:-1))(i:i) /= z'7f') call zzrc(8+i)
        if (maxval(bb, mask=bb .eq. 'abcde')(i:i) /= '\0')
     +    call zzrc(9+i)
        if (minval(bb, mask=bb .eq. 'abcde')(i:i) /= z'7f')
     +    call zzrc(10+i)
      enddo
      end
