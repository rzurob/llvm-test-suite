!*  ===================================================================
!*
!*  DATE                       : 1/15/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAX/MIN with Hollerith constant as argument
!*                               to sub with null character appened
!*                               using -qnullterm.
!* ===================================================================
@process nullterm

  program mxminHollerithNullterm

      character*1 :: x, y
      character*4 :: x1, y1
      x1 = "abcd"
      y1 = "dcba"
      x="a"
      y="z"

      call sub(1,max("a", 1Hz), min(1Ha, "z"),' ')
      call sub(2,1Ha//1Hb,1Hb//1Hc,'  ')

  end program mxminHollerithNullterm

@process nullterm
  subroutine sub(arg1,arg2,arg3,arg4)

      integer*4 arg1
      character*(*) arg2, arg3,arg4
      character*1 c
      pointer (p,c)
      integer count

      if (arg1 /=len(arg2)) error stop 1_4
      if (arg1 /=len(arg3)) error stop 2_4
      if (arg1 /=len(arg4)) error stop 3_4

      p = loc(arg2)
      do count = 1, arg1
        if (c==char(0)) error stop 4_4
        p = p + 1
      end do
      if (c /= char(0)) error stop 5_4

      p = loc(arg3)
      do count = 1, arg1
        if (c==char(0)) error stop 6_4
        p = p + 1
      end do
      if (c /= char(0)) error stop 7_4

      p = loc(arg4)
      do count = 1, arg1
        if (c==char(0)) error stop 8_4
        p = p + 1
      end do
      if (c /= char(0)) error stop 9_4

  end subroutine
