!*  ===================================================================
!*
!*  DATE                       : 2/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :  TC to test max/min with Hollerith constant
!*                                without -qctyplss.
!* ===================================================================

  module modHollerith
     contains
        subroutine sub2(arg)
           character(*) arg
           if(arg .ne. "abcdefgh") error stop 6_4
        end subroutine
  end module modHollerith

  program mxminHollerith

     use modHollerith

     interface
        function func1(carg, n)
          character(*), dimension(*) :: carg
          character(n), dimension(n) :: func1
        end function
     end interface

     character*4 v(3)

     if(len(max(1Ha//1Hb, 8Habcdefgh)) .ne. 8) error stop 1_4

     if(min(1Ha//1Hb, 8Habcdefgh) .ne. "ab") error stop 2_4

     if(len(max(1Ha//1Hb, 8Habcdefgh, "z")) .ne. 8) error stop 3_4

     if(max(1Ha//1Hb, 8Habcdefgh, "z") .ne. "z       ") error stop 4_4

     if(len(min("a", 1Ha//1Hb, 8Habcdefgh)) .ne. 8) error stop 5_4

     call sub2(max(1Ha//1Hb, "aaa", 8Habcdefgh))

     v = func1(max((/1Ha//1Hb, 1Hc//1Hd, 1Hf//1Hg/), "bbbb"), 4)

     if(v(1) .ne. "bbbb" .or. v(2) .ne. "cd  " .or. v(3) .ne. "fg  ") then
          error stop 7_4
     endif

     if(max("aa", "bb")//min(1Hc//1Hu, 1Hd//1He) .ne. "bbcu") error stop 8_4

#if __BIG_ENDIAN__
     if(min(2Hab, 2Hcd) .ne. 1633820704) error stop 9_4
#else
     if(min(2Hab, 2Hcd) .ne. reverse_byte_order(1633820704)) error stop 9_4
#endif

  end program mxminHollerith

  function func1(carg, n)
       character(*), dimension(*) :: carg
       character(n), dimension(n) :: func1
       do i = 1, n
           func1(i) = carg(i)
       end do
  end function

