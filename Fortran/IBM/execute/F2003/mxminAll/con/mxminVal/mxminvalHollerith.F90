!*  ===================================================================
!*
!*  DATE                       : 2/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX*/MIN* intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAXVAL/MINVAL Hollerith constant as its
!*                               argument.
!*  (315974)
!* ===================================================================

  module modValHollerith
     contains
        subroutine sub2(arg)
           character(*):: arg
           if(arg .ne. "gh") error stop 6_4
        end subroutine
  end module modValHollerith

program mxminvalHollerith

   use modValHollerith

   character*2 v(3)

   if(maxval(reshape((/1Ha//1Hb, 1Hc//1Hd, 1He//1Hf, 1Hg//1Hh, 1Hx//1Hy, 1Hr//1Hs/), (/2,3/))) .ne. "xy") error stop 1_4

   if(minval(reshape((/1Ha//1Hb, 1Hc//1Hd, 1He//1Hf, 1Hg//1Hh, 1Hx//1Hy, 1Hr//1Hs/), (/2,3/))) .ne. "ab") error stop 2_4

#if __BIG_ENDIAN__
  v = minval(reshape((/1Ha//1Hb, 1Hc//1Hd, 1He//1Hf, 1Hg//1Hh, 1Hx//1Hy, 1Hr//1Hs/), (/2,3/)),dim=max(1HA , 1HB)-1109401631, mask = .true.)
#else
  v = minval(reshape((/1Ha//1Hb, 1Hc//1Hd, 1He//1Hf, 1Hg//1Hh, 1Hx//1Hy, 1Hr//1Hs/), (/2,3/)),dim=reverse_byte_order(max(1HA , 1HB))-1109401631, mask = .true.)
#endif

  if(v(1) .ne. "ab" .or. v(2) .ne. "ef" .or. v(3) .ne. "rs") error stop 3_4

  if(maxval((/1Ha//1Hb, 1Hc//1Hd, 1He//1Hf, 1Hg//1Hh/)) .ne. "gh") then
        error stop 4_4
  endif

  if(maxval((/1Ha//1Hb, max(1Hc//1Hd, 1Ha//1Ha), 1He//1Hf, 1Hg//1Hh/)) .ne. "gh") then
        error stop 5_4
  endif

   call sub2(maxval((/1Ha//1Hb, max(1Hc//1Hd, 1Ha//1Ha), 1He//1Hf, 1Hg//1Hh/)))

end program mxminvalHollerith

