!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 2/05/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX*/MIN* intrinsics 
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*
!*  DESCRIPTION                : MAXLOC/MINLOC with Hollerith constant as its 
!*                               argument.
!* ===================================================================

program mxminlocHollerith 

   interface
        function func1(carg, n)
          integer, dimension(*) :: carg
          integer, dimension(n) :: func1
        end function
   end interface

   integer v(3)
   integer v1(2)

   v1 = maxloc(reshape((/1Ha//1Hb, 1Hc//1Hd, 1He//1Hf, 1Hg//1Hh, 1Hx//1Hy, 1Hr//1Hs/), (/2,3/)))

   if(v1(1) .ne. 1 .or. v1(2) .ne. 3) error stop 1_4

   v1 = minloc(reshape((/1Ha//1Hb, 1Hc//1Hd, 1He//1Hf, 1Hg//1Hh, 1Hx//1Hy, 1Hr//1Hs/), (/2,3/)))

   if(v1(1) .ne. 1 .or. v1(2) .ne. 1) error stop 2_4

#if __BIG_ENDIAN__
   v = minloc(reshape((/1Ha//1Hb, 1Hc//1Hd, 1He//1Hf, 1Hg//1Hh, 1Hx//1Hy, 1Hr//1Hs/), (/2,3/)),dim=max(1HA , 1HB)-1109401631, mask = .true.) 
#else
   v = minloc(reshape((/1Ha//1Hb, 1Hc//1Hd, 1He//1Hf, 1Hg//1Hh, 1Hx//1Hy, 1Hr//1Hs/), (/2,3/)),dim=REVERSE_BYTE_ORDER(max(1HA , 1HB))-1109401631, mask = .true.) 
#endif

  if(v(1) .ne. 1 .or. v(2) .ne.  1 .or. v(3) .ne. 2) error stop 3_4

  if(any(minloc((/1Ha//1Hb, 1Hc//1Hd, 1He//1Hf, 1Hg//1Hh/)) .ne.1 )) error stop 4_4

  if(maxloc((/1Ha//1Hb, max(1Hc//1Hd, 1Ha//1Ha), 1He//1Hf, 1Hg//1Hh/),dim=1) .ne. 4) error stop 5_4

  v1 = 999

  v1 = func1(maxloc(reshape((/1Ha//1Hb, 1Hc//1Hd, 1He//1Hf, 1Hg//1Hh, 1Hx//1Hy, 1Hr//1Hs/), (/2,3/))), 2)

   if(v1(1) .ne. 1 .or. v1(2) .ne. 3) error stop 6_4

end program mxminlocHollerith 

  function func1(carg, n)
       integer, dimension(*) :: carg
       integer, dimension(n) :: func1
       do i = 1, n
           func1(i) = carg(i)
       end do
  end function
