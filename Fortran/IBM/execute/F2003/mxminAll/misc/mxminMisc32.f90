!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 2/25/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX*/MIN* intrinsics 
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*
!*  DESCRIPTION                : MAX*/MIN* as actual argument passed to
!*                               internal procedure with variable length dummy
!*                               argument. Actual argument length is around 
!*                               32K long.
!* ===================================================================

program mxminMisc32

  integer x(2)

  character*(2**15-1) c1, c1Arr(2,3)
  character*(2**14-1) c2, c2Arr(10)

  c1 = repeat("a", 2**15-1)
  c2 = repeat("b", 2**14-1)

  c1Arr = repeat("a", 2**15-1)
  c1Arr(1,2) = repeat("c", 2**15-1)
  c2Arr = repeat("d", 2**14-1)
  c2Arr(7) = repeat("z", 2**15-1)

  if(len(max(c1, c2)) .ne. 32767) error stop 1_4
   
  if(min(c1, c2)(3456:3459) .ne. "aaaa") error stop 2_4

  x = maxloc(c1Arr)
  if(x(1) .ne. 1 .or. x(2) .ne. 2) error stop 3_4

  x = minloc(c1Arr, dim=2, mask=.true.)
  if(x(1) .ne. 1 .or. x(2) .ne. 1) error stop 4_4

  if(maxval(c2Arr)(100:105) .ne. "zzzzzz") error stop 5_4

  call submax(max(c1, c2))

  call subminval(maxval(c2Arr))

  contains
     subroutine submax(ch)
      character*(*) ch
      if (ch(12000:12003) .ne. 'bbbb') error stop 6_4 
     end subroutine

     subroutine subminval(ch)
      character*(*) :: ch
      if(ch(12005:12010) .ne. "zzzzzz") error stop 7_4

     end subroutine

end program mxminMisc32



