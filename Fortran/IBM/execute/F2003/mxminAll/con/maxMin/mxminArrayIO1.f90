!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 1/05/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics 
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*
!*  DESCRIPTION                : MAX/MIN with named constant(different length)
!*                               as object in IO.
!* ===================================================================

  program mxminArrayIO1 

     character*3 :: charArr1(2,3)
     character*8 :: charArr2(2,3)
     integer io_stat

     character*50 CHARVAR

     parameter(charArr1 = 'abc')
     parameter(charArr2 = 'bca')

     CHARVAR = "(6A8, 6A8, 6A8)"

     io_stat = -1
     write (unit=4, fmt = CHARVAR, iostat=io_stat) max(charArr1, charArr2), &
          min(charArr1, charArr2) , max(charArr1, charArr2, "zzzzaaaa")

     if (io_stat .ne. 0) then
        error stop 1_4
     end if

     io_stat = -1
     close (unit=4, iostat=io_stat)
     if (io_stat .ne. 0) then
        error stop 2_4
     end if

  end program mxminArrayIO1 

