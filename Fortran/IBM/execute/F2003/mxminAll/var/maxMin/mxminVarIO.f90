!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 1/15/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics 
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*
!*  DESCRIPTION                : MAX/MIN with variable(different length)
!*                               as object in IO.
!* ===================================================================

  program mxminVarIO 

     character*3 :: charArr1(2,3)
     character*8 :: charArr2(2,3)
     character*8 :: charArr3(2,3)

     integer io_stat

     character*50 CHARVAR

     charArr1 = 'abc'
     charArr2 = 'bca'
     charArr3 = "zzzzaaaa"

     CHARVAR = "(6A8, 6A8, 6A8)"

     io_stat = -1
     write (unit=4, fmt = CHARVAR, iostat=io_stat) max(charArr1, charArr2, charArr1), min(charArr1, charArr2) , max(charArr1, charArr2, maxval(charArr3))

     if (io_stat .ne. 0) then
        error stop 1_4
     end if

     io_stat = -1
     close (unit=4, iostat=io_stat)
     if (io_stat .ne. 0) then
        error stop 2_4
     end if

  end program mxminVarIO 

