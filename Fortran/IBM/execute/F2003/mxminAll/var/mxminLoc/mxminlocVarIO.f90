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
!*  DESCRIPTION                : MAX/MIN with variable as object in
!*                               IO.
!* ===================================================================

  program mxminlocVarIO 

     character*3 :: charArr(2,3,4,5)
     logical        m(2,3,4,5)
     integer io_stat

     character*50 CHARVAR

     charArr = 'abc'

     CHARVAR = "(30I3, 4I3, 60I3)"

     m =  .true.
     m(:,3,4,:) = .false.

     io_stat = -1
     write (unit=4, fmt = CHARVAR, iostat=io_stat) maxloc(charArr, dim=3)   &
     , minloc(charArr), maxloc(charArr, dim=1, mask = m)
     
     if (io_stat .ne. 0) then
        error stop 1_4
     end if

     io_stat = -1
     close (unit=4, iostat=io_stat)
     if (io_stat .ne. 0) then
        error stop 2_4
     end if

  end program mxminlocVarIO 

