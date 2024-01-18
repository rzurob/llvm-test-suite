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
!*  DESCRIPTION                : MAX*/MIN* inside parallel region 
!*                                
!* ===================================================================

program mxminMisc18 

    character*3 x(3, 4), y(2,3,4,6), x_v(3,4), y_v(2,3,4,6), v_x
    integer i_v(2)
 
    x = "aaa"
    y = "fff"

!$omp parallel
 !$omp workshare
   !$omp critical(region1)

      x_v = max(x, "ccc") 

   !$omp end critical(region1)

   !$omp critical(region2)
      y_v = min(y, "ddd")
   !$omp end critical(region2)
 !$omp end workshare

 !$omp barrier

 !$omp workshare
 
     v_x = maxval(x)

 !$omp end workshare  

 !$omp workshare

     i_v = maxloc(x)

 !$omp end workshare

!$omp end parallel

    if(any(x_v .ne. "ccc")) then
        error stop 1_4
    endif
    
    if(any(y_v .ne. "ddd")) then
        error stop 2_4
    endif

    if(any(i_v .ne. 1)) then
        error stop 3_4
    endif

    if(v_x .ne. "aaa") then
        error stop 4_4
    endif
 
end program mxminMisc18

