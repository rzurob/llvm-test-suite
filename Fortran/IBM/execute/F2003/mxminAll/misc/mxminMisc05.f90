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
!*  DESCRIPTION                : MAXVAL/MINVAL and MAXLOC/MINLOC as actual 
!*                               argument passed to subprogram with  literal 
!*                               argument. 
!*   (314692)
!* ===================================================================

module misc5

    contains

          function fun1(arg)
             character*2, intent(in):: arg
             character*2 fun1
             fun1 = arg
          end function 

          function fun3(arg)
             integer, intent(in)::arg
             integer fun3
             fun3 = arg
          end function 

end module misc5

program mxminMisc05 
    use misc5

    if(fun1(maxval((/'aa', 'bb' ,'cc'/), dim=1, mask = .true.)) .ne. 'cc') then
          error stop 1_4
    endif

    if(ichar(fun1(maxval((/'aa', 'bb' ,'cc'/), dim = 1, mask = .false.)))  &
         .ne. 0)  then
          error stop 2_4
    endif
        
    if(fun1(minval((/'aa', 'bb' ,'cc'/), dim=1, mask = .true.)) .ne. 'aa') then
          error stop 3_4
    endif

    if(ichar(fun1(minval((/'aa', 'bb' ,'cc'/), dim = 1, mask = .false.)))  &
         .ne. 127)  then
          error stop 4_4
    endif

    if(fun3(maxloc((/'aa','bb','cc'/), dim =1,  mask = .true.)) .ne. 3) then
             error stop 5_4
    endif

    if(fun3(minloc((/'aa','bb','cc', 'dd'/), dim=1,  mask = .false.)) &  
        .ne. 0) then
            error stop 6_4
    endif

end program mxminMisc05 

