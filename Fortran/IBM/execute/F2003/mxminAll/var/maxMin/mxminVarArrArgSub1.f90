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
!*  DESCRIPTION                : MAX/MIN with variable as actual
!*                               argument to subprogram
!*                
!*                               inside sub. argument of max/min is 
!*                               assumed-sizE array, automatic array 
!*                               and assumed length array
!* ===================================================================

  module subVarArg
     contains
        subroutine sub2(arg)
           character(*) arg(2,3)
           character*4  carg(2,3)
           if(len(arg) .ne. 3) then
               error stop 7_4
           endif
           if(any(arg .ne. "gko")) then
               error stop 8_4
           end if
           carg = "fko"
           if(any(min(arg, carg) .ne. "fko")) then
                error stop 9_4
           endif
        end subroutine
  end module subVarArg 

  program mxminVarArrArgSub1 
    
    use subVarArg 

    interface
        function func1(carg, n)
          character(*), dimension(*) :: carg 
          character(n), dimension(2*n) :: func1
        end function 
    end interface

    character*3 x(4), z1(2,3) , z2(2,3)
    character*3 y

    x = "ddd"
    y = "bbb" 
    z1="gko"
    z2="fgh"

    call sub2(max(z1, z2))

    if(any(LLT(min(x, y), max(x, y)) .neqv. .true.))then
         error stop 1_4
    endif

    if(any(max(x, y) .ne. "ddd")) then
          error stop 2_4
    endif 

    call sub1(max(x,y), 4)

    if(any(LGT(max(z1, z2), min(z1,z2)) .neqv. .true.)) then
           error stop 3_4
    endif

    if(any(func1(max(z1, z2),3) .ne. "gko")) then
           error stop 4_4
    endif

    call sub2(max(z1, z2))

    contains
         subroutine sub1(carg, iarg)
            character(*), dimension(*) :: carg
            integer iarg
            character*3  carray(iarg)
            character*3 :: darg(4) = "zzz" 
             
            carray = "xxx"

            if(any(max(carg(1:4), carray, darg) .ne. "zzz")) then
                   error stop 5_4
            endif
            if(any(min(carg(1:4), carray, darg) .ne. "ddd")) then
                   error stop 6_4
            endif

         end subroutine 

  end program mxminVarArrArgSub1 

  function func1(carg, n)
       character(*), dimension(*) :: carg 
       character(n), dimension(2*n) :: func1
       do i = 1, 2*n
           func1(i) = carg(i)
       end do
  end function
