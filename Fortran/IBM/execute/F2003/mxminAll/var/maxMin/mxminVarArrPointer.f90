!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 1/25/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX*/MIN* intrinsics 
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*
!*  DESCRIPTION                : MAX/MIN with variable as array
!*                               pointer. 
!* ===================================================================

  program mxminVarArrPointer 

        character*3, pointer, dimension(:,:, :) :: arr_pointer
        character*3, target, dimension(3,3,2) :: arr_target
        character*5 :: v

        arr_target = "abc"
        v = "aaaaa"

        arr_pointer => arr_target

        if(len(max(arr_pointer, v,arr_target)) .ne. 5) error stop 1_4

        if(any(min(arr_pointer,arr_target, v) .ne. "aaaaa")) error stop 2_4

        allocate(arr_pointer(3,3,3))

        arr_pointer = "zzz" 

        if(len(min(arr_pointer, v)) .ne. 5) error stop 3_4

        if(any(max(arr_pointer,v) .ne. "zzz  ")) error stop 4_4
 
        deallocate(arr_pointer)

  end program mxminVarArrPointer 
