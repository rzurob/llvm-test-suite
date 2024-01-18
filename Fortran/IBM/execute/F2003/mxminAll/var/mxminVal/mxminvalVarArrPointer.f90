!*  ===================================================================
!*
!*  DATE                       : 1/25/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX*/MIN* intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAXVAL/MINVAL with variable as array
!*                               pointer.
!* ===================================================================

  program mxminvalArrPointer

        character*3, pointer, dimension(:,:, :) :: arr_pointer
        character*3, target, dimension(3,3,2) :: arr_target
        character*3 :: v(3,3)
        logical,  dimension(3,5,2) :: arr_logical

        arr_target = "abc"
        arr_logical = .true.

        arr_target(:,2,1) = "xyz"
        arr_logical(:,2,1) = .false.

        arr_pointer => arr_target

        if(maxval(arr_pointer) .ne. "xyz") error stop 1_4

        if(maxval(arr_pointer, mask=arr_logical) .ne. "abc") error stop 2_4

        v = maxval(arr_pointer, dim=3)

        if(any(v(:,3) .ne. "abc")) error stop 3_4

        if(any(minval(arr_pointer, dim=3) .ne. minval(arr_target,dim=3))) then
                  error stop 4_4
        endif

        allocate(arr_pointer(3,3,3))

        arr_pointer = "zzz"
        arr_pointer(:,3,3) = "abc"

        v = minval(arr_pointer, dim=3)

        if(any(v(:,3) .ne. "abc")) error stop 5_4

        deallocate(arr_pointer)

  end program mxminvalArrPointer
