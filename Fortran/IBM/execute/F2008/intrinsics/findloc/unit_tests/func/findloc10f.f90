!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2013-05-27
!*
!*  PRIMARY FUNCTIONS TESTED   : FINDLOC intrinsic
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FINDLOC (ARRAY, VALUE, DIM [, MASK, KIND, BACK])
!*                               FINDLOC (ARRAY, VALUE [, MASK, KIND, BACK])
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


  program FINDLOC_ALLOCATABLE

    integer loop_i
    integer :: result1

    integer :: arr_int(3)= (/1,2,3/)
    real :: arr_real(3) = (/1.0,2.0,3.0/)
    character(3) :: arr_char(3) = (/"ab1", "ab2", "ab3"/)
    complex :: arr_comp(3) = (/(1,0), (2,1), (3,0)/)
    logical :: arr_log(3) = (/.TRUE., .FALSE., .TRUE./)

    call test(arr_int, arr_real, arr_char, arr_comp, arr_log)

  contains
    subroutine test(arr_i, arr_r, arr_ch, arr_co, arr_l)
     integer :: arr_i(:)
     real :: arr_r(:)
     character(3) :: arr_ch(:)
     complex :: arr_co(:)
     logical :: arr_l(:)

     if (findloc(arr_i, 2, 1) .NE. 2) ERROR STOP 100
     if(findloc(arr_r, 1.0, 1) .NE. 1) ERROR STOP 200
     if (findloc(arr_ch, "ab3", 1) .NE. 3) ERROR STOP 300
     if (findloc(arr_co, (2,1), 1) .NE. 2) ERROR STOP 400
     if (findloc(arr_l, .FALSE., 1) .NE. 2) ERROR STOP 500
   end subroutine  test

 end
