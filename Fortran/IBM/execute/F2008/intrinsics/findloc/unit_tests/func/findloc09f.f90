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


  program FINDLOC_POINTER

    integer loop_i
    integer :: result1

    integer, pointer :: arr_int_p1(:), arr_int_p2(:)
    real, pointer :: arr_real_p1(:), arr_real_p2(:)
    character(3), pointer :: arr_char_p1(:), arr_char_p2(:)
    complex, pointer :: arr_comp_p1(:), arr_comp_p2(:)
    logical, pointer :: arr_log_p1(:), arr_log_p2(:)

    integer, target :: arr_int(5) = (/59, 95, 59, 59, 95/)
    real, target :: arr_real(5) = (/44.5, 44.5, 44.0, 44.5, 44.5/)
    character(3), target :: arr_char(5) = (/"IBM", "XLF", "IBM", "IBM", "IBM"/)
    complex, target :: arr_comp(5) = (/(2,1), (2,1), (2,1), (2,1), (1,2)/)
    logical, target :: arr_log(5) = (/.FALSE., .TRUE., .TRUE., .TRUE., .TRUE./)

    allocate(arr_int_p1(1:5))
    arr_int_p1 = 59
    arr_int_p1(5) = 95
    arr_int_p1(2) = 95

    allocate(arr_real_p1(1:5))
    arr_real_p1 = 44.5
    arr_real_p1(3) = 44.0

    allocate(arr_char_p1(1:5))
    arr_char_p1 = "IBM"
    arr_char_p1(2) = "XLF"

    allocate(arr_comp_p1(1:5))
    arr_comp_p1 = (2,1)
    arr_comp_p1(5) = (1,2)

    allocate(arr_log_p1(1:5))
    arr_log_p1 = .TRUE.
    arr_log_p1(1) = .FALSE.

    arr_int_p2  => arr_int
    arr_real_p2 => arr_real
    arr_char_p2 => arr_char
    arr_comp_p2 => arr_comp
    arr_log_p2  => arr_log

    if (findloc(arr_int_p1, 95, 1, BACK=.TRUE.) .NE. 5) ERROR STOP 10
    if (findloc(arr_int_p2, 95, 1, BACK=.TRUE.) .NE. 5) ERROR STOP 11

    if (ANY(findloc(arr_real_p1, 44.0) .NE. (/3/))) ERROR STOP 20
    if (ANY(findloc(arr_real_p2, 44.0) .NE. (/3/))) ERROR STOP 21

    if (findloc(arr_char_p1, "XLF", 1) .NE. 2) ERROR STOP 30
    if (findloc(arr_char_p2, "XLF", 1) .NE. 2) ERROR STOP 31

    if (findloc(arr_comp_p1, (1,2), 1) .NE. 5) ERROR STOP 40
    if (findloc(arr_comp_p2, (1,2), 1) .NE. 5) ERROR STOP 41

    if (ANY(findloc(arr_log_p1, .FALSE.) .NE. (/1/))) ERROR STOP 50
    if (ANY(findloc(arr_log_p2, .FALSE.) .NE. (/1/))) ERROR STOP 51

 end
