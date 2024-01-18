!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : FINDLOC_ALLOCATABLE
!*
!*  PROGRAMMER                 : Maryam Moghadas
!*  DATE                       : 2013-05-27
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : FINDLOC intrinsic
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              :
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

    integer, allocatable :: arr_int(:)
    real, allocatable :: arr_real(:)
    character(3), allocatable :: arr_char(:)
    complex, allocatable :: arr_comp(:)
    logical, allocatable :: arr_log(:)
   
    allocate(arr_int(1:6))
    arr_int = 59
    arr_int(5) = 95
    arr_int(2) = 95

    allocate(arr_real(1:8))
    arr_real = 44.5
    arr_real(3) = 44.0

    allocate(arr_char(1:5))
    arr_char = "IBM"
    arr_char(2) = "XLF"

    allocate(arr_comp(1:10))
    arr_comp = (2,1)
    arr_comp(10) = (1,2)

    allocate(arr_log(1:3)) 
    arr_log = .TRUE.
    arr_log(1) = .FALSE.
    
    if (findloc(arr_int, 95, 1, BACK=.TRUE.) .NE. 5) ERROR STOP 10

    if (ANY(findloc(arr_real, 44.0) .NE. (/3/))) ERROR STOP 20

    if (findloc(arr_char, "XLF", 1) .NE. 2) ERROR STOP 30

    if (findloc(arr_comp, (1,2), 1) .NE. 10) ERROR STOP 40

    if (ANY(findloc(arr_log, .FALSE.) .NE. (/1/))) ERROR STOP 50
    

 end
