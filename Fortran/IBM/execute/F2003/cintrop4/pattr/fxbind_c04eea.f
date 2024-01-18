! *********************************************************************
!**********************************************************************
!* ===================================================================
!*
!* DATE                         : Jan, 7, 2004
!*
!* PRIMARY FUNCTIONS TESTED     :Interoperable Functions.
!*                              - Fortran Entry  function called from C
!*                              - interop functions contained in Module.
!*
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!*   - Test: BINC(C) attribute with  different intrinsic data type,
!*           integer*1, integer*2,integer*4,integer*8.
!*
!*   - The interoperable  procedure itself is  implemented using Fortran
!*     function Entry Statement.
!*   - passing scalar arguments by REFERENCE and by VALUE
!*   - Both primary entry point and an alternate entry point have
!*     bind(c) attribute.
!*   - main written in C, C  calls FORTRAN functions.
!*
!*  ALGORITHM :
!*          1. C program call the Fortran function has a primary entry
!*             point and an alternate entry point.
!*          2. Assertion: Check the return value from Fortran Function
!*             and argument in C to verify it is correct.
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  01/07/04   KT     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mint
contains

  function fn_int1 (a, b) bind(c)
    integer(1), intent(inout) :: a
    integer(1), intent(inout) :: b
    integer(1)             :: fn_int1
    integer(1)             :: ent_int1
    fn_int1 = a * b
    a = a + 1
    b = b + 1
    return
    entry ent_int1 (a, b) bind(c)
    a = a + 1
    b = b + 1
    ent_int1 = a + b
    return
  end function fn_int1

  function fn_int2 (a, b) bind(c)
    integer(2), intent(inout) :: a
    integer(2), intent(inout) :: b
    integer(2)             :: fn_int2
    integer(2)             :: ent_int2
    fn_int2 = a * b
    a = a + 1
    b = b + 1
    return
    entry ent_int2 (a, b) bind(c)
    a = a + 1
    b = b + 1
    ent_int2 = a + b
    return
  end function fn_int2

  function fn_int4 (a, b) bind(c)
    integer(4), intent(inout) :: a
    integer(4), intent(inout) :: b
    integer(4)             :: fn_int4
    integer(4)             :: ent_int4
    fn_int4 = a * b
    a = a + 1
    b = b + 1
    return
    entry ent_int4 (a, b) bind(c)
    a = a + 1
    b = b + 1
    ent_int4 = a + b
    return
  end function fn_int4

  function fn_int8 (a, b) bind(c)
    integer(8), intent(inout) :: a
    integer(8), intent(inout) :: b
    integer(8)             :: fn_int8
    integer(8)             :: ent_int8
    fn_int8 = a * b
    a = a + 1
    b = b + 1
    return
    entry ent_int8 (a, b) bind(c)
    a = a + 1
    b = b + 1
    ent_int8 = a + b
    return
  end function fn_int8

  function fn_intval1 (a, b) bind(c)
    integer(1), value :: a
    integer(1), value :: b
    integer(1)             :: fn_intval1
    integer(1)             :: ent_intval1
    a = a +1
    b = b +1
    fn_intval1 = a * b
    return
    entry ent_intval1 (a, b) bind(c)
    a = a +1
    b = b +1
    ent_intval1 = a + b
    return
  end function fn_intval1

  function fn_intval2 (a, b) bind(c)
    integer(2), value :: a
    integer(2), value :: b
    integer(2)             :: fn_intval2
    integer(2)             :: ent_intval2
    a = a +1
    b = b +1
    fn_intval2 = a * b
    return
    entry ent_intval2 (a, b) bind(c)
    a = a +1
    b = b +1
    ent_intval2 = a + b
    return
  end function fn_intval2

  function fn_intval4 (a, b) bind(c)
    integer(4), value :: a
    integer(4), value :: b
    integer(4)             :: fn_intval4
    integer(4)             :: ent_intval4
    a = a +1
    b = b +1
    fn_intval4 = a * b
    return
    entry ent_intval4 (a, b) bind(c)
    a = a +1
    b = b +1
    ent_intval4 = a + b
    return
  end function fn_intval4

  function fn_intval8 (a, b) bind(c)
    integer(8), value :: a
    integer(8), value :: b
    integer(8)             :: fn_intval8
    integer(8)             :: ent_intval8
    a = a +1
    b = b +1
    fn_intval8 = a * b
    return
    entry ent_intval8 (a, b) bind(c)
    a = a +1
    b = b +1
    ent_intval8 = a + b
    return
  end function fn_intval8
end module mint
