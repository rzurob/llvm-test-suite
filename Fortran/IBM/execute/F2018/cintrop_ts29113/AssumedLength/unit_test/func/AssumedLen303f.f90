! *********************************************************************
!* ===================================================================
!*
!* DATE                         : June  25, 2014
!*
!* PRIMARY FUNCTIONS TESTED     : C Interop: Assumed length object
!* SECONDARY FUNTIONS TESTED    :
!*
!* REQUIRED COMPILER OPTIONS    : -qdebug = BCASSUMEDLEN  (temporarily)
!*
!* DESCRIPTION                  : Calling a C BIND(C) procedure from Fortran
!*
!*                                - type character(*)
!*                                - Dummy argument is an explicit_shape array
!*                                - Call to BIND(C) procedure from internal
!*                                      procedure to test optional attribute
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890
program main
 interface
  subroutine sub(arg)
    character(*), optional :: arg(5)
  end subroutine
  subroutine sub_1(arg1) bind(c)
    character(*), optional :: arg1(*)
  end subroutine sub_1
 end interface

 character(3), allocatable :: var(:)

 call sub()

 call sub_1(var)

end program


subroutine sub(arg)
 use iso_c_binding
 character(*), optional :: arg(5)
 interface
  subroutine sub_1(arg1) bind(c)
    character(*), optional :: arg1(*)
  end subroutine sub_1
 end interface

 call sub_1()
 call sub_1(arg)
end subroutine sub



