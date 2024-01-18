! *********************************************************************
!* ===================================================================
!*
!* DATE                         : June  25, 2014
!* ORIGIN                       : AIX Complier Development
!*
!* PRIMARY FUNCTIONS TESTED     : C Interop: Assumed length object
!* SECONDARY FUNTIONS TESTED    :
!*
!* REQUIRED COMPILER OPTIONS    : -qdebug = BCASSUMEDLEN  (temporarily)
!*
!* DESCRIPTION                  : Calling a C BIND(C) procedure from Fortran
!*
!*                                - type character(*)
!*                                - Dummy argument is an Scalar
!*                                - Call to BIND(C) procedure from
!*                                   internal procedure to test
!*                                   optional attribute.
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890
subroutine sub(arg)
 character(*), optional :: arg
 interface
  subroutine sub_1(arg1) bind(c)
    character(*), optional :: arg1
  end subroutine sub_1
 end interface
 call sub_1(arg)
end subroutine sub

program main
 interface
  subroutine sub(arg)
    character(*), optional :: arg
  end subroutine
  subroutine sub_1(arg1) bind(c)
    character(*), optional :: arg1
  end subroutine sub_1
 end interface

 character(3) :: var1
 character(:), allocatable :: var2

 var1 = "IBM"

 call sub()

 call sub_1(var2)

end program
