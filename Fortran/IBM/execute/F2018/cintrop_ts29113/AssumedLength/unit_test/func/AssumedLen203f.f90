! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : AssumedLen201f.f
!*
!* PROGRAMMER                   : Maryam Moghadas
!* DATE                         : June  25, 2014
!* ORIGIN                       : AIX Complier Development
!*
!*
!* PRIMARY FUNCTIONS TESTED     : C Interop: Assumed length object
!* SECONDARY FUNTIONS TESTED    :
!*
!* DRIVER STANZA                :
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
subroutine sub(arg) 
 character(*), optional :: arg(2,3)
 interface
  subroutine sub_1(arg1) bind(c)
    character(*), optional :: arg1(2,3)
  end subroutine sub_1
 end interface

 call sub_1()  
 call sub_1(arg)
end subroutine sub

program AssumedLen12 
 interface
  subroutine sub(arg) 
    character(*), optional :: arg(2,3)
  end subroutine
  subroutine sub_1(arg1) bind(c)
    character(*), optional :: arg1(2,3)
  end subroutine sub_1 
 end interface
 
 character(3) :: var1(6)
 character(3), allocatable :: var2(:)

 var1 = "IBM"
 
 call sub()
 call sub_1(var2)

end program
