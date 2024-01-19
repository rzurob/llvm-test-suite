!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : July 29 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : TYPE PARAMETER INQUIRY
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 6.1.3
!* 2. DEFECT 354413
!*
!234567890123456789012345678901234567890123456789012345678901234567890

    program d354413
    implicit none
    character(*), parameter :: string = 'string literal'

    if (fun2(0_8) /= 8) error stop 1
    if (fun2(1*2_8) /= 8) error stop 2

    if (fun1('abcd') /= 'xlft') error stop 3
    if (fun1(string) /= 'xlftest') error stop 4
    if (len(fun1(string)) /= len(string)) error stop 5

    contains

     function fun2(b)  result (res)
         integer(8),intent(in) :: b
         integer(b%kind) res

         res=b%kind
     end function

     function fun1(c) result(res)
         character(*),intent(in) :: c
         character(c%len) res

         res='xlftest'
     end function

end
