!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d358778_2.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Nov. 13 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   :  
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*  DEFECT 358778
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  contains
   subroutine sub1(arg1,arg2)
      character(:),allocatable,optional :: arg1(:)
      character(:),allocatable          :: arg2(:)

      print *,fun1(arg1,arg2)
      contains
         elemental function fun1(arg1,arg2)
            character(3),intent(in),optional :: arg1
            character(3),intent(in)          :: arg2
            character(3)                      :: fun1

            if(present(arg1)) then
                  fun1=arg1
            else
                  fun1=arg2
            end if
         end function
   end subroutine
end module

program d358778_2
  use m
  implicit none

  character(:),allocatable :: c1(:)
  character(:),allocatable :: c2(:)

  allocate(c1(2),source=["xlf","xlc"])
  allocate(c2(2),source=["123","456"])

  call sub1(c1,c2)

  call sub1(arg2=c2)

end program
