!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d358778_1.f   
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
  type dtp(l1)
     integer,len     :: l1
     character(l1)   :: c1
  end type

  contains

   subroutine sub1(arg1,arg2)
      type(dtp(:)),allocatable,optional :: arg1(:)
      type(dtp(:)),allocatable          :: arg2(:)

      print *,fun1(arg1,arg2)
      contains
         elemental function fun1(arg1,arg2)
            type(dtp(3)),intent(in),optional :: arg1
            type(dtp(3)),intent(in)          :: arg2
            type(dtp(3))                     :: fun1

            if(present(arg1)) then
                  fun1%c1=arg1%c1  
            else
                  fun1%c1=arg2%c1
            end if
         end function
   end subroutine
end module

program d358778_1
  use m
  implicit none

  type(dtp(:)),allocatable :: dtp1(:)
  type(dtp(:)),allocatable :: dtp2(:)

  allocate(dtp1(2),source=[dtp(3)("xlf"),dtp(3)("xlc")])
  allocate(dtp2(2),source=[dtp(3)("123"),dtp(3)("456")])

  call sub1(dtp1,dtp2)

  call sub1(arg2=dtp2)

end program

