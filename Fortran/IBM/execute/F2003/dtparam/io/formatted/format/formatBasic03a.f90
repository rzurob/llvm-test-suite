!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : formatBasic03a.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Dec. 5 2008 
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
!* 1. derived type has integer componnet and nested derived type component which has ultimate integer component
!* 2. test PRINT statement with integer editing, use different format such as binary,octal,hexdecimal format
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type inner1(k1,l1)
     integer,kind :: k1
     integer,len  :: l1
     integer(k1) :: i1(l1)
  end type 
  type inner2(k2,l2)
     integer,kind :: k2
     integer,len  :: l2
     integer(k2) :: i2(l2)
     type(inner1(2*k2,l2+1)) :: comp1
  end type

  type outer(k3,l3)
     integer,kind :: k3
     integer,len  :: l3
     integer(k3)  :: i3(l3)
     type(inner2(2*k3,l3+1)) :: comp2 
  end type

  contains

    function getResult(arg)
      class(outer(2,*)),intent(in) :: arg(:)
      type(outer(arg%k3,arg%l3)),allocatable   :: getResult(:) 

      getResult=arg 
    end function

    subroutine write1(arg)
      type(outer(2,*)),intent(in) :: arg(:)
 
      print '("|",B64.32,"|")',  arg
    end subroutine 

    subroutine write2(arg)
      type(outer(2,*)),intent(in) :: arg(:)

      print '("|",O64.32,"|")',  arg
    end subroutine

    subroutine write3(arg)
      type(outer(2,*)),intent(in) :: arg(:)

      print '("|",Z16.8,"|")',  arg
    end subroutine

end module

program formatBasic03a
  use m
  implicit none

  type(outer(2,1)) :: outer1(2)
  type(outer(2,:)),pointer :: outer2(:)
  type(outer(2,:)),target,allocatable :: outer3(:)

  outer1=[outer(2,1)(i3=[101],comp2= &
         inner2(4,2)(i2=[202,303],comp1=inner1(8,3)(i1=[404,505,606])) ), &
          outer(2,1)(i3=[-101],comp2= &
       inner2(4,2)(i2=[-202,-303],comp1=inner1(8,3)(i1=[-404,-505,-606])) ) ]
          
  outer3=outer1

  outer2(0:1)=>outer3

  print 100, getResult(outer1)

  call write1(outer2)

  call write2(outer3)

  call write3(getResult(outer3))

100 format(bz,sp,"|",i5,"|",i7.4,"|",i7.5,"|",tr3,tl3,ss,bn,"|",i8,"|",i8.6,"|",i8.5,"|")


end program
