!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d359315.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Nov. 24 2008 
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
!*  DEFECT 359315 
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type dt(l)
     integer,len  :: l
     integer      :: i1
  end type
  type base(l1)
     integer,len  :: l1
     type(dt(l1)) :: dtcomp1
  end type
  type,extends(base) :: child(l2)
     integer,len  :: l2
  end type

  contains
     function multichild(this,arg)
       class(child(*,*)),intent(in) :: this
       class(base(:)),allocatable,intent(in) :: arg(:)
       class(base(:)),allocatable :: multichild(:) 

       allocate(child(1,3) ::multichild(size(arg,1)) )
       select type(multichild)
          type is(child(*,*))
             select type(arg)
               type is(child(*,*))
                  multichild%dtcomp1%i1=this%dtcomp1%i1 * arg%dtcomp1%i1
                  print *,multichild%dtcomp1%i1
               class default
                  error stop 10_4
             end select
       end select
     end function

end module

program d359315
  use m
  implicit none

  class(base(:)),allocatable :: this,dtp1(:)
  class(base(:)),allocatable :: result1(:)

  allocate(this,source=child(3,2)(dtcomp1=dt(3)(i1=1) ) )

  allocate(dtp1(2:3),source=&
      [child(1,3)(dtcomp1=dt(1)(i1=3)) ,&
       child(1,3)(dtcomp1=dt(1)(i1=-3)) ])

  select type(this)
    type is(child(*,*))
      allocate(result1(2),source = multichild(this,dtp1) )
    class default
      error stop 11_4
  end select
  select type(result1)
     type is(child(*,*))
          print *,result1%dtcomp1%i1
     class default
          error stop 12_4
  end select

end program
