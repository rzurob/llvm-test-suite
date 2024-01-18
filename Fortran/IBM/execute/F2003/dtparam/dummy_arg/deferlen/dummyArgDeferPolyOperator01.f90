!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 23 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : DUMMY ARGUMENT WITH DEFERRED LENGTH
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1.allocatable with deferred length parameter is used as dummy argument of overriding & overiden bindings.
!*  2. user defined operator is "*"
!*  3. it will invoke corresponding bindings when using base or child passed-object
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type dt(k,l)
     integer,kind :: k
     integer,len  :: l
     integer(k)   :: i1
  end type
  type base(k1,l1)
     integer,kind :: k1
     integer,len  :: l1
     type(dt(k1,l1)) :: dtcomp1
     contains
       procedure,pass :: multi=>multibase
       generic :: operator(*) => multi
  end type
  type,extends(base) :: child(k2,l2)
     integer,kind :: k2
     integer,len  :: l2
     type(dt(k2+1,l2+1)) :: dtcomp2
     contains
       procedure,pass :: multi=>multichild
       generic :: operator(*) =>multi
  end type

  contains

     function multibase(this,arg)
       class(base(2,*)),intent(in) :: this
       class(base(2,:)),allocatable,intent(in) :: arg(:)
       class(base(2,:)),allocatable :: multibase(:)

       allocate(base(2,20) :: multibase(size(arg,1)) )

       multibase%dtcomp1%i1=this%dtcomp1%i1 * arg%dtcomp1%i1 * 2
     end function

     function multichild(this,arg)
       class(child(2,*,1,*)),intent(in) :: this
       class(base(2,:)),allocatable,intent(in) :: arg(:)
       class(base(2,:)),allocatable :: multichild(:)

       allocate(child(2,5,1,10) ::multichild(size(arg,1)) )
       select type(multichild)
          type is(child(2,*,1,*))
             select type(arg)
               type is(child(2,*,1,*))
                  multichild%dtcomp1%i1=this%dtcomp1%i1 * arg%dtcomp1%i1
                  multichild%dtcomp2%i1=this%dtcomp2%i1 * arg%dtcomp2%i1
               class default
                  error stop 51_4
             end select
          class default
             error stop 50_4
       end select
     end function

end module

program dummyArgDeferPolyOperator01
  use m
  implicit none

  class(base(2,:)),allocatable :: this,dtp1(:)
  class(base(2,:)),allocatable :: result1(:),result2(:)

  allocate(this,source=child(2,3,1,2)(dtcomp1=dt(2,3)(i1=1), &
            dtcomp2=dt(2,3)(i1=2)) )


  allocate(dtp1(2:3),source=&
      [child(2,1,1,3)(dtcomp1=dt(2,1)(i1=3),dtcomp2=dt(2,4)(i1=4)) ,&
       child(2,1,1,3)(dtcomp1=dt(2,1)(i1=-3),dtcomp2=dt(2,4)(i1=-4)) ])


  allocate(result1(2),source = this * dtp1 )

  select type(result1)
     type is(child(2,*,1,*))
          if(result1%l1 /= 5)                     error stop 10_4
          if(result1%l2 /= 10)                    error stop 11_4
          if(any(result1%dtcomp1%i1 /= [3,-3]))   error stop 12_4
          if(any(result1%dtcomp2%i1 /= [8,-8]))   error stop 13_4
      class default
          error stop 52_4
  end select

  select type(this)
     type is(child(2,*,1,*))
        allocate(result2(-1:0),source= this%base * dtp1)
        select type(result2)
          type is(base(2,*))
             if(result2%l1 /= 20)                  error stop 14_4
             if(any(result2%dtcomp1%i1 /=[6,-6]))  error stop 15_4
          class default
             error stop 53_4
        end select
  end select

end program
