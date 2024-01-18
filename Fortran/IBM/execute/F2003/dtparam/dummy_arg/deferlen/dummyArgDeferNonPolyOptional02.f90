!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArgDeferNonPolyOptional02.f
!*
!*  DATE                       : Nov. 10 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : DUMMY ARGUMENT WITH DEFERRED LENGTH
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  allocatable or pointer derived type is optional dummy argument, function returns different result depends on optional dummy argument present or not.
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type dtp(k1,l1)
     integer,kind    :: k1
     integer,len     :: l1
     integer(k1)     :: id(l1-1:l1+1)
     character(l1)   :: name(l1-1:l1+1)
  end type

end module

program dummyArgDeferNonPolyOptional02
  use m
  implicit none

  type(dtp(2,:)),allocatable :: dtp1(:),dtp2(:)
  type(dtp(2,:)),pointer     :: dtp3=>null(),dtp4=>null()

  dtp1=[dtp(2,6)([1,2,3],["Robert","Michel","Grigor"]), &
        dtp(2,6)([4,5,6],["David","Nancy","Dorra"]), &
        dtp(2,6)([7,8,9],["Gaby","Glen","Kobi"])]

  allocate(dtp3,source=dtp1(1))

  dtp2=fun1(dtp1)
  print *,dtp2
  if(size(dtp2) /= 3)                                         error stop 9_4
  if(dtp2%l1 /= 6)                                            error stop 10_4
  if(any(dtp2(1)%id  /= [1,2,3]))                             error stop 11_4
  if(any(dtp2(2)%id  /= [4,5,6]))                             error stop 12_4
  if(any(dtp2(3)%id  /= [7,8,9]))                             error stop 13_4

  if(any(dtp2(1)%name  /= ["Robert","Michel","Grigor"]))      error stop 14_4
  if(any(dtp2(2)%name  /= ["David","Nancy","Dorra"]))         error stop 15_4
  if(any(dtp2(3)%name  /= ["Gaby","Glen","Kobi"]))            error stop 16_4

  dtp2=fun1()
  print *,dtp2
  if(size(dtp2) /= 1)                                         error stop 17_4
  if(any(dtp2(1)%id  /= [1,2,3]))                             error stop 18_4
  if(any(dtp2(1)%name  /= ["uname1","uname2","uname3"]))      error stop 19_4

  allocate(dtp4,source=fun2(dtp3))
  if(dtp4%l1 /= 6)                                            error stop 20_4
  if(any(dtp4%id  /= [1,2,3]))                                error stop 21_4
  if(any(dtp4%name /= ["Robert","Michel","Grigor"]))          error stop 22_4

  deallocate(dtp4)
  allocate(dtp4,source=fun2())
  if(dtp4%l1 /= 6)                                            error stop 23_4
  if(any(dtp4%id  /= [1,2,3]))                                error stop 24_4
  if(any(dtp4%name /= ["uname1","uname2","uname3"]))          error stop 25_4

  contains

    function fun1(arg)
       type(dtp(2,:)),optional,allocatable :: arg(:)
       type(dtp(2,:)),allocatable :: fun1(:)

       if(present(arg)) then
           fun1=arg
           print *,arg%l1,arg(1)%id,"|",arg(1)%name,"|"
       else
           fun1=[dtp(2,6)([1,2,3],["uname1","uname2","uname3"])]
       end if
    end function

    function fun2(arg)
       type(dtp(2,:)),optional,pointer :: arg
       type(dtp(2,:)),pointer :: fun2

       if(present(arg)) then
           allocate(fun2,source=arg)
       else
           allocate(fun2,source=dtp(2,6)([1,2,3],["uname1","uname2","uname3"]) )
       end if

    end function

end program
