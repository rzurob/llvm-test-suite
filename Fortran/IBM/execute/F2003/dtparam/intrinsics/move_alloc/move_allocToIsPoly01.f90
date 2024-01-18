!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 6 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC(FROM,TO)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. SECTION 13.7.82
!*  2. TO IS POLYMORPHIC SCALAR
!*  3. DEFECT 357082
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l1)
      integer,len :: l1
      character(l1) :: c1(l1)
      character(:),allocatable :: c2(:)
   end type
   type,extends(base) :: child(l2)
      integer,len :: l2
      character(l2),pointer :: c3(:)=>null()
   end type
end module

program move_allocToIsPoly01

  use m
  implicit none

  type(child(2,3)),allocatable :: from1
  class(base(:)),allocatable   :: from2
  class(*),allocatable         :: from3
  type(child(2,3))             :: child

  class(base(:)),allocatable :: to1
  class(*),allocatable       :: to2

  allocate(child(2,3) :: from1)
  from1%c1=["ab","cd"]
  from1%c2=["12","34"]
  allocate(character(3) :: from1%c3(3))
  from1%c3=["xlf","ibm","xlc"]

  call move_alloc(from1,to1)

  if(allocated(from1))                           error stop 10_4
  if(.not. allocated(to1))                       error stop 11_4
  select type(x=>to1)
     type is(child(*,*))
        if(x%l1 /= 2)                            error stop 12_4
        if(x%l2 /= 3)                            error stop 13_4
        print *,x%c1%len,x%c2%len,x%c3%len
        if(any(x%c1 /= ["ab","cd"]))             error stop 14_4
        if(any(x%c2 /= ["12","34"]))             error stop 15_4
        if(any(x%c3 /= ["xlf","ibm","xlc"]))     error stop 16_4
     class default
        error stop 100_4
  end select

  allocate(from2,source=child(2,3)(["00","11"],["22","33"]))

  select type(from2)
    type is(child(*,*))
        allocate(from2%c3(3),source=["44","55","66"])
  end select

  call move_alloc(from2,to2)

  if(allocated(from2))                           error stop 17_4
  if(.not. allocated(to2))                       error stop 18_4
  select type(x=>to2)
     type is(child(*,*))
        if(x%l1 /= 2)                            error stop 19_4
        if(x%l2 /= 3)                            error stop 20_4
        print *,x%c1%len,x%c2%len,x%c3%len
        if(any(x%c1 /= ["00","11"]))             error stop 21_4
        if(any(x%c2 /= ["22","33"]))             error stop 22_4
        if(any(x%c3 /= ["44","55","66"]))        error stop 23_4
     class default
        error stop 101_4
  end select

  child=child(2,3)(c1=["a1","b2"],c2=["1","2","3","4"])

  allocate(child%c3(2), source=["x","y"])

  allocate(from3,source=child)

  call move_alloc(from3, to2)

  if(allocated(from3))                           error stop 24_4
  if(.not. allocated(to2))                       error stop 25_4
  select type(x=>to2)
     type is(child(*,*))
        if(x%l1 /= 2)                            error stop 26_4
        if(x%l2 /= 3)                            error stop 27_4
        print *,x%c1%len,x%c2%len,x%c3%len
        if(any(x%c1 /= ["a1","b2"]))             error stop 28_4
        if(any(x%c2 /= ["1","2","3","4"]))       error stop 29_4
        if(any(x%c3 /= ["x","y"]))               error stop 30_4
     class default
        error stop 102_4
  end select

end program

