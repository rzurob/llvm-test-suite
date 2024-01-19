!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 3 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC(FROM,TO)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. SECTION 13.7.82
!*  2. COMPONENTS ARE POLYMORPHIC OR UNLIMITED POLYMORPHIC POINTER
!*  3. FROM AND TO ARE ARRAY
!234567890123456789012345678901234567890123456789012345678901234567890
module m
    type dtp(k1,l1)
       integer,kind :: k1
       integer,len  :: l1
       class(*),pointer :: unlimit1=>null()
    end type
    type container(k2,l2)
       integer,kind   :: k2
       integer(2),len :: l2
       class(dtp(2*k2,2*l2)),pointer :: dtp1=>null()
    end type
end module

program move_allocPolyComp02
  use m
  implicit none

  type(container(1,2)) :: contain1,contain2
  type(container(1,:)),allocatable :: from1(:)
  type(container(1,2)),allocatable :: to1(:)

  allocate(contain1%dtp1,source=dtp(2,4)(null()) )
  select type(x=>contain1%dtp1)
     type is(dtp(2,*))
        allocate(contain1%dtp1%unlimit1,source=contain1)
  end select

  allocate(contain2%dtp1,source=dtp(2,4)(null()) )
  select type(x=>contain2%dtp1)
     type is(dtp(2,*))
        allocate(contain2%dtp1%unlimit1,source=contain2%dtp1)
     class default
        error stop 100_4
  end select

  allocate(from1(2:3),source=[contain1,contain2])

  allocate(to1(4:7),source=[contain1,contain2,contain2,contain1])

  if(.not. allocated(to1))                   error stop 11_4
  if(lbound(to1,1) /= 4)                     error stop 10_4
  if(ubound(to1,1) /= 7)                     error stop 11_4
  if(size(to1) /= 4)                         error stop 12_4

  call move_alloc(from=from1,to=to1)

  if(allocated(from1))                       error stop 13_4
  if(.not. allocated(to1))                   error stop 14_4

  if(size(to1) /= 2)                         error stop 15_4
  if(lbound(to1,1) /= 2)                     error stop 16_4
  if(ubound(to1,1) /= 3)                     error stop 17_4
  if(to1%k2 /= 1)                            error stop 18_4
  if(to1%l2 /= 2)                            error stop 19_4

  if(to1(2)%dtp1%k1 /= 2)                    error stop 20_4
  if(to1(2)%dtp1%l1 /= 4)                    error stop 21_4
  select type(x=>to1(2)%dtp1)
    type is(dtp(2,*))
       select type(y=>x%unlimit1)
          type is(container(1,*))
             if(y%k2 /= 1)                   error stop 22_4
             if(y%l2 /= 2)                   error stop 23_4
          class default
             error stop 101_4
       end select
  end select

  if(to1(3)%dtp1%k1 /= 2)                    error stop 24_4
  if(to1(3)%dtp1%l1 /= 4)                    error stop 25_4
  select type(x=>to1(3)%dtp1)
    type is(dtp(2,*))
       select type(y=>x%unlimit1)
          type is(dtp(2,*))
             if(y%k1 /= 2)                   error stop 26_4
             if(y%l1 /= 4)                   error stop 27_4
          class default
             error stop 102_4
       end select
  end select
end program
