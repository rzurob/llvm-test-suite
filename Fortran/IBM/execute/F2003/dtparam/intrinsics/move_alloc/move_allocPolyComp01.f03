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
!*  2. COMPONENTS ARE POLYMORPHIC OR UNLIMITED POLYMORPHIC ALLOCATABLE
!*  3. FROM AND TO ARE ALSO POLYMORPHIC
!234567890123456789012345678901234567890123456789012345678901234567890
module m
    type dtp(k1,l1)
       integer,kind :: k1
       integer,len  :: l1
       class(*),allocatable :: unlimit1
    end type
    type container(k2,l2)
       integer,kind   :: k2
       integer(2),len :: l2
       class(dtp(2*k2,2*l2)),allocatable :: dtp1
    end type
end module

program move_allocPolyComp01

  use m
  implicit none

  type(container(1,2)) :: contain1
  class(container(1,2)),allocatable :: from1
  class(*),allocatable :: to1

  allocate(contain1%dtp1,source=dtp(2*contain1%k2,2*contain1%l2)(null()) )

  select type(x=>contain1%dtp1)
     type is(dtp(2,*))
         allocate(x%unlimit1,source="xlftest")
     class default
         error stop 100_4
  end select

  allocate(from1,source=contain1)

  call move_alloc(from1,to1)

  if(allocated(from1))                        error stop 11
  if(.not. allocated(to1))                    error stop 12

  select type(x=>to1)
     type is(container(1,*))
        if(x%k2 /= 1)                         error stop 13
        if(x%l2 /= 2)                         error stop 14
        select type(y=>x%dtp1)
           type is(dtp(2,*))
             if(y%k1 /= 2)                    error stop 15
             if(y%l1 /= 4)                    error stop 16
             select type(z=>y%unlimit1)
               type is(character(*))
                  if(z%len /=7)               error stop 17
                  if(z /= "xlftest")          error stop 18
               class default
                  error stop 101_4
             end select
        end select
  end select

  if(allocated(from1))  deallocate(from1)
  allocate(from1,source=contain1)

  call move_alloc(from1%dtp1,to1)

  if(.not. allocated(to1))                    error stop 19
  if(.not. allocated(from1))                  error stop 20
  if(allocated(from1%dtp1))                   error stop 21
  select type(y=>from1%dtp1)
     type is(dtp(2,*))
        if(y%k1 /= 2)                         error stop 22
        if(y%l1 /= 4)                         error stop 23
           select type(z=>y%unlimit1)
               type is(character(*))
                  if(z%len /=7)               error stop 24
                  if(z /= "xlftest")          error stop 25
               class default
                   error stop 102_4
             end select
      class default
           error stop 103_4
  end select

  if(allocated(from1))  deallocate(from1)
  allocate(from1,source=contain1)

  call move_alloc(from1%dtp1%unlimit1,to1)

  if(.not. allocated(to1))                    error stop 26
  if(.not. allocated(from1))                  error stop 27
  if(.not. allocated(from1%dtp1))             error stop 28
  if(allocated(from1%dtp1%unlimit1))          error stop 29

  select type(z=>to1)
     type is(character(*))
        if(z%len /=7)                         error stop 30
        if(z /= "xlftest")                    error stop 31
     class default
        error stop 104_4
  end select

end program
