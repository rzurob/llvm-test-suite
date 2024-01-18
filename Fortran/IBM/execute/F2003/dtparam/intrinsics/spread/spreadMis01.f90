!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 23 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : SPREAD(SOURCE,DIM,NCOPIES)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. SECTION 13.7.114
!*  2. SPREAD IS USED AS ACTUAL ARGUMENT OF MERGE
!*  3. AND MERGE IS ALSO USED AS ACTUAL ARGUMENT OF SPREAD
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l1)
       integer,len :: l1
       character(l1) :: c1
   end type
   type,extends(base) :: child(l2)
       integer,len :: l2
       character(l2) :: c2
       class(base(l1)),pointer :: base1=>null()
   end type
end module

program spreadMis01
  use m
  implicit none

  integer :: i
  class(base(:)),allocatable   :: dtp1
  type(child(2,3)),target      :: tar1
  class(base(:)),allocatable   :: dtp2(:)
  type(child(:,:)),allocatable :: dtp3(:,:)

  tar1%c1 = "00"
  tar1%c2 = "11"

  allocate(dtp1,source=child(2,3)("12","34",tar1))

  allocate(dtp2(2:3),source= &
              merge(spread(dtp1,1,2),spread(dtp1,1,2),[.true.,.false.]) )

  select type(dtp2)
    type is(child(*,*))
      if(dtp2%l1 /= 2)                              error stop 10_4
      if(dtp2%l2 /= 3)                              error stop 11_4
      if(any(dtp2%c1 /= "12"))                      error stop 12_4
      if(any(dtp2%c2 /= "34"))                      error stop 13_4
      if(.not. associated(dtp2(2)%base1,tar1))      error stop 14_4
      if(.not. associated(dtp2(3)%base1,tar1))      error stop 15_4
      select type(x=>dtp2(2)%base1)
         type is(child(*,*))
              if(x%c1 /= "00")                      error stop 16_4
              if(x%c2 /= "11")                      error stop 17_4
         class default
            error stop 101_4
      end select
      select type(x=>dtp2(3)%base1)
         type is(child(*,*))
              if(x%c1 /= "00")                      error stop 18_4
              if(x%c2 /= "11")                      error stop 19_4
         class default
              error stop 102_4
      end select
    class default
       error stop 100_4
  end select

  dtp3=spread(merge([child(2,3)("aa","bb"),child(2,3)("cc","dd",tar1)], &
                     [child(2,3)("55","66"),child(2,3)("77","88")], &
                     [ .false. , .true.] ),1,5 )
  do i=1, 5
    if(dtp3(i,1)%c1 /= "55")                         error stop 20_4
    if(dtp3(i,1)%c2 /= "66")                         error stop 21_4
    if(associated(dtp3(i,1)%base1))                  error stop 22_4
    if(dtp3(i,2)%c1 /= "cc")                         error stop 23_4
    if(dtp3(i,2)%c2 /= "dd")                         error stop 24_4
    if(.not. associated(dtp3(i,2)%base1))            error stop 25_4
    select type(x=>dtp3(i,2)%base1)
       type is(child(*,*))
          if(x%c1 /= "00")                           error stop 26_4
          if(x%c2 /= "11")                           error stop 27_4
       class default
         error stop 103_4
    end select
  end do
end program
