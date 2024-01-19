!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 7 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC(FROM,TO)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. SECTION 13.7.82
!*  2. FROM AND TO ARE ARRAY
!*  3. CALL MOVE_ALLOC IN SUBROUTINE
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp(k,l)
       integer,kind :: k
       integer,len  :: l
       integer(k)   :: i1(l)
   end type
end module

program move_allocBasic01

  use m
  implicit none

  integer :: i=0,j=0,k=0

  type(dtp(2,2)),allocatable :: from1(:)
  type(dtp(2,2)),allocatable :: from2(:,:)

  type(dtp(2,:)),allocatable :: to1(:)
  type(dtp(2,2)),allocatable :: to2(:,:)

  interface
     subroutine sub1(arg1,arg2)
        import
        type(dtp(2,*)),allocatable,intent(inout) :: arg1(:)
        type(dtp(2,:)),allocatable,intent(out)   :: arg2(:)
     end subroutine

     subroutine sub2(arg1,arg2)
        import
        type(dtp(2,*)),allocatable,intent(inout) :: arg1(:,:)
        type(dtp(2,2)),allocatable,intent(out)   :: arg2(:,:)
     end subroutine

  end interface

  allocate(from1(2:10),source= (/( dtp(2,2)(i1=[i,-i]),i=1,9) /) )

  allocate(dtp(2,2) :: from2(-3:-1,4:6))
  from2=reshape(from1,(/3,3/))

  allocate(to1(0:-1),source=dtp(2,2)(i1=-1))
  allocate(to2(1,1),source=dtp(2,2)(i1=-2))

  if(.not. allocated(from1))                    error stop 10_4
  if(.not. allocated(to1))                      error stop 11_4

  call sub1(from1,to1)

  if(allocated(from1))                          error stop 12_4
  if(.not. allocated(to1))                      error stop 13_4

  if(to1%k /= 2)                                error stop 14_4
  if(to1%l /= 2)                                error stop 15_4
  if(lbound(to1,1) /= 2)                        error stop 16_4
  if(ubound(to1,1) /= 10)                       error stop 17_4

  do i=2, 10
     if(any(to1(i)%i1 /= [i-1,-(i-1)]))         error stop 18_4
  end do

  call sub2(from2,to2)

  if(allocated(from2))                          error stop 19_4
  if(.not. allocated(to2))                      error stop 20_4

  if(to2%k /= 2)                                error stop 21_4
  if(to2%l /= 2)                                error stop 22_4
  if(lbound(to2,1) /= -3)                       error stop 23_4
  if(ubound(to2,1) /= -1)                       error stop 24_4
  if(lbound(to2,2) /= 4)                        error stop 25_4
  if(ubound(to2,2) /= 6)                        error stop 26_4

  do j=4,6
    do i=-3,-1
      k=k+1
      if(any(to2(i,j)%i1 /= [k,-k]))            error stop 27_4
    end do
  end do

end program

  subroutine sub1(arg1,arg2)
     use m,only: dtp
     type(dtp(2,*)),allocatable,intent(inout) :: arg1(:)
     type(dtp(2,:)),allocatable,intent(out)   :: arg2(:)

     call move_alloc(arg1,arg2)
  end subroutine

  subroutine sub2(arg1,arg2)
     use m,only: dtp
     type(dtp(2,*)),allocatable,intent(inout) :: arg1(:,:)
     type(dtp(2,2)),allocatable,intent(out)   :: arg2(:,:)

     call move_alloc(arg1,arg2)
  end subroutine
