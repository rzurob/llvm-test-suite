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
!*  2. USE GENERIC BINDING WITH SPREAD AS ACTUAL ARGUMENT
!*  3. RANK ARE DIFFERENT
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type dtp(k1,k2,l1,l2)
     integer,kind     :: k1,k2
     integer,len      :: l1,l2
     integer(k1+k2)   :: i(l1:l2)
     contains
        procedure,nopass :: spread1=>checkSpread1
        procedure,nopass :: spread2=>checkSpread2
        procedure,nopass :: spread3=>checkSpread3
        generic :: spread=>spread1,spread2,spread3
  end type

  contains

    subroutine checkSpread1(dt,dim)
       type(dtp(2,2,*,*)),intent(in) :: dt(:)
       integer, intent(in) :: dim
       integer :: i

       print *," in checkSpread1,dim= ",dim

       if(dt%k1 /= 2 .or. dt%k2 /= 2)                 error stop 10_4
       if(dt%l1 /= 1 .or. dt%l2 /= 2)                 error stop 11_4
       if(dim == 1) then
          do i=1,ubound(dt,1)
             if(lbound(dt(i)%i,1) /= 1)               error stop 12_4
             if(ubound(dt(i)%i,1) /= 2)               error stop 13_4
             if(any(dt(i)%i /= [1,2]))                error stop 14_4
          end do
       end if
    end subroutine
    subroutine checkSpread2(dt,dim)
       type(dtp(2,2,*,*)),intent(in) :: dt(:,:)
       integer, intent(in) :: dim
       integer  :: i

       print *," in checkSpread2,dim= ",dim

       if(dt%k1 /= 2 .or. dt%k2 /= 2)                 error stop 15_4
       if(dt%l1 /= 1 .or. dt%l2 /= 2)                 error stop 16_4
       select case(dim)
          case(1)
             do i=1,size(dt,1)
               if(any(dt(i,1)%i /= [1,2]))            error stop 17_4
               if(any(dt(i,2)%i /= [1,2]))            error stop 18_4
               if(lbound(dt(i,1)%i,1) /= 1)           error stop 19_4
               if(ubound(dt(i,2)%i,1) /= 2)           error stop 20_4
             end do
          case(2)
             do i=1,size(dt,2)
               if(any(dt(1,i)%i /= [1,2]))            error stop 21_4
               if(any(dt(2,i)%i /= [1,2]))            error stop 22_4
               if(lbound(dt(1,i)%i,1) /= 1)           error stop 23_4
               if(ubound(dt(2,i)%i,1) /= 2)           error stop 24_4
             end do
          case default
            error stop 101_4
       end select
    end subroutine
    subroutine checkSpread3(dt,dim)
       type(dtp(2,2,*,*)),intent(in) :: dt(:,:,:)
       integer,intent(in)  :: dim
       integer  :: i

       print *," in checkSpread3,dim= ",dim

       if(dt%k1 /= 2 .or. dt%k2 /= 2)                  error stop 25_4
       if(dt%l1 /= 1 .or. dt%l2 /= 2)                  error stop 26_4
       select case(dim)
          case(1)
             do i=1,size(dt,1)
               if(any(dt(i,1,1)%i /= [1,2]))           error stop 27_4
               if(any(dt(i,2,1)%i /= [1,2]))           error stop 28_4
               if(any(dt(i,1,1)%i /= [1,2]))           error stop 27_4
               if(any(dt(i,2,1)%i /= [1,2]))           error stop 28_4

               if(lbound(dt(i,1,1)%i,1) /= 1)          error stop 29_4
               if(ubound(dt(i,2,1)%i,1) /= 2)          error stop 30_4
               if(lbound(dt(i,1,2)%i,1) /= 1)          error stop 31_4
               if(ubound(dt(i,2,2)%i,1) /= 2)          error stop 32_4
             end do
          case(2)
             do i=1,size(dt,2)
               if(any(dt(1,i,1)%i /= [1,2]))           error stop 33_4
               if(any(dt(2,i,1)%i /= [1,2]))           error stop 34_4
               if(any(dt(1,i,1)%i /= [1,2]))           error stop 35_4
               if(any(dt(2,i,1)%i /= [1,2]))           error stop 36_4

               if(lbound(dt(1,i,1)%i,1) /= 1)          error stop 37_4
               if(ubound(dt(2,i,1)%i,1) /= 2)          error stop 38_4
               if(lbound(dt(1,i,2)%i,1) /= 1)          error stop 39_4
               if(ubound(dt(2,i,2)%i,1) /= 2)          error stop 40_4
             end do
          case(3)
             do i=1,size(dt,3)
               if(any(dt(1,1,i)%i /= [1,2]))           error stop 41_4
               if(any(dt(2,1,i)%i /= [1,2]))           error stop 42_4
               if(any(dt(1,1,i)%i /= [1,2]))           error stop 43_4
               if(any(dt(2,1,i)%i /= [1,2]))           error stop 44_4

               if(lbound(dt(1,1,i)%i,1) /= 1)          error stop 45_4
               if(ubound(dt(2,1,i)%i,1) /= 2)          error stop 46_4
               if(lbound(dt(1,2,i)%i,1) /= 1)          error stop 47_4
               if(ubound(dt(2,2,i)%i,1) /= 2)          error stop 48_4
             end do
          case default
            error stop 102_4
       end select
    end subroutine
end module

program spreadAsActualArg01
  use m
  implicit none

  type(dtp(2,2,:,:)),allocatable :: dtp1

  integer :: dim

  allocate(dtp(2,2,1,2) :: dtp1)
  dtp1%i=[1,2]

  call dtp1%spread(spread(dtp1,1,2),dim=1)
  call dtp1%spread(spread(spread(dtp1,1,2),1,3),dim=1)
  call dtp1%spread(spread(spread(dtp1,1,2),1,3),dim=2)
  call dtp1%spread(spread( spread( spread(dtp1,1,2), 1,2),1,2),dim=1 )
  call dtp1%spread(spread( spread( spread(dtp1,1,2), 1,2),1,2),dim=2 )
  call dtp1%spread(spread( spread( spread(dtp1,1,2), 1,2),1,2),dim=3 )
end program
