!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 17 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : SPREAD(SOURCE,DIM,NCOPIES)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. SECTION 13.7.114
!*  2. SOURCE IS POLYMORPHIC ARRAY AND HAS INTEGER ARRAY COMPONENT
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l1)
     integer,len     :: l1
     integer,allocatable :: i1(:)
   end type
   type,extends(base) :: child(k,l2)
     integer(8),kind  :: k
     integer,len      :: l2
     integer(k),pointer :: i2(:)
   end type

end module

program spreadSourceIsArrayIntComp02
  use m
  implicit none

  class(base(:)),allocatable ::  b1(:)
  class(base(:)),allocatable ::  b2(:,:)

  integer(2),target :: i2_1(3)=[3,4,5]
  integer(2),target :: i2_2(3)=[-3,-4,-5]
  integer(2),target :: i2_3(3)=[33,44,55]
  integer(2),target :: i2_4(3)=[-33,-44,-55]


  allocate(b1(2:5),source=[child(2,2,3)([1,2],i2_1),&
                           child(2,2,3)([-1,-2],i2_2), &
                           child(2,2,3)([11,22],i2_3),&
                           child(2,2,3)([-11,-22],i2_4) ] )


  allocate(b2(2,2),source=reshape(b1,(/2,2/)) )

  call verify1(spread(b1,1,5)) ! dim is 1
  !   spread(..) becomes ...
  !   | b1(2), b1(3), b1(4), b1(5) |
  !   | b1(2), b1(3), b1(4), b1(5) |
  !   | b1(2), b1(3), b1(4), b1(5) |
  !   | b1(2), b1(3), b1(4), b1(5) |
  !   | b1(2), b1(3), b1(4), b1(5) |


  call verify2(spread(b1,2,5)) ! dim is 2
  !   spread(..) becomes ...
  !   | b1(2), b1(2), b1(2), b1(2), b1(2) |
  !   | b1(3), b1(3), b1(3), b1(3), b1(3) |
  !   | b1(4), b1(4), b1(4), b1(4), b1(4) |
  !   | b1(5), b1(5), b1(5), b1(5), b1(5) |


  !   b2 is
  !   | (i1=[1,2],i2=[3,4,5])     , (i1=[11,22],i2=[33,44,55])      |
  !   | (i1=[-1,-2],i2=[-3,-4,-5]), (i1=[-11,-22],i2=[-33,-44,-55]) |
  !   b2(1,1) - (i1=[1,2],i2=[3,4,5])
  !   b2(1,2) - (i1=[11,22],i2=[33,44,55])
  !   b2(2,1) - (i1=[-1,-2],i2=[-3,-4,-5])
  !   b2(2,2) - (i1=[-11,-22],i2=[-33,-44,-55])

  call verify3(spread(b2,1,5)) ! dim is 1
  !   shape is 5,2,2


  call verify4(spread(b2,2,5)) ! dim is 2
  !   shape is 2 5 2
  !   dt(1,x,1) - (x is 1 - 5) - (i1=[1,2],i2=[3,4,5])
  !   dt(1,x,2) - (x is 1 - 5) - (i1=[11,22],i2=[33,44,55])
  !   dt(2,x,1) - (x is 1 - 5) - (i1=[-1,-2],i2=[-3,-4,-5])
  !   dt(2,x,2) - (x is 1 - 5) - (i1=[-11,-22],i2=[-33,-44,-55])

  contains

     subroutine verify1(dt)
        class(base(*)),intent(in) :: dt(:,:)
        integer :: i
        ! element order:
        ! dt(1,1) - b1(2) - (i1=[1,2],i2=[3,4,5])
        ! dt(2,1) - b1(2)
        ! dt(3,1) - b1(2)
        ! dt(4,1) - b1(2)
        ! dt(5,1) - b1(2)
        ! dt(1,2) - b1(3) - (i1=[-1,-2],i2=[-3,-4,-5])
        ! dt(2,2) - b1(3)
        ! dt(3,2) - b1(3)
        ! dt(4,2) - b1(3)
        ! dt(5,2) - b1(3)
        ! dt(1,3) - b1(4) - (i1=[11,22],i2=[33,44,55])
        ! dt(2,3) - b1(4)
        ! dt(3,3) - b1(4)
        ! dt(4,3) - b1(4)
        ! dt(5,3) - b1(4)
        ! dt(1,4) - b1(5) - (i1=[-11,-22],i2=[-33,-44,-55])
        ! dt(2,4) - b1(5)
        ! dt(3,4) - b1(5)
        ! dt(4,4) - b1(5)
        ! dt(5,4) - b1(5)

        if(dt%l1 /= 2)                               error stop 10_4
        select type(dt)
          type is(child(*,2,*))
            if(dt%k  /= 2)                           error stop 11_4
            if(dt%l2 /= 3)                           error stop 12_4
            if(size(dt,1) /= 5)                      error stop 13_4
            if(size(dt,2) /= 4)                      error stop 14_4
            do i=1,5
              if(any(dt(i,1)%i1 /= [1,2]))           error stop 15_4
              if(any(dt(i,1)%i2 /= [3,4,5]))         error stop 16_4

              if(any(dt(i,2)%i1 /= [-1,-2]))         error stop 17_4
              if(any(dt(i,2)%i2 /= [-3,-4,-5]))      error stop 18_4

              if(any(dt(i,3)%i1 /= [11,22]))         error stop 20_4
              if(any(dt(i,3)%i2 /= [33,44,55]))      error stop 21_4

              if(any(dt(i,4)%i1 /= [-11,-22]))       error stop 22_4
              if(any(dt(i,4)%i2 /= [-33,-44,-55]))   error stop 23_4
            end do
           class default
              error stop 100_4
        end select
     end subroutine

     subroutine verify2(dt)
        class(base(*)),intent(in) :: dt(:,:)
        integer :: i

        ! element order
        ! dt(1,1) - b1(2) - (i1=[1,2],i2=[3,4,5])
        ! dt(2,1) - b1(3) - (i1=[-1,-2],i2=[-3,-4,-5])
        ! dt(3,1) - b1(4) - (i1=[11,22],i2=[33,44,55])
        ! dt(4,1) - b1(5) - (i1=[-11,-22],i2=[-33,-44,-55])
        ! dt(1,2) - b1(2)
        ! dt(2,2) - b1(3)
        ! dt(3,2) - b1(4)
        ! dt(4,2) - b1(5)
        ! dt(1,3) - b1(2)
        ! dt(2,3) - b1(3)
        ! dt(3,3) - b1(4)
        ! dt(4,3) - b1(5)
        ! dt(1,4) - b1(2)
        ! dt(2,4) - b1(3)
        ! dt(3,4) - b1(4)
        ! dt(4,4) - b1(5)
        ! dt(1,5) - b1(2)
        ! dt(2,5) - b1(3)
        ! dt(3,5) - b1(4)
        ! dt(4,5) - b1(5)
        if(dt%l1 /= 2)                               error stop 24_4
        select type(dt)
          type is(child(*,2,*))
            if(dt%k  /= 2)                           error stop 25_4
            if(dt%l2 /= 3)                           error stop 26_4
            if(size(dt,1) /= 4)                      error stop 27_4
            if(size(dt,2) /= 5)                      error stop 28_4
            do i=1,5
              if(any(dt(1,i)%i1 /= [1,2]))           error stop 29_4
              if(any(dt(1,i)%i2 /= [3,4,5]))         error stop 30_4

              if(any(dt(2,i)%i1 /= [-1,-2]))         error stop 31_4
              if(any(dt(2,i)%i2 /= [-3,-4,-5]))      error stop 32_4

              if(any(dt(3,i)%i1 /= [11,22]))         error stop 33_4
              if(any(dt(3,i)%i2 /= [33,44,55]))      error stop 34_4

              if(any(dt(4,i)%i1 /= [-11,-22]))       error stop 35_4
              if(any(dt(4,i)%i2 /= [-33,-44,-55]))   error stop 36_4
            end do
          class default
             error stop 101_4
        end select
     end subroutine

    subroutine verify3(dt)
       class(base(*)),intent(in) :: dt(:,:,:)
       integer :: i
       ! element order:
       ! dt(1,1,1) - b2(1,1) - (i1=[1,2],i2=[3,4,5])
       ! dt(2,1,1) - b2(1,1)
       ! dt(3,1,1) - b2(1,1)
       ! dt(4,1,1) - b2(1,1)
       ! dt(5,1,1) - b2(1,1)

       ! dt(1,2,1) - b2(2,1) - (i1=[-1,-2],i2=[-3,-4,-5])
       ! dt(2,2,1) - b2(2,1)
       ! dt(3,2,1) - b2(2,1)
       ! dt(4,2,1) - b2(2,1)
       ! dt(5,2,1) - b2(2,1)

       ! dt(1,1,2) - b2(1,2) - (i1=[11,22],i2=[33,44,55])
       ! dt(2,1,2) - b2(1,2)
       ! dt(3,1,2) - b2(1,2)
       ! dt(4,1,2) - b2(1,2)
       ! dt(5,1,2) - b2(1,2)

       ! dt(1,2,2) - b2(2,2) - (i1=[-11,-22],i2=[-33,-44,-55])
       ! dt(2,2,2) - b2(2,2)
       ! dt(3,2,2) - b2(2,2)
       ! dt(4,2,2) - b2(2,2)
       ! dt(5,2,2) - b2(2,2)

       if(dt%l1 /= 2)                                  error stop 37_4
        select type(dt)
          type is(child(*,2,*))
            if(dt%k  /= 2)                             error stop 38_4
            if(dt%l2 /= 3)                             error stop 39_4
            if(size(dt,1) /= 5)                        error stop 40_4
            if(size(dt,2) /= 2)                        error stop 41_4
            if(size(dt,3) /= 2)                        error stop 42_4

            do i=1,5
              if(any(dt(i,1,1)%i1 /= [1,2]))           error stop 43_4
              if(any(dt(i,1,1)%i2 /= [3,4,5]))         error stop 44_4

              if(any(dt(i,2,1)%i1 /= [-1,-2]))         error stop 45_4
              if(any(dt(i,2,1)%i2 /= [-3,-4,-5]))      error stop 46_4

              if(any(dt(i,1,2)%i1 /= [11,22]))         error stop 47_4
              if(any(dt(i,1,2)%i2 /= [33,44,55]))      error stop 48_4

              if(any(dt(i,2,2)%i1 /= [-11,-22]))       error stop 49_4
              if(any(dt(i,2,2)%i2 /= [-33,-44,-55]))   error stop 50_4
            end do
            class default
              error stop 102_4
        end select

    end subroutine

    subroutine verify4(dt)
       class(base(*)),intent(in) :: dt(:,:,:)
       integer :: i

       ! element order:
       ! dt(1,1,1) - (i1=[1,2],i2=[3,4,5])
       ! dt(2,1,1) - (i1=[-1,-2],i2=[-3,-4,-5])
       ! dt(1,2,1) -
       ! dt(2,2,1) -
       ! dt(1,3,1) -
       ! dt(2,3,1) -
       ! dt(1,4,1) -
       ! dt(2,4,1) -
       ! dt(1,5,1) -
       ! dt(2,5,1) -

       ! dt(1,1,2) - (i1=[11,22],i2=[33,44,55])
       ! dt(2,1,2) - (i1=[-11,-22],i2=[-33,-44,-55])
       ! dt(1,2,2) -
       ! dt(2,2,2) -
       ! dt(1,3,2) -
       ! dt(2,3,2) -
       ! dt(1,4,2) -
       ! dt(2,4,2) -
       ! dt(1,5,2) -
       ! dt(2,5,2) -

       if(dt%l1 /= 2 )                                 error stop 51_4

        select type(dt)
          type is(child(*,2,*))
            if(dt%k  /= 2)                             error stop 52_4
            if(dt%l2 /= 3)                             error stop 53_4
            if(size(dt,1) /= 2)                        error stop 54_4
            if(size(dt,2) /= 5)                        error stop 55_4
            if(size(dt,3) /= 2)                        error stop 56_4

            do i=1,5
              if(any(dt(1,i,1)%i1 /= [1,2]))           error stop 57_4
              if(any(dt(1,i,1)%i2 /= [3,4,5]))         error stop 58_4

              if(any(dt(2,i,1)%i1 /= [-1,-2]))         error stop 59_4
              if(any(dt(2,i,1)%i2 /= [-3,-4,-5]))      error stop 60_4

              if(any(dt(1,i,2)%i1 /= [11,22]))         error stop 61_4
              if(any(dt(1,i,2)%i2 /= [33,44,55]))      error stop 62_4

              if(any(dt(2,i,2)%i1 /= [-11,-22]))       error stop 64_4
              if(any(dt(2,i,2)%i2 /= [-33,-44,-55]))   error stop 64_4
            end do
           class default
             error stop 103_4
        end select

    end subroutine
end program

