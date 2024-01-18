!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : spreadSourceIsArrayIntComp01.f
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
!*  2. SOURCE IS ARRAY
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp(k,l)
     integer(2),kind :: k
     integer,len     :: l
     integer(k)      :: i(l)
   end type

end module

program spreadSourceIsArrayIntComp01
  use m
  implicit none

  type(dtp(1,2)) :: dtp1(2:5)=[dtp(1,2)(i=[1,2]),dtp(1,2)(i=[3,4]), &
                    dtp(1,2)(i=[5,6]),dtp(1,2)(i=[7,8])]

  type(dtp(1,2)),allocatable :: dtp2(:,:)

  allocate(dtp2(2,2),source=reshape(dtp1,(/2,2/)) )

  call verify1(spread(dtp1,1,5)) ! dim is 1
  !   spread(..) becomes ...
  !   | dtp1(2), dtp1(3), dtp1(4), dtp1(5) |
  !   | dtp1(2), dtp1(3), dtp1(4), dtp1(5) |
  !   | dtp1(2), dtp1(3), dtp1(4), dtp1(5) |
  !   | dtp1(2), dtp1(3), dtp1(4), dtp1(5) |
  !   | dtp1(2), dtp1(3), dtp1(4), dtp1(5) |


  call verify2(spread(dtp1,2,5)) ! dim is 2
  !   spread(..) becomes ...
  !   | dtp1(2), dtp1(2), dtp1(2), dtp1(2), dtp1(2) |
  !   | dtp1(3), dtp1(3), dtp1(3), dtp1(3), dtp1(3) |
  !   | dtp1(4), dtp1(4), dtp1(4), dtp1(4), dtp1(4) |
  !   | dtp1(5), dtp1(5), dtp1(5), dtp1(5), dtp1(5) |


  !   dtp2 is
  !   | [1,2], [5,6] |
  !   | [3,4], [7,8] |
  !   dtp2(1,1) - [1,2]
  !   dtp2(1,2) - [5,6]
  !   dtp2(2,1) - [3,4]
  !   dtp2(2,2) - [7,8]
  call verify3(spread(dtp2,1,5)) ! dim is 1
  !   shape is 5,2,2

  !   | [1,2], [5,6] |   dtp2(1,1) , dtp2(1,2)
  !   | [3,4], [7,8] |   dtp2(2,1) , dtp2(2,2)
  call verify4(spread(dtp2,2,5)) ! dim is 2
  !   shape is 2 5 2
  !   dt(1,x,1) - (x is 1 - 5) - [1,2]
  !   dt(1,x,2) - (x is 1 - 5) - [5,6]
  !   dt(2,x,1) - (x is 1 - 5) - [3,4]
  !   dt(2,x,2) - (x is 1 - 5) - [7,8]

  contains

     subroutine verify1(dt)
        type(dtp(1,*)),intent(in) :: dt(:,:)
        integer :: i
        ! element order:
        ! dt(1,1) - dtp1(2) - [1,2]
        ! dt(2,1) - dtp1(2)
        ! dt(3,1) - dtp1(2)
        ! dt(4,1) - dtp1(2)
        ! dt(5,1) - dtp1(2)
        ! dt(1,2) - dtp1(3) - [3,4]
        ! dt(2,2) - dtp1(3)
        ! dt(3,2) - dtp1(3)
        ! dt(4,2) - dtp1(3)
        ! dt(5,2) - dtp1(3)
        ! dt(1,3) - dtp1(4) - [5,6]
        ! dt(2,3) - dtp1(4)
        ! dt(3,3) - dtp1(4)
        ! dt(4,3) - dtp1(4)
        ! dt(5,3) - dtp1(4)
        ! dt(1,4) - dtp1(5) - [7,8]
        ! dt(2,4) - dtp1(5)
        ! dt(3,4) - dtp1(5)
        ! dt(4,4) - dtp1(5)
        ! dt(5,4) - dtp1(5)

        if(dt%k /= 1)                              error stop 10_4
        if(dt%l /= 2)                              error stop 11_4
        if(size(dt,1) /= 5)                        error stop 12_4
        if(size(dt,2) /= 4)                        error stop 13_4
        do i=1,5
          if(any(dt(i,1)%i /= [1,2]))              error stop 14_4
          if(any(dt(i,2)%i /= [3,4]))              error stop 15_4
          if(any(dt(i,3)%i /= [5,6]))              error stop 16_4
          if(any(dt(i,4)%i /= [7,8]))              error stop 17_4
        end do
     end subroutine

     subroutine verify2(dt)
        type(dtp(1,*)),intent(in) :: dt(:,:)
        integer :: i

        ! element order
        ! dt(1,1) - dtp1(2) - [1,2]
        ! dt(2,1) - dtp1(3) - [3,4]
        ! dt(3,1) - dtp1(4) - [5,6]
        ! dt(4,1) - dtp1(5) - [7,8]
        ! dt(1,2) - dtp1(2)
        ! dt(2,2) - dtp1(3)
        ! dt(3,2) - dtp1(4)
        ! dt(4,2) - dtp1(5)
        ! dt(1,3) - dtp1(2)
        ! dt(2,3) - dtp1(3)
        ! dt(3,3) - dtp1(4)
        ! dt(4,3) - dtp1(5)
        ! dt(1,4) - dtp1(2)
        ! dt(2,4) - dtp1(3)
        ! dt(3,4) - dtp1(4)
        ! dt(4,4) - dtp1(5)
        ! dt(1,5) - dtp1(2)
        ! dt(2,5) - dtp1(3)
        ! dt(3,5) - dtp1(4)
        ! dt(4,5) - dtp1(5)
        if(dt%k /= 1)                              error stop 18_4
        if(dt%l /= 2)                              error stop 19_4
        if(size(dt,2) /= 5)                        error stop 20_4
        if(size(dt,1) /= 4)                        error stop 21_4
        do i=1,5
          if(any(dt(1,i)%i /= [1,2]))              error stop 22_4
          if(any(dt(2,i)%i /= [3,4]))              error stop 23_4
          if(any(dt(3,i)%i /= [5,6]))              error stop 24_4
          if(any(dt(4,i)%i /= [7,8]))              error stop 25_4
        end do
     end subroutine

    subroutine verify3(dt)
       type(dtp(1,*)),intent(in) :: dt(:,:,:)
       integer :: i
       ! element order:
       ! dt(1,1,1) - dtp2(1,1) - [1,2]
       ! dt(2,1,1) - dtp2(1,1)
       ! dt(3,1,1) - dtp2(1,1)
       ! dt(4,1,1) - dtp2(1,1)
       ! dt(5,1,1) - dtp2(1,1)

       ! dt(1,2,1) - dtp2(2,1) - [3,4]
       ! dt(2,2,1) - dtp2(2,1)
       ! dt(3,2,1) - dtp2(2,1)
       ! dt(4,2,1) - dtp2(2,1)
       ! dt(5,2,1) - dtp2(2,1)

       ! dt(1,1,2) - dtp2(1,2) - [5,6]
       ! dt(2,1,2) - dtp2(1,2)
       ! dt(3,1,2) - dtp2(1,2)
       ! dt(4,1,2) - dtp2(1,2)
       ! dt(5,1,2) - dtp2(1,2)

       ! dt(1,2,2) - dtp2(2,2) - [7,8]
       ! dt(2,2,2) - dtp2(2,2)
       ! dt(3,2,2) - dtp2(2,2)
       ! dt(4,2,2) - dtp2(2,2)
       ! dt(5,2,2) - dtp2(2,2)
       if(dt%k /= 1)                              error stop 26_4
       if(dt%l /= 2)                              error stop 27_4
       if(size(dt,1) /= 5)                        error stop 28_4
       if(size(dt,2) /= 2)                        error stop 29_4
       if(size(dt,3) /= 2)                        error stop 30_4

       do i=1,5
         if(any(dt(i,1,1)%i /= [1,2]))            error stop 31_4
         if(any(dt(i,2,1)%i /= [3,4]))            error stop 32_4
         if(any(dt(i,1,2)%i /= [5,6]))            error stop 33_4
         if(any(dt(i,2,2)%i /= [7,8]))            error stop 34_4
       end do
    end subroutine

    subroutine verify4(dt)
       type(dtp(1,*)),intent(in) :: dt(:,:,:)
       integer :: i

       ! element order:
       ! dt(1,1,1) - [1,2]
       ! dt(2,1,1) - [3,4]
       ! dt(1,2,1) - [1,2]
       ! dt(2,2,1) - [3,4]
       ! dt(1,3,1) - [1,2]
       ! dt(2,3,1) - [3,4]
       ! dt(1,4,1) - [1,2]
       ! dt(2,4,1) - [3,4]
       ! dt(1,5,1) - [1,2]
       ! dt(2,5,1) - [3,4]

       ! dt(1,1,2) - [5,6]
       ! dt(2,1,2) - [7,8]
       ! dt(1,2,2) - [5,6]
       ! dt(2,2,2) - [7,8]
       ! dt(1,3,2) - [5,6]
       ! dt(2,3,2) - [7,8]
       ! dt(1,4,2) - [5,6]
       ! dt(2,4,2) - [7,8]
       ! dt(1,5,2) - [5,6]
       ! dt(2,5,2) - [7,8]

       if(dt%k /= 1)                              error stop 35_4
       if(dt%l /= 2)                              error stop 36_4
       if(size(dt,1) /= 2)                        error stop 37_4
       if(size(dt,2) /= 5)                        error stop 38_4
       if(size(dt,3) /= 2)                        error stop 39_4

       do i= 1,5
            if(any(dt(1,i,1)%i /= [1,2]))         error stop 40_4
            if(any(dt(2,i,1)%i /= [3,4]))         error stop 41_4
            if(any(dt(1,i,2)%i /= [5,6]))         error stop 42_4
            if(any(dt(2,i,2)%i /= [7,8]))         error stop 43_4
       end do
    end subroutine
end program

