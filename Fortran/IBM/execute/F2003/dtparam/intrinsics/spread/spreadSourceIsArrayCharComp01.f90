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
!*  2. SOURCE IS ARRAY,COMPONENT IS CHARACTER
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp(k,l)
     integer(2),kind :: k
     integer,len     :: l
     character(l)    :: ch(k)
   end type

end module

program spreadSourceIsArrayCharComp01
  use m
  implicit none

  type(dtp(2,3)) :: dtp1(2:5)=[dtp(2,3)(ch=["111","222"]), &
                               dtp(2,3)(ch=["333","444"]), &
                               dtp(2,3)(ch=["555","666"]), &
                               dtp(2,3)(ch=["777","888"])]

  type(dtp(2,:)),allocatable :: dtp2(:,:)

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
  !   | ["111","222"], ["555","666"] |
  !   | ["333","444"], ["777","888"] |
  !   dtp2(1,1) - ["111","222"]
  !   dtp2(1,2) - ["333","444"]
  !   dtp2(2,1) - ["555","666"]
  !   dtp2(2,2) - ["777","888"]
  call verify3(spread(dtp2,1,5)) ! dim is 1
  !   shape is 5,2,2

  !   | ["111","222"], ["555","666"] |   dtp2(1,1) , dtp2(1,2)
  !   | ["333","444"], ["777","888"] |   dtp2(2,1) , dtp2(2,2)
  call verify4(spread(dtp2,2,5)) ! dim is 2
  !   shape is 2 5 2
  !   dt(1,x,1) - (x is 1 - 5) - ["111","222"]
  !   dt(1,x,2) - (x is 1 - 5) - ["555","666"]
  !   dt(2,x,1) - (x is 1 - 5) - ["333","444"]
  !   dt(2,x,2) - (x is 1 - 5) - ["777","888"]

  contains

     subroutine verify1(dt)
        type(dtp(2,*)),intent(in) :: dt(:,:)
        integer :: i
        ! element order:
        ! dt(1,1) - dtp1(2) - ["111","222"]
        ! dt(2,1) - dtp1(2)
        ! dt(3,1) - dtp1(2)
        ! dt(4,1) - dtp1(2)
        ! dt(5,1) - dtp1(2)
        ! dt(1,2) - dtp1(3) - ["333","444"]
        ! dt(2,2) - dtp1(3)
        ! dt(3,2) - dtp1(3)
        ! dt(4,2) - dtp1(3)
        ! dt(5,2) - dtp1(3)
        ! dt(1,3) - dtp1(4) - ["555","666"]
        ! dt(2,3) - dtp1(4)
        ! dt(3,3) - dtp1(4)
        ! dt(4,3) - dtp1(4)
        ! dt(5,3) - dtp1(4)
        ! dt(1,4) - dtp1(5) - ["777","888"]
        ! dt(2,4) - dtp1(5)
        ! dt(3,4) - dtp1(5)
        ! dt(4,4) - dtp1(5)
        ! dt(5,4) - dtp1(5)

        if(dt%k /= 2)                              error stop 10_4
        if(dt%l /= 3)                              error stop 11_4
        if(size(dt,1) /= 5)                        error stop 12_4
        if(size(dt,2) /= 4)                        error stop 13_4
        do i=1,5
          if(any(dt(i,1)%ch /= ["111","222"]))     error stop 14_4
          if(any(dt(i,2)%ch /= ["333","444"]))     error stop 15_4
          if(any(dt(i,3)%ch /= ["555","666"]))     error stop 16_4
          if(any(dt(i,4)%ch /= ["777","888"]))     error stop 17_4
        end do
     end subroutine

     subroutine verify2(dt)
        type(dtp(2,*)),intent(in) :: dt(:,:)
        integer :: i

        ! element order
        ! dt(1,1) - dtp1(2) - ["111","222"]
        ! dt(2,1) - dtp1(3) - ["333","444"]
        ! dt(3,1) - dtp1(4) - ["555","666"]
        ! dt(4,1) - dtp1(5) - ["777","888"]
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
        if(dt%k /= 2)                              error stop 18_4
        if(dt%l /= 3)                              error stop 19_4
        if(size(dt,2) /= 5)                        error stop 20_4
        if(size(dt,1) /= 4)                        error stop 21_4
        do i=1,5
          if(any(dt(1,i)%ch /= ["111","222"]))     error stop 22_4
          if(any(dt(2,i)%ch /= ["333","444"]))     error stop 23_4
          if(any(dt(3,i)%ch /= ["555","666"]))     error stop 24_4
          if(any(dt(4,i)%ch /= ["777","888"]))     error stop 25_4
        end do
     end subroutine

    subroutine verify3(dt)
       type(dtp(2,*)),intent(in) :: dt(:,:,:)
       integer :: i
       ! element order:
       ! dt(1,1,1) - dtp2(1,1) - ["111","222"]
       ! dt(2,1,1) - dtp2(1,1)
       ! dt(3,1,1) - dtp2(1,1)
       ! dt(4,1,1) - dtp2(1,1)
       ! dt(5,1,1) - dtp2(1,1)

       ! dt(1,2,1) - dtp2(2,1) - ["333","444"]
       ! dt(2,2,1) - dtp2(2,1)
       ! dt(3,2,1) - dtp2(2,1)
       ! dt(4,2,1) - dtp2(2,1)
       ! dt(5,2,1) - dtp2(2,1)

       ! dt(1,1,2) - dtp2(1,2) - ["555","666"]
       ! dt(2,1,2) - dtp2(1,2)
       ! dt(3,1,2) - dtp2(1,2)
       ! dt(4,1,2) - dtp2(1,2)
       ! dt(5,1,2) - dtp2(1,2)

       ! dt(1,2,2) - dtp2(2,2) - ["777","888"]
       ! dt(2,2,2) - dtp2(2,2)
       ! dt(3,2,2) - dtp2(2,2)
       ! dt(4,2,2) - dtp2(2,2)
       ! dt(5,2,2) - dtp2(2,2)
       if(dt%k /= 2)                              error stop 26_4
       if(dt%l /= 3)                              error stop 27_4
       if(size(dt,1) /= 5)                        error stop 28_4
       if(size(dt,2) /= 2)                        error stop 29_4
       if(size(dt,3) /= 2)                        error stop 30_4

       do i=1,5
         if(any(dt(i,1,1)%ch /= ["111","222"]))   error stop 31_4
         if(any(dt(i,2,1)%ch /= ["333","444"]))   error stop 32_4
         if(any(dt(i,1,2)%ch /= ["555","666"]))   error stop 33_4
         if(any(dt(i,2,2)%ch /= ["777","888"]))   error stop 34_4
       end do
    end subroutine

    subroutine verify4(dt)
       type(dtp(2,*)),intent(in) :: dt(:,:,:)
       integer :: i

       ! element order:
       ! dt(1,1,1) - ["111","222"]
       ! dt(2,1,1) - ["333","444"]
       ! dt(1,2,1) - ["111","222"]
       ! dt(2,2,1) - ["333","444"]
       ! dt(1,3,1) - ["111","222"]
       ! dt(2,3,1) - ["333","444"]
       ! dt(1,4,1) - ["111","222"]
       ! dt(2,4,1) - ["333","444"]
       ! dt(1,5,1) - ["111","222"]
       ! dt(2,5,1) - ["333","444"]

       ! dt(1,1,2) - ["555","666"]
       ! dt(2,1,2) - ["777","888"]
       ! dt(1,2,2) - ["555","666"]
       ! dt(2,2,2) - ["777","888"]
       ! dt(1,3,2) - ["555","666"]
       ! dt(2,3,2) - ["777","888"]
       ! dt(1,4,2) - ["555","666"]
       ! dt(2,4,2) - ["777","888"]
       ! dt(1,5,2) - ["555","666"]
       ! dt(2,5,2) - ["777","888"]

       if(dt%k /= 2)                              error stop 35_4
       if(dt%l /= 3)                              error stop 36_4
       if(size(dt,1) /= 2)                        error stop 37_4
       if(size(dt,2) /= 5)                        error stop 38_4
       if(size(dt,3) /= 2)                        error stop 39_4

       do i= 1,5
            if(any(dt(1,i,1)%ch /= ["111","222"]))     error stop 40_4
            if(any(dt(2,i,1)%ch /= ["333","444"]))     error stop 41_4
            if(any(dt(1,i,2)%ch /= ["555","666"]))     error stop 42_4
            if(any(dt(2,i,2)%ch /= ["777","888"]))     error stop 43_4
       end do
    end subroutine
end program

