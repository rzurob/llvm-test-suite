!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : spreadSourceIsArrayCharComp02.f
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
!*  2. SOURCE IS POLYMORPHIC ARRAY AND HAS CHARACTER AND UNLIMITED POLYMORPHIC  COMPONENT
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l1)
     integer,len     :: l1
     class(*),pointer :: c1(:)
   end type
   type,extends(base) :: child(k,l2)
     integer(8),kind  :: k
     integer,len      :: l2
     character(l2),pointer :: c2(:)
   end type

end module

program spreadSourceIsArrayCharComp02
  use m
  implicit none

  class(base(:)),pointer ::  b1(:)
  class(base(:)),pointer ::  b2(:,:)

  character(3),target :: c2_1(3)=["3","4","5"]
  character(3),target :: c2_2(3)=["-3","-4","-5"]
  character(3),target :: c2_3(3)=["33","44","55"]
  character(3),target :: c2_4(3)=["-33","-44","-55"]


  allocate(b1(2:5),source=[child(2,2,3)(null(),c2_1),&
                           child(2,2,3)(null(),c2_2), &
                           child(2,2,3)(null(),c2_3),&
                           child(2,2,3)(null(),c2_4) ] )
  select type(x=>b1)
     type is(child(*,2,*))
       allocate(x(2)%c1(2),source=["1","2"])
       allocate(x(3)%c1(2),source=["-1","-2"])
       allocate(x(4)%c1(2),source=["11","22"])
       allocate(x(5)%c1(2),source=["-11","-22"])
  end select

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
  !   b2(1,1) - (c1=["1","2"],c2=["3","4","5"])
  !   b2(1,2) - (c1=["11","22"],c2=["33","44","55"])
  !   b2(2,1) - (c1=["-1","-2"],c2=["-3","-4","-5"])
  !   b2(2,2) - (c1=["-11","-22"],c2=["-33","-44","-55"])

  call verify3(spread(b2,1,5)) ! dim is 1
  !   shape is 5,2,2


  call verify4(spread(b2,2,5)) ! dim is 2
  !   shape is 2 5 2
  !   dt(1,x,1) - (x is 1 - 5) - (c1=["1","2"],c2=["3","4","5"])
  !   dt(1,x,2) - (x is 1 - 5) - (c1=["11","22"],c2=["33","44","55"])
  !   dt(2,x,1) - (x is 1 - 5) - (c1=["-1","-2"],c2=["-3","-4","-5"])
  !   dt(2,x,2) - (x is 1 - 5) - (c1=["-11","-22"],c2=["-33","-44","-55"])

  contains

     subroutine verify1(dt)
        class(base(*)),intent(in) :: dt(:,:)
        integer :: i
        ! element order:
        ! dt(1,1) - b1(2) - (c1=["1","2"],c2=["3","4","5"])
        ! dt(2,1) - b1(2)
        ! dt(3,1) - b1(2)
        ! dt(4,1) - b1(2)
        ! dt(5,1) - b1(2)
        ! dt(1,2) - b1(3) - (c1=["-1","-2"],c2=["-3","-4","-5"])
        ! dt(2,2) - b1(3)
        ! dt(3,2) - b1(3)
        ! dt(4,2) - b1(3)
        ! dt(5,2) - b1(3)
        ! dt(1,3) - b1(4) - (c1=["11","22"],c2=["33","44","55"])
        ! dt(2,3) - b1(4)
        ! dt(3,3) - b1(4)
        ! dt(4,3) - b1(4)
        ! dt(5,3) - b1(4)
        ! dt(1,4) - b1(5) - (c1=["-11","-22"],c2=["-33","-44","-55"])
        ! dt(2,4) - b1(5)
        ! dt(3,4) - b1(5)
        ! dt(4,4) - b1(5)
        ! dt(5,4) - b1(5)

        if(dt%l1 /= 2)                                     error stop 10_4
        select type(dt)
          type is(child(*,2,*))
            if(dt%k  /= 2)                                 error stop 11_4
            if(dt%l2 /= 3)                                 error stop 12_4
            if(size(dt,1) /= 5)                            error stop 13_4
            if(size(dt,2) /= 4)                            error stop 14_4
            do i=1,5
              if(any(dt(i,1)%c2 /= ["3","4","5"]))         error stop 15_4
              if(any(dt(i,2)%c2 /= ["-3","-4","-5"]))      error stop 16_4
              if(any(dt(i,3)%c2 /= ["33","44","55"]))      error stop 17_4
              if(any(dt(i,4)%c2 /= ["-33","-44","-55"]))   error stop 18_4

              select type(y=>dt(i,1)%c1)
                  type is(character(*))
                    if(any(y /= ["1","2"]))              error stop 19_4
                  class default
                     error stop 201_4
              end select

              select type(y=>dt(i,2)%c1)
                  type is(character(*))
                    if(any(y /= ["-1","-2"]))            error stop 20_4
                  class default
                    error stop 202_4
              end select

              select type(y=>dt(i,3)%c1)
                  type is(character(*))
                    if(any(y /= ["11","22"]))            error stop 21_4
                  class default
                    error stop 203_4
              end select

              select type(y=>dt(i,4)%c1)
                  type is(character(*))
                    if(any(y /= ["-11","-22"]))          error stop 22_4
                  class default
                    error stop 204_4
              end select

            end do
          class default
             error stop 200_4
        end select
     end subroutine

     subroutine verify2(dt)
        class(base(*)),intent(in) :: dt(:,:)
        integer :: i

        ! element order
        ! dt(1,1) - b1(2) - (c1=["1","2"],c2=["3","4","5"])
        ! dt(2,1) - b1(3) - (c1=["-1","-2"],c2=["-3","-4","-5"])
        ! dt(3,1) - b1(4) - (c1=["11","22"],c2=["33","44","55"])
        ! dt(4,1) - b1(5) - (c1=["-11","-22"],c2=["-33","-44","-55"])
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
        if(dt%l1 /= 2)                                     error stop 23_4
        select type(dt)
          type is(child(*,2,*))
            if(dt%k  /= 2)                                 error stop 24_4
            if(dt%l2 /= 3)                                 error stop 25_4
            if(size(dt,1) /= 4)                            error stop 26_4
            if(size(dt,2) /= 5)                            error stop 27_4
            do i=1,5
              if(any(dt(1,i)%c2 /= ["3","4","5"]))         error stop 28_4
              if(any(dt(2,i)%c2 /= ["-3","-4","-5"]))      error stop 29_4
              if(any(dt(3,i)%c2 /= ["33","44","55"]))      error stop 30_4
              if(any(dt(4,i)%c2 /= ["-33","-44","-55"]))   error stop 31_4

              select type(y=>dt(1,i)%c1)
                  type is(character(*))
                    if(any(y /= ["1","2"]))                 error stop 32_4
                  class default
                    error stop 206_4
              end select

              select type(y=>dt(2,i)%c1)
                  type is(character(*))
                    if(any(y /= ["-1","-2"]))               error stop 33_4
                  class default
                    error stop 207_4
              end select

              select type(y=>dt(3,i)%c1)
                  type is(character(*))
                    if(any(y /= ["11","22"]))               error stop 34_4
                  class default
                     error stop 208_4
              end select

              select type(y=>dt(4,i)%c1)
                  type is(character(*))
                    if(any(y /= ["-11","-22"]))             error stop 35_4
                  class default
                    error stop 209_4
              end select

            end do
          class default
            error stop 205_4
        end select
     end subroutine

    subroutine verify3(dt)
       class(base(*)),intent(in) :: dt(:,:,:)
       integer :: i
       ! element order:
       ! dt(1,1,1) - b2(1,1) - (c1=["1","2"],c2=["3","4","5"])
       ! dt(2,1,1) - b2(1,1)
       ! dt(3,1,1) - b2(1,1)
       ! dt(4,1,1) - b2(1,1)
       ! dt(5,1,1) - b2(1,1)

       ! dt(1,2,1) - b2(2,1) - (c1=["-1","-2"],c2=["-3","-4","-5"])
       ! dt(2,2,1) - b2(2,1)
       ! dt(3,2,1) - b2(2,1)
       ! dt(4,2,1) - b2(2,1)
       ! dt(5,2,1) - b2(2,1)

       ! dt(1,1,2) - b2(1,2) - (c1=["11","22"],c2=["33","44","55"])
       ! dt(2,1,2) - b2(1,2)
       ! dt(3,1,2) - b2(1,2)
       ! dt(4,1,2) - b2(1,2)
       ! dt(5,1,2) - b2(1,2)

       ! dt(1,2,2) - b2(2,2) - (c1=["-11","-22"],c2=["-33","-44","-55"])
       ! dt(2,2,2) - b2(2,2)
       ! dt(3,2,2) - b2(2,2)
       ! dt(4,2,2) - b2(2,2)
       ! dt(5,2,2) - b2(2,2)

       if(dt%l1 /= 2)                                        error stop 36_4
        select type(dt)
          type is(child(*,2,*))
            if(dt%k  /= 2)                                   error stop 37_4
            if(dt%l2 /= 3)                                   error stop 38_4
            if(size(dt,1) /= 5)                              error stop 39_4
            if(size(dt,2) /= 2)                              error stop 40_4
            if(size(dt,3) /= 2)                              error stop 41_4

            do i=1,5
              if(any(dt(i,1,1)%c2 /= ["3","4","5"]))         error stop 42_4
              if(any(dt(i,2,1)%c2 /= ["-3","-4","-5"]))      error stop 43_4
              if(any(dt(i,1,2)%c2 /= ["33","44","55"]))      error stop 44_4
              if(any(dt(i,2,2)%c2 /= ["-33","-44","-55"]))   error stop 45_4

              select type(y=>dt(i,1,1)%c1)
                  type is(character(*))
                    if(any(y /= ["1","2"]))                  error stop 46_4
                  class default
                     error stop 212_4
              end select

              select type(y=>dt(i,2,1)%c1)
                  type is(character(*))
                    if(any(y /= ["-1","-2"]))                error stop 47_4
                  class default
                    error stop 213_4
              end select

              select type(y=>dt(i,1,2)%c1)
                  type is(character(*))
                    if(any(y /= ["11","22"]))                error stop 48_4
                  class default
                    error stop 214_4
              end select

              select type(y=>dt(i,2,2)%c1)
                  type is(character(*))
                    if(any(y /= ["-11","-22"]))              error stop 49_4
                  class default
                   error stop 215_4
              end select

            end do
            class default
               error stop 211_4
        end select

    end subroutine

    subroutine verify4(dt)
       class(base(*)),intent(in) :: dt(:,:,:)
       integer :: i

       ! element order:
       ! dt(1,1,1) - (c1=["1","2"],c2=["3","4","5"])
       ! dt(2,1,1) - (c1=["-1","-2"],c2=["-3","-4","-5"])
       ! dt(1,2,1) -
       ! dt(2,2,1) -
       ! dt(1,3,1) -
       ! dt(2,3,1) -
       ! dt(1,4,1) -
       ! dt(2,4,1) -
       ! dt(1,5,1) -
       ! dt(2,5,1) -

       ! dt(1,1,2) - (c1=["11","22"],c2=["33","44","55"])
       ! dt(2,1,2) - (c1=["-11","-22"],c2=["-33","-44","-55"])
       ! dt(1,2,2) -
       ! dt(2,2,2) -
       ! dt(1,3,2) -
       ! dt(2,3,2) -
       ! dt(1,4,2) -
       ! dt(2,4,2) -
       ! dt(1,5,2) -
       ! dt(2,5,2) -

       if(dt%l1 /= 2 )                                        error stop 50_4

        select type(dt)
          type is(child(*,2,*))
            if(dt%k  /= 2)                                    error stop 51_4
            if(dt%l2 /= 3)                                    error stop 52_4
            if(size(dt,1) /= 2)                               error stop 53_4
            if(size(dt,2) /= 5)                               error stop 54_4
            if(size(dt,3) /= 2)                               error stop 55_4

            do i=1,5
              if(any(dt(1,i,1)%c2 /= ["3","4","5"]))          error stop 56_4
              if(any(dt(2,i,1)%c2 /= ["-3","-4","-5"]))       error stop 57_4
              if(any(dt(1,i,2)%c2 /= ["33","44","55"]))       error stop 58_4
              if(any(dt(2,i,2)%c2 /= ["-33","-44","-55"]))    error stop 59_4

              select type(y=>dt(1,i,1)%c1)
                  type is(character(*))
                    if(any(y /= ["1","2"]))                   error stop 60_4
                  class default
                    error stop 217_4
              end select

              select type(y=>dt(2,i,1)%c1)
                  type is(character(*))
                    if(any(y /= ["-1","-2"]))                 error stop 61_4
                  class default
                    error stop 218_4
              end select

              select type(y=>dt(1,i,2)%c1)
                  type is(character(*))
                    if(any(y /= ["11","22"]))                 error stop 62_4
                  class default
                    error stop 219_4
              end select

              select type(y=>dt(2,i,2)%c1)
                  type is(character(*))
                    if(any(y /= ["-11","-22"]))               error stop 63_4
                  class default
                     error stop 220_4
              end select
            end do
           class default
               error stop 216_4
        end select

    end subroutine
end program

