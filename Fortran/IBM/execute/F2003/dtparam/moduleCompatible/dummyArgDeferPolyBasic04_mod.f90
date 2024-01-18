module m
  type nest(l)
     integer,len  :: l
     character(l),allocatable :: c
  end type 
  type base(l1)
      integer,len  :: l1
      type(nest(:)),allocatable :: nest1
  end type

  type,extends(base) :: child(l2)
      integer,len  :: l2
      type(nest(:)),allocatable :: nest2
  end type

  contains

    subroutine sub1(arg)

      class(base(:)),allocatable, intent(inout) :: arg(:) 
      select type(arg)
         type is(child(*,*))
            call sub2(arg)
         class default
            error stop 50_4
      end select
    end subroutine

    subroutine sub2(arg)
      type(child(*,*)),intent(inout) :: arg(5:)

      call sub3(arg(5)%nest1,arg(5)%nest2,1)
      call sub3(arg(6)%nest1,arg(6)%nest2,2)
       
    end subroutine

    subroutine sub3(nest1,nest2,flag)

      type(nest(*)),intent(inout) :: nest1 
      type(nest(*)),intent(inout) :: nest2 
      integer,intent(in) :: flag 

      if(flag .eq. 1) then 
         if(nest1%c /= "abcdefg")                   error stop 10_4
         if(nest2%c /= "aabbccddeeffg")             error stop 11_4
         nest1%c = "xyz"
         nest2%c = "xxxyyyzzz" 
      else if(flag .eq. 2) then
         if(nest1%c /= "1234567")                   error stop 12_4
         if(nest2%c /= "1122334455667")             error stop 13_4
         nest1%c = "789"
         nest2%c = "777888999"
      end if 
    end subroutine

end module
