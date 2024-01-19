!*  ===================================================================
!*
!*  DATE                       : 1/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAX/MIN with named constant as selector
!*                               in associate construct
!*
!* ===================================================================

  program mxminArrayArgObj2

   type base
     character*3 bname(2,5)
   end type

   type, extends (base) :: child
     character*3 :: cname(2,5)
   end type

   class(child), allocatable :: z(:)

   character*3 x1(2,5), y1(2,5), x2(2,5), y2(2,5), x3(2,5), y3(2,5)

   parameter(x1 = 'aaa', y1 = 'bbb', x2 ="ccc", y2="ddd", x3="eee", &
       y3 = "fff")

   allocate(z(3), source=(/child(max(x1, y1), y2), child(x3, min(x2, y2)), &
          child(max(x3, y3, x1),y2)/))

   associate(As1 => max(z(1)%cname, z(2)%bname))

       if(any(As1 .ne. "eee") )then
         error stop 1_4
       end if

   end associate

   associate(As2 => min(z(1)%cname, z(2)%bname, z(3)%cname))

       if(any(As2 .ne. "ddd")) then
         error stop 2_4
        endif

   end associate

   deallocate(z)

  end program mxminArrayArgObj2

