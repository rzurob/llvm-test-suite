!*  ===================================================================
!*
!*  DATE                       : 1/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAX/MIN with literal as selector
!*                               in associate construct
!*
!* ===================================================================

  program mxminLiteralArrObj2

   type base
     character*3 bname(2,2)
   end type

   type, extends (base) :: child
     character*3 :: cname(2,2)
   end type

   class(child), allocatable :: z(:)

   allocate(z(3), source=(/child(max(reshape((/"aaa","aaa","aaa","aaa"/), (/2,2/)), reshape((/"bbb","bbb","bbb","bbb"/), (/2,2/))), reshape((/"ddd","ddd","ddd","ddd"/), (/2,2/))), child(reshape((/"eee","eee","eee","eee"/), (/2,2/)), min(reshape((/"ccc","ccc","ccc","ccc"/), (/2,2/)), reshape((/"ddd","ddd","ddd","ddd"/), (/2,2/)))), child(max(reshape((/"eee","eee","eee","eee"/), (/2,2/)), reshape((/"fff","fff","fff","fff"/), (/2,2/)), reshape((/"aaa","aaa","aaa","aaa"/), (/2,2/))),reshape((/"ddd","ddd","ddd","ddd"/), (/2,2/)))/))

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

   associate(As3 => max(z(1)%cname(1,2), z(2)%bname(1,2)))

       if(As3 .ne. "eee")then
         error stop 3_4
       end if

   end associate

   associate(As4 => min(z(1)%cname(1,2)(1:2), z(2)%bname(1,2)(1:2), z(3)%cname(1,2)(1:2)))

       if(As4 .ne. "dd") then
         error stop 4_4
        endif

   end associate

   deallocate(z)

  end program mxminLiteralArrObj2

