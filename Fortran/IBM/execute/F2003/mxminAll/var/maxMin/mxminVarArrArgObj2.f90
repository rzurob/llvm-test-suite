!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 1/15/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics 
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*
!*  DESCRIPTION                : MAX/MIN with variable and used as selector 
!*                               in associate construct 
!*                                
!*
!* ===================================================================

  program mxminVarArrArgObj2 

   type base
     character*8 bname(2,5)
   end type

   type, extends (base) :: child
     character*8 :: cname(2,5)
   end type

   class(child), allocatable :: z(:)

   character*3 x1(2,5)
   character*4 y1(2,5)
   character*5 x2(2,5)
   character*6 y2(2,5)
   character*7 x3(2,5)
   character*8 y3(2,5)

   x1 = 'aaa'
   y1 = 'bbbb'
   x2 = "ccccc"
   y2 = "dddddd"
   x3 = "eeeeeee"
   y3 = "ffffffff"

   allocate(z(3), source=(/child(max(x1, y1), y2), child(x3, min(x2, y2)), &
          child(max(x3, y3, x1),y2)/))

   associate(As1 => max(z(1)%cname, z(2)%bname))

       if(len(As1) .ne. 8) error stop 1_4

       if(any(As1 .ne. "eeeeeee ")) error stop 2_4

   end associate

   associate(As2 => min(z(1)%cname, z(2)%bname, z(3)%cname))

       if(len(As2) .ne. 8) error stop 3_4

       if(any(As2 .ne. "dddddd  "))  error stop 4_4

   end associate

   deallocate(z)

  end program mxminVarArrArgObj2 

