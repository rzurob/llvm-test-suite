!*  ===================================================================
!*
!*  DATE                       : 1/15/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX*/MIN* intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAXVAL/MINVAL with named constant as selector
!*                               in associate construct
!*
!* ===================================================================

  program mxminvalObjOrient2

   type base
     character*3 bname(5)
   end type

   type, extends (base) :: child
     character*3 :: cname(2)
   end type

   class(child), allocatable :: z(:)

   character*3 x(2,5), y(2,5)

   parameter(x = 'aaa', y = 'bbb')

   allocate(z(3), source=(/child(maxval(x, dim=1), maxval(x, dim=2)), child(minval(y, dim=1), maxval(x, dim=2)), child(minval(x, dim=1), minval(y, dim=2))/))

   associate(As1 => maxval(x))

       if(As1 .ne. maxval(z(1)%bname))then
         error stop 1_4
       end if

   end associate

   associate(As2 => minval(y, dim=1))

       if(any(As2 .ne. z(2)%bname))then
         error stop 2_4
       end if

   end associate

   deallocate(z)

  end program mxminvalObjOrient2

