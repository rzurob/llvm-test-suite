!*  ===================================================================
!*
!*  DATE                       : 2/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX*/MIN* intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAXLOC/MINLOC with variable as actual
!*                               argument to struct constructor.
!* ===================================================================

  program mxminlocVarArgObj2

   type base
     integer bname(2)
   end type

   type, extends (base) :: child
     integer :: cname(3)
   end type

   class(child), allocatable :: z(:)

   character*3 x(2,3)

   x = 'aaa'
   x(1,1) = "bbb"
   x(1,3) = "zzz"

   allocate(z(3), source=(/child(maxloc(x, dim=2), minloc(x, dim=1)), child(maxloc(x, dim=2), minloc(x, dim=1)), child(maxloc(x, dim=2), minloc(x, dim=1))/))

   associate(As1 => maxloc(z(1)%bname))

      if(any(As1 .ne. 1) )then
         error stop 1_4
       end if

   end associate

   associate(As2 => minloc(z(1)%cname))

       if(any(As2 .ne. 2) )then
         error stop 2_4
       end if

   end associate

  end program mxminlocVarArgObj2
