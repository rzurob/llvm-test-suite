! GB DTP extension using:
! ftcx_dtp -qk -qdeferredlp -qreuse=base /tstdev/F2003/mxminAll/var/mxminVal/mxminvalVarArgObj2.f
! opt variations: -qck -qnok -qnodeferredlp -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 1/15/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX*/MIN* intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAXVAL/MINVAL with variable and used as
!*                               selector in associate construct.
!*
!* ===================================================================

  program mxminvalVarArgObj2

   type base(k1,n1)    ! (4,8)
     integer, kind :: k1
     integer, len  :: n1
     character(n1)    bname(2,5)
   end type

   type, extends (base) :: child    ! (4,8)
     character(n1) :: cname(2,5)
   end type

   class(child(4,:)), allocatable :: z(:)

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

   allocate(z(3), source=(/child(4,8)(max(maxval(x1), y1), y2), child(4,8)(minval(x3), min(x2, y2)), child(4,8)(max(x3, y3, x1),y2)/))

   associate(As1 => maxval(z(1)%cname))

       if(len(As1) .ne. 8) error stop 1_4

       if(As1 .ne. "dddddd  ") error stop 2_4

   end associate

   associate(As2 => minval(z(2)%bname, dim=2))

       if(len(As2) .ne. 8) error stop 3_4

       if(any(As2 .ne. "eeeeeee "))  error stop 4_4

   end associate

   associate(As3 => maxval(z(3)%cname, dim=1, mask=.true.))

       if(len(As3) .ne. 8) error stop 5_4

       if(any(shape(As3) .ne. 5)) error stop 6_4

       if(any(As3 .ne. "dddddd  "))  error stop 7_4

   end associate

   deallocate(z)

  end program mxminvalVarArgObj2

