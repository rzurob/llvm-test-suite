! GB DTP extension using:
! ftcx_dtp -qck -ql -qreuse=base /tstdev/F2003/mxminAll/misc/mxminMisc16.f
! opt variations: -qnock -qnol -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 1/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAX*/MIN* as component in struct constructor
!*
!* ===================================================================

program mxminMisc16

     type base(n1,k1)    ! (20,4)
         integer, kind :: k1
         integer, len  :: n1
         integer(k1)      id
     end type

     type, extends(base) :: child(k2,n2)    ! (20,4,1,3)
          integer, kind             :: k2
          integer, len              :: n2
          character(kind=k2,len=n2) :: name
     end type

     type, extends(child) :: grandchild    ! (20,4,1,3)
          character(kind=k2,len=n2) :: gname(3)
     end type

     character*4 :: x(5)

     type (child(20,4,1,3)) :: c
     type (grandchild(20,4,1,3)) :: g

     type (base(20,4)) :: b

     x = "zzzz"

     x(2) = 'aaaa'

     b = base(20,4)(minloc(x, dim=1, mask=.true.))

     c = child(20,4,1,3)(base = b, name = max("ddd", "aaa"))

     g = grandchild(20,4,1,3)(child = c, gname = minval(x))

     if(b%id .ne. 2) then
        error stop 1_4
     endif

     if(c%name .ne. 'ddd')then
        error stop 2_4
     endif

     if(any(g%gname .ne. "aaa")) then
        error stop 3_4
     endif

     g = grandchild(20,4,1,3)(child = c, gname = maxval(pack(x, mask = .true.)))

     if(any(g%gname .ne. "zzz")) then
        error stop 4_4
     endif

end program mxminMisc16

